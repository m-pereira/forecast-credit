# global methods with tune
library(tidyverse)  # loading dplyr, tibble, ggplot2, .. dependencies
library(timetk)  # using timetk plotting, diagnostics and augment operations
library(tsibble)  # for month to Date conversion
library(tsibbledata)  # for aus_retail dataset
library(fastDummies)  # for dummyfying categorical variables
library(tidymodels)
library(modeltime)

my_tbl_uf <- 
  readRDS(here::here("data","cleaned.RDS"))%>% ungroup()  
my_tbl_uf <- 
  my_tbl_uf %>% filter(data_ref <= as.Date("2022-06-01"))

total_uf <- my_tbl_uf %>% pull(uf) %>% unique()
total_uf %>% length()

groups <- lapply(X = 1:length(total_uf), 
                 FUN = function(x) {
                   my_tbl_uf %>%
                     filter(uf == total_uf[x]) %>%
                     arrange(data_ref) %>%
                     mutate(credito = log1p(x = credito)) %>%
                     mutate(credito = standardize_vec(credito)) %>%
                     future_frame(data_ref, .length_out = "12 months",
                                  .bind_data = TRUE) %>%
                     mutate(uf = total_uf[x]) %>%
                     tk_augment_timeseries_signature(data_ref) %>%
                     tk_augment_fourier(.date_var = data_ref,
                                        .periods = 12, .K = 1) %>%
                     #adicionando lags ao modelo
                     tk_augment_lags(.value = credito, .lags = c(1:6)) %>%
                     tk_augment_slidify(
                       .value = credito_lag6,
                       .f = ~ mean(.x, na.rm = TRUE),
                       .period = c(1:6),
                       .partial = TRUE,
                       .align = "center"
                     )
                 })

groups_fe_tbl <- bind_rows(groups) %>%
  rowid_to_column(var = "rowid")
groups_fe_tbl %>% glimpse()
## salvando alguns parâmetros
tmp <- my_tbl_uf %>%
  group_by(uf) %>%
  arrange(data_ref) %>%
  mutate(credito = log1p(x = credito)) %>%
  group_map(~ c(
    mean = mean(.x$credito, na.rm = TRUE),
    sd = sd(.x$credito, na.rm = TRUE)
  )) %>%
  bind_rows()

std_mean <- tmp$mean
std_sd <- tmp$sd
rm('tmp')

## salvando os parâmetros 

## future table -------------------

groups_fe_tbl %>%
  tail(n = 13) %>%
  glimpse() 

#### splits --------
data_prepared_tbl <- groups_fe_tbl %>%
  filter(!is.na(credito)) %>%
  drop_na()

future_tbl <- groups_fe_tbl %>%
  filter(is.na(credito))
set.seed(34)
splits <- data_prepared_tbl %>%
  time_series_split(data_ref,
                    assess = paste0("14 months"),
                    cumulative = TRUE
  )
splits %>%
  tk_time_series_cv_plan() %>%
  select(.id) %>%
  table()
splits %>%
  tk_time_series_cv_plan() %>%
  select(.key) %>%
  table()
# validando os splits
splits %>%
  tk_time_series_cv_plan() %>%
  filter(uf == 53) %>%
  plot_time_series_cv_plan(
    .date_var = data_ref,
    .value = credito,
    .title = paste0("Split for ")
  )


##  recipe ----------------

recipe_spec <- recipe(credito ~ .,
                      data = training(splits)
) %>%
  update_role(rowid, new_role = "indicator") %>%
  step_rm(matches("day|hour|minute|second|hour12|am.pm|wday|wday.xts|mday|qday|yday|mweek|week|month.xts|year.iso|month")) %>% 
  step_dummy(all_nominal(), one_hot = FALSE)  %>%
  step_range(index.num, year, min = 0,max =1)

training(splits) %>% glimpse()

recipe_spec %>%
  prep() %>%
  summary()
juiced_ <-
  recipe_spec %>%
  prep() %>%
  juice()
juiced_ %>% glimpse()

recipe(credito ~ ., data = training(splits)) %>%
  step_timeseries_signature(data_ref) %>%
  prep() %>%
  juice() %>%
  skimr::skim()



artifacts <- list(
  # Data
  data = list(
    data_prepared_tbl = data_prepared_tbl,
    future_tbl = future_tbl,
    uf = total_uf
  ),
  
  # Recipes
  recipes = list(
    recipe_spec = recipe_spec
  ),
  
  # Splits
  splits = splits,
  
  # Inversion Parameters
  standardize = list(
    std_mean = std_mean,
    std_sd   = std_sd
  ),
  normalize = list(
    Month_index.num_limit_lower = min(juiced_$index.num),
    Month_index.num_limit_upper = max(juiced_$index.num),
    Month_year_limit_lower = min(juiced_$year),
    Month_year_limit_upper = max(juiced_$year)
  )
)

artifacts %>%
  write_rds(
    here::here(
      "artifacts",
      "filtered_feature_engineering_artifacts_list.rds"
    )
  )


#
# 02 MACHINE LEARNING ---------------------
library(modeltime) # ML models specifications and engines
library(tictoc) # measure training elapsed time
# 2.1- random forest ----------
tic()
wflw_fit_rf <- workflow() %>%
  add_model(
    spec = rand_forest(
      mode = "regression"
    ) %>%
      set_engine("ranger")
  ) %>%
  add_recipe(recipe_spec %>%
               step_rm(data_ref)) %>%
  fit(training(splits))
toc()


# 2.2- xgboost --------------
tic()
wflw_fit_xgboost <- workflow() %>%
  add_model(
    spec = boost_tree(
      mode = "regression"
    ) %>%
      set_engine("xgboost")
  ) %>%
  add_recipe(recipe_spec %>%
               step_rm(data_ref)) %>%
  fit(training(splits))
toc()


# 2.2- arima + xgboost --------------
tic()
wflw_fit_arima_boost <- workflow() %>%
  add_model(
    spec = arima_boost(
      mode = "regression"
    ) %>%
      set_engine("auto_arima_xgboost")
  ) %>%
  add_recipe(recipe_spec)  %>%
  fit(training(splits))
toc()


## modeltime table ------------------
## modeltime table
submodels_tbl <- modeltime_table(
  wflw_fit_rf,
  wflw_fit_xgboost,
  wflw_fit_arima_boost
)


submodels_tbl
submodels_tbl %>% write_rds(here::here("artifacts","filtered_submodels.RDS"))

calibrated_wflws_tbl <- submodels_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibrated_wflws_tbl
  

calibrated_wflws_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse, mae, mape)


workflow_artifacts <- list(
  workflows = list(
    wflw_random_forest = wflw_fit_rf,
    wflw_xgboost = wflw_fit_xgboost,
    wflw_fit_arima_boost = wflw_fit_arima_boost
  ),
  calibration = list(calibration_tbl = calibrated_wflws_tbl)
)

workflow_artifacts %>%
  write_rds(here::here("artifacts", "filtered_workflows_artifacts_list.rds"))


# 03 HYPERPARAMETER TUNING ------------------
library(tictoc)
library(future)
library(doFuture)
library(plotly)
wflw_artifacts <- read_rds(
  here::here("artifacts", "filtered_workflows_artifacts_list.rds"))
# cross validation

set.seed(2356)
resamples_kfold <- training(splits) %>%
  vfold_cv(v = 6)



resamples_kfold %>%
  tk_time_series_cv_plan() %>%
  filter(uf == 53) %>%
  plot_time_series_cv_plan(
    .date_var = data_ref,
    .value = credito,
    .facet_ncol = 2
  )

# parallel processing
registerDoFuture()
n_cores <- parallel::detectCores()

artifacts <- 
  read_rds(
    here::here(
      "artifacts",
      "filtered_feature_engineering_artifacts_list.rds"
    )
  )



# 3.1 RF -------------
### first tuning --------------------



model_spec_rf_tune <-  
  rand_forest(
    mode = "regression",
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) %>%
  set_engine("ranger")

wflw_spec_rf_tune <- workflow() %>%
  add_model(model_spec_rf_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_rf_tune
extract_parameter_set_dials(wflw_spec_rf_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(wflw_spec_rf_tune) %>%
    update(mtry = mtry(range = c(2, 20))),
  size = 20
)
grid_spec_1


## tune
tic()
tune_results_rf_1 <- wflw_spec_rf_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_1,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()

plan(strategy = sequential)

tune_results_rf_1 %>%
  show_best("rmse", n = Inf)


tune_results_rf_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_rf_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

#ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_rf_tune) %>%
    update(
      mtry = mtry(range = c(8, 16))
    ),
  size = 20
)
grid_spec_2


# 2. toggle on parallel processing
plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)
# 3. perform hyperparameter tuning with new grid specification
tic()
tune_results_rf_2 <- wflw_spec_rf_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_2,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()
# 4. toggle off parallel processing
plan(strategy = sequential)

# 5. analyze best RMSE and RSQ results (here top 2)

tune_results_rf_2 %>%
  show_best("rsq", n = 2)
tune_results_rf_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_rf_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

#gy(gr2)

### terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_rf_tune) %>%
    update(
      mtry = mtry(range = c(8, 12)),
      trees = trees(range = c(200,800)),
      min_n = min_n(range = c(10, 20))
    ),
  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_rf_3 <- wflw_spec_rf_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_3,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()

plan(strategy = sequential)


## select best models -----------------

# Fitting round 3 best RMSE model
set.seed(123)

wflw_fit_spec_rf_tune_rmse <- wflw_spec_rf_tune %>%
  finalize_workflow(
    select_best(tune_results_rf_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

modeltime_table(wflw_fit_spec_rf_tune_rmse) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_spec_rf_tune_rsq <- wflw_spec_rf_tune %>%
  finalize_workflow(
    select_best(tune_results_rf_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

modeltime_table(wflw_fit_spec_rf_tune_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_rf <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_rf_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_rf_1,
    round2 = tune_results_rf_2,
    round3 = tune_results_rf_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_spec_rf_tune_rmse,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_rf %>%
  write_rds(here::here("artifacts", "filtered_tuned_rf.rds"))



# 3.2 XGBOOST --------------
### first tuning --------------------
gc()
model_spec_xg_tune <- 
  boost_tree(
    mode = "regression",
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    stop_iter = tune()
  ) %>%
  set_engine("xgboost")

wflw_spec_xgboost_tune <- workflow() %>%
  add_model(model_spec_xg_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec %>%
               step_rm(data_ref))

wflw_spec_xgboost_tune
extract_parameter_set_dials(model_spec_xg_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_xg_tune) %>%
    update(
      mtry = mtry(range = c(4, 16))),
  size = 10 # pqno
)
grid_spec_1

## tune
tic()
tune_results_xg_boost_1 <- 
  wflw_spec_xgboost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_1,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()

plan(strategy = sequential)

tune_results_xg_boost_1 %>%
  show_best("rmse", n = Inf)


tune_results_xg_boost_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_xg_boost_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

#plotly::#ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_xg_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -0.5)),
      min_n = min_n(range = c(8, 20))
    ),
  size = 10
)
grid_spec_2


# 2. toggle on parallel processing
plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)
# 3. perform hyperparameter tuning with new grid specification
tic()
tune_results_xg_boost_2 <- wflw_spec_xgboost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_2,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()
# 4. toggle off parallel processing
plan(strategy = sequential)

# 5. analyze best RMSE and RSQ results (here top 2)

tune_results_xg_boost_2 %>%
  show_best("rsq", n = 2)
tune_results_xg_boost_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_xg_boost_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

#ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_xg_tune) %>%
    update(
      mtry = mtry(range = c(12, 14)),
      learn_rate = learn_rate(range = c(-1.0, -0.5)),
      min_n = min_n(range = c(12, 22)),
      trees =trees(range = c(300,800)),
      tree_depth=tree_depth(range = c(4,8))
    ),
  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_xg_boost_3 <- wflw_spec_xgboost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_3,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()

plan(strategy = sequential)

tune_results_xg_boost_3 %>%
  show_best("rsq", n = 2)

tune_results_xg_boost_3 %>%
  show_best("rmse", n = 2)


## select best models -----------------

set.seed(123)
wflw_fit_spec_xgboost_tune <- wflw_spec_xgboost_tune %>%
  finalize_workflow(
    select_best(tune_results_xg_boost_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_spec_xgboost_tune) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


set.seed(335)
wflw_fit_xgboost_tuned_rsq <- wflw_spec_xgboost_tune %>%
  finalize_workflow(
    select_best(tune_results_xg_boost_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_xgboost_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_xgb <- list(
  # Workflow spec
  tune_wkflw_spec = wflw_spec_xgboost_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_xg_boost_1,
    round2 = tune_results_xg_boost_2,
    round3 = tune_results_xg_boost_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_xgboost_tuned_rsq,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_xgb %>%
  write_rds(here::here("artifacts", "filtered_tuned_xgb.rds"))







# 3.3 arima  ---------------
### first tuning --------------------
gc()
spec_arima_tune <- arima_boost(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
 # sample_size = tune(),
  stop_iter = tune()
) %>%
  set_engine("auto_arima_xgboost")

wflw_spec_arima_boost_tune <- workflow() %>%
  add_model(spec_arima_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_arima_boost_tune
extract_parameter_set_dials(spec_arima_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(spec_arima_tune) %>%
    update(
      mtry = mtry(range = c(2, 40))),
  size = 10
)

grid_spec_1
## tune
tic()
tune_results_arima_boost_1 <- wflw_spec_arima_boost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_1,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()

plan(strategy = sequential)

tune_results_arima_boost_1 %>%
  show_best("rmse", n = Inf)


tune_results_arima_boost_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_arima_boost_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

#ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(spec_arima_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -2.0)),
      min_n = min_n(range = c(8, 20)),
      stop_iter = stop_iter(range = c(5,15)),
      trees= trees(range = c(400,80)),
      tree_depth = tree_depth(range(4,8))
    ),
  size = 10
)
grid_spec_2


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)
tic()
tune_results_arima_boost_2 <- wflw_spec_arima_boost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_2,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()
plan(strategy = sequential)


tune_results_arima_boost_2 %>%
  show_best("rsq", n = 2)
tune_results_arima_boost_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_arima_boost_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

#ggplotly(gr2)

### terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(spec_arima_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -2.0)),
      min_n = min_n(range = c(8, 20)),
      stop_iter = stop_iter(range = c(5,10)),
      trees = trees(range = c(400,80)),
      tree_depth = tree_depth(range = c(4,8)),
      loss_reduction = loss_reduction(range = c(-2,-7))
    ),
  size = 10
)

plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_arima_boost_3 <- wflw_spec_arima_boost_tune %>%
  tune_grid(
    resamples = resamples_kfold,
    grid = grid_spec_3,
    control = control_grid(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()

plan(strategy = sequential)

## select best models -----------------
# Fitting round 3 best RMSE model
set.seed(123)
wflw_fit_arima_boost_tuned <- wflw_spec_arima_boost_tune %>%
  finalize_workflow(
    select_best(tune_results_arima_boost_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_arima_boost_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_arima_boost_tuned_rsq <- wflw_spec_arima_boost_tune %>%
  finalize_workflow(
    select_best(tune_results_arima_boost_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_arima_boost_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_arima_xgb <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_arima_boost_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_arima_boost_1,
    round2 = tune_results_arima_boost_2,
    round3 = tune_results_arima_boost_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_arima_boost_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_arima_xgb %>%
  write_rds(here::here("artifacts", "filtered_tuned_arima_xgb.rds"))


tuned_arima_xgb <- 
  read_rds(here::here("artifacts", "filtered_tuned_arima_xgb.rds"))
tuned_xgb <- 
  read_rds(here::here("artifacts", "filtered_tuned_xgb.rds"))
tuned_rf <- 
  read_rds(here::here("artifacts", "filtered_tuned_rf.rds"))

submodels_tbl <- read_rds(here::here("artifacts","filtered_submodels.RDS"))

# 04  CALIBRATION -------------
submodels_tbl # modelos originais

submodels_all_tbl <- modeltime_table(
  tuned_arima_xgb$tune_wflw_fit,
  tuned_rf$tune_wflw_fit,
  tuned_xgb$tune_wflw_fit
)  %>%
  update_model_description(1, "ARIMA W/ XGBOOST ERRORS - Tuned") %>%
  update_model_description(2, "RANGER - Tuned") %>%
  update_model_description(3, "XGBOOST ERRORS - Tuned") %>%  
  combine_modeltime_tables(submodels_tbl)

submodels_all_tbl %>% 
  write_rds(here::here("artifacts","filtered_submodels_all_tbl.RDS"))

submodels_all_tbl

calibration_all_tbl <- submodels_all_tbl %>%
  modeltime_calibrate(testing(splits))

calibration_all_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

calibration_all_tbl %>%
  modeltime_accuracy() %>%
  arrange(desc(rsq))

calibration_all_tbl %>%
  modeltime_accuracy() %>%
  arrange(desc(rsq))

calibration_all_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy()



calibration_all_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = artifacts$data$data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  filter(uf == mun_teste) %>%
  plot_modeltime_forecast(
    # .facet_ncol         = 4,
    .conf_interval_show = FALSE,
    .interactive = TRUE,
    .title = mun_teste
  )


workflow_all_artifacts <- list(
  workflows = submodels_all_tbl,
  calibration = calibration_all_tbl
)

workflow_all_artifacts %>%
  write_rds(here::here("artifacts",
                       "filtered_workflows_NonandTuned_artifacts_list.rds"))

submodels_all_tbl %>%
  write_rds(here::here("artifacts",
                       "filtered_submodels_all_tbl.rds"))

submodels_all_tbl <- 
  read_rds(here::here("artifacts","filtered_submodels_all_tbl.RDS"))

workflow_all_artifacts <- 
  read_rds(here::here("artifacts",
                      "filtered_workflows_NonandTuned_artifacts_list.rds"))
artifacts <- 
  read_rds(
    here::here(
      "artifacts",
      "filtered_feature_engineering_artifacts_list.rds"
    )
  )

# 05 ENSEMBLES ------------------------
library(modeltime.ensemble)
calibration_tbl <- read_rds(here::here("artifacts","filtered_workflows_NonandTuned_artifacts_list.rds"))
calibration_tbl <- calibration_tbl$calibration
calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

ensemble_fit_mean <- submodels_all_tbl %>%
  ensemble_average(type = "mean")

ensemble_fit_median <- submodels_all_tbl %>%
  ensemble_average(type = "median")

## Weighted ensembles por rank ---------
calibration_tbl %>%
  modeltime_accuracy() %>%
  mutate(rank = min_rank(-rmse))

loadings_tbl <- submodels_all_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  mutate(rank = min_rank(-rmse)) %>%
  select(.model_id, rank)

ensemble_fit_wt <- submodels_all_tbl %>%
  ensemble_weighted(loadings = loadings_tbl$rank)

ensemble_fit_wt
ensemble_fit_wt$fit$loadings_tbl



## evaluating


modeltime_table(
  ensemble_fit_mean,
  ensemble_fit_median,
  ensemble_fit_wt
) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)

calibration_all_tbl <- modeltime_table(
  ensemble_fit_mean,
  ensemble_fit_median,
  ensemble_fit_wt
) %>%
  combine_modeltime_tables(calibration_tbl)  %>%   
  modeltime_calibrate(testing(splits)) 

calibration_all_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)

#### rolou algum overffiting ne fiote
calibration_all_tbl %>%
  modeltime_accuracy(training(splits)) %>%
  arrange(rmse)



calibration_all_tbl %>% write_rds(
  here::here("artifacts","filtered_calibration_ensemble_all_tbl.RDS"))

calibration_all_tbl <- read_rds(
  here::here("artifacts","filtered_calibration_ensemble_all_tbl.RDS"))

## forecast ----------------------
plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
set.seed(15)
refit_stacking_tbl <- calibration_all_tbl %>%
  modeltime_refit(
    data = artifacts$data$data_prepared_tbl,
    resamples = artifacts$data$data_prepared_tbl %>%
      drop_na() %>%
      vfold_cv(v = 5)
  )
toc()

artifacts$data$data_prepared_tbl |> glimpse()
artifacts$data$data_prepared_tbl |> names()
artifacts$data$future_tbl |> names()
artifacts$data$future_tbl |> glimpse()



forecast_stacking_tbl <- refit_stacking_tbl %>%
  modeltime_forecast(
    new_data = artifacts$data$future_tbl,
    actual_data = artifacts$data$data_prepared_tbl %>%
      drop_na(),
    keep_data = TRUE
  )
forecast_stacking_tbl
plan(sequential)

forecast_stacking_tbl |> names()

lforecasts <- lapply(X = 1:length(total_uf), FUN = function(x) {
  forecast_stacking_tbl %>%
    filter(uf == total_uf[x]) %>%
    group_by(uf) %>%
    mutate(across(.value:.value,
                  .fns = ~ standardize_inv_vec(
                    x = .,
                    mean = artifacts$standardize$std_mean[x],
                    sd = artifacts$standardize$std_sd[x]
                  )
    )) %>%
    mutate(across(.value:.value,
                  .fns = ~ expm1(x = .)
    ))
})

forecast_stacking_tbl <- bind_rows(lforecasts)

forecast_stacking_tbl %>%
  group_by(uf) %>%
  plot_modeltime_forecast(
    .title = "Turnover 1-year forecast",
    .facet_ncol = 4,
    .conf_interval_show = FALSE,
    .interactive = TRUE
  )

forecast_stacking_tbl |> 
  write_rds(here::here(
    "artifacts",
    "filtered_credito_forecast_ml.RDS"))

forecast_stacking_tbl %>% 
  filter(.model_desc %in% c("ACTUAL","ENSEMBLE (MEAN): 6 MODELS")) %>%
  group_by(.index) %>% 
  summarise(.value = sum(.value)) %>% 
  plot_time_series(.date_var = .index,
                   .value = .value)

