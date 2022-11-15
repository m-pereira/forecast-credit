# global methods with tune

library(tidyverse)
library(timetk)
library(tidymodels)
my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS"))  



groups <- lapply(X = 1:length(total_mun), 
                 FUN = function(x) {
                   credito %>%
                     filter(uf == total_mun[x]) %>%
                     arrange(data_ref) %>%
                     mutate(credito = log1p(x = credito)) %>%
                     mutate(credito = standardize_vec(credito)) %>%
                     future_frame(data_ref, .length_out = "12 months", .bind_data = TRUE) %>%
                     mutate(uf = total_mun[x]) %>%
                     step_timeseries_signature(data_ref) %>%
                     step_rm(data_ref) %>%
                     step_zv(all_predictors()) %>%
                     step_dummy(all_nominal_predictors(), 
                                one_hot = TRUE) %>%
                     step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
                     #    step_rm(matches("lbl")) %>%
                     # adicionando serie de fourier aos dados
                     tk_augment_fourier(.date_var = data_ref, .periods = 12, .K = 1) %>%
                     # adicionando lags ao modelo
                     tk_augment_lags(.value = credito, .lags = c(12)) %>%
                     tk_augment_slidify(
                       .value = credito_lag12,
                       .f = ~ mean(.x, na.rm = TRUE),
                       .period = c(12),
                       .partial = TRUE,
                       .align = "center"
                     ) 
                 })

groups_fe_tbl <- bind_rows(groups) %>%
  rowid_to_column(var = "rowid")
#groups |> View()

## future table -------------------

groups_fe_tbl %>%
  tail(n = 13) %>%
  glimpse() 

groups_fe_tbl %>%
  tail(n = 13) %>%
  glimpse() %>% View()

## salvando alguns parâmetros
tmp <- credito %>%
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
# rm('tmp')

#### splits --------
data_prepared_tbl <- groups_fe_tbl %>%
  filter(!is.na(credito)) %>%
  drop_na()

future_tbl <- groups_fe_tbl %>%
  filter(is.na(credito))
set.seed(34)
splits <- data_prepared_tbl %>%
  time_series_split(data_ref,
                    assess = paste0("12 months"),
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
  filter(uf == mun_teste) %>%
  plot_time_series_cv_plan(
    .date_var = data_ref,
    .value = credito,
    .title = paste0("Split for ", mun_teste)
  )


##  recipe ----------------

recipe_spec <- recipe(credito ~ .,
                      data = training(splits)
) %>%
  update_role(rowid, new_role = "indicator") %>%
  step_other(uf) %>%
  # step para adicionar dados de series temporais
  #step_timeseries_signature(data_ref) %>%
  #step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
  #step_rm(matches("(lbl)")) %>%
  step_dummy(all_nominal(), one_hot = FALSE) #%>%
#step_normalize(data_ref_index.num, data_ref_year)

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



feature_engineering_artifacts_list <- list(
  # Data
  data = list(
    data_prepared_tbl = data_prepared_tbl,
    future_tbl = future_tbl,
    uf = total_mun
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
    Month_index.num_limit_lower = min(juiced_$data_ref_index.num),
    Month_index.num_limit_upper = max(juiced_$data_ref_index.num),
    Month_year_limit_lower = min(juiced_$data_ref_year),
    Month_year_limit_upper = max(juiced_$data_ref_year)
  )
)

feature_engineering_artifacts_list %>%
  write_rds(
    here::here(
      "data", "artifacts","credito",
      "feature_engineering_artifacts_list.rds"
    )
  )


#
# 02 MACHINE LEARNING ---------------------
library(modeltime) # ML models specifications and engines
library(tictoc) # measure training elapsed time


artifacts <- read_rds(here::here(
  "data", "artifacts","credito",
  "feature_engineering_artifacts_list.rds"
))

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


# 2.3- prophet --------------

tic()
wflw_fit_prophet <- workflow() %>%
  add_model(
    spec = prophet_reg(
      seasonality_daily  = FALSE,
      seasonality_weekly = FALSE,
      seasonality_yearly = TRUE
    ) %>%
      set_engine("prophet")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()

# 2.4- PROPHET + XGBOOST ----
tic()
wflw_fit_prophet_boost <- workflow() %>%
  add_model(
    spec = prophet_boost(
      seasonality_daily  = FALSE,
      seasonality_weekly = FALSE,
      seasonality_yearly = FALSE
    ) %>%
      set_engine("prophet_xgboost")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()


# 2.5 - hierarquical (thief)-----------
tic()
wkflw_fit_thief <- workflow() %>%
  add_model(temporal_hierarchy(
    seasonal_period = "12 months",
    combination_method = "mse",
    use_model = "arima"
  ) %>%
    set_engine("thief")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()

# 2.6- arima ------------------
tic()
wflw_fit_arima <- workflow() %>%
  add_model(
    arima_reg(
      mode = "regression",
      seasonal_period = NULL,
      non_seasonal_ar = NULL,
      non_seasonal_differences = NULL,
      non_seasonal_ma = NULL,
      seasonal_ar = NULL,
      seasonal_differences = NULL,
      seasonal_ma = NULL
    ) %>%
      set_engine("auto_arima")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits)
  ) 
toc()
# 2.7- nnenetar -------------
tic()
wflw_fit_nnetar <-
  workflow() %>%
  add_model(
    nnetar_reg("regression",
               seasonal_period = NULL,
               non_seasonal_ar = NULL,
               seasonal_ar = NULL,
               hidden_units = NULL,
               num_networks = NULL,
               penalty = NULL,
               epochs = NULL
    ) %>%
      set_engine("nnetar")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()

# 2.8- ets --------------------
tic()
wflw_fit_ets <-
  workflow() %>%
  add_model(
    exp_smoothing(
      seasonal_period = NULL,
      error = NULL,
      trend = NULL,
      season = NULL,
      damping = NULL,
      smooth_level = NULL,
      smooth_trend = NULL,
      smooth_seasonal = NULL
    ) %>%
      set_engine("ets")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()

# 2.9- moving average -----------------
tic()
wflw_fit_ma12 <-
  workflow() %>%
  add_model(
    window_reg(
      mode = "regression",
      window_size = 12
    ) %>%
      set_engine(
        engine = "window_function")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()

#devtools::install_github("AlbertoAlmuinha/garchmodels")
teste <- get_model_env()
teste$models

# 2.10 GARCH ------------------------

# 2.11.1 bayesian models -------------------------
# bayesiano
# https://albertoalmuinha.github.io/bayesmodels/
# library(bayesmodels)
# ## aditive state space model
# tic()
# wkflw_fit_additive_state_space <-
#   workflow() %>%
#   add_model(
#     bayesmodels::additive_state_space(
#       mode = "regression",
#       trend_model = NULL,
#       damped_model = NULL,
#       seasonal_model = NULL,
#       seasonal_period = NULL,
#       garch_t_student = NULL,
#       markov_chains = NULL,
#       chain_iter = NULL,
#       warmup_iter = NULL,
#       adapt_delta = NULL,
#       tree_depth = NULL,
#       pred_seed = NULL
#     )
#   ) %>%
#   set_engine(engine = "stan") %>%
#   add_recipe(recipe_spec) %>%
#   fit(training(splits))
# toc()
# 
# ## adaptive spline
# tic()
# wklw_fit_adaptive_spline <-
#   workflow() %>%
#   add_model(
#     bayesmodels::adaptive_spline(
#       mode = "regression",
#       splines_degree = NULL,
#       max_degree = NULL,
#       max_categorical_degree = NULL,
#       min_basis_points = NULL
#     )
#   ) %>%
#   set_engine(engine = "stan") %>%
#   add_recipe(recipe_spec) %>%
#   fit(training(splits))
# toc()
# 
# # General Interface for Stochastic Volatility Regression Models
# tic()
# wklw_fit_stochatic_vol_reg <-
#   workflow() %>%
#   add_model(
#     bayesmodels::svm_reg(
#       mode = "regression",
#       non_seasonal_ar = NULL,
#       non_seasonal_ma = NULL,
#       markov_chains = NULL,
#       chain_iter = NULL,
#       warmup_iter = NULL,
#       adapt_delta = NULL,
#       tree_depth = NULL,
#       pred_seed = NULL
#     )
#   ) %>%
#   set_engine(engine = "stan") %>%
#   add_recipe(recipe_spec) %>%
#   fit(training(splits))
# toc()

# garch
#https://albertoalmuinha.github.io/garchmodels/

## modeltime table ------------------
## modeltime table
submodels_tbl <- modeltime_table(
  wflw_fit_rf,
  wflw_fit_xgboost,
  wflw_fit_prophet,
  wflw_fit_prophet_boost,
  wkflw_fit_thief,
  wflw_fit_arima,
  wflw_fit_nnetar,
  wflw_fit_ets,
  wflw_fit_ma12
  #  wkflw_fit_additive_state_space,
  # wklw_fit_adaptive_spline,
  #  wklw_fit_stochatic_vol_reg,
  #  wflw_fit_garch
)


submodels_tbl


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
    wflw_prophet = wflw_fit_prophet,
    wflw_prophet_boost = wflw_fit_prophet_boost
  ),
  calibration = list(calibration_tbl = calibrated_wflws_tbl)
)

workflow_artifacts %>%
  write_rds(here::here("data", "artifacts","credito", "workflows_artifacts_list.rds"))


# 03 HYPERPARAMETER TUNING ------------------

library(future)
library(doFuture)
library(plotly)
wflw_artifacts <- read_rds(
  here::here("data", "artifacts","credito", "workflows_artifacts_list.rds"))

wflw_artifacts$calibration$calibration_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)

# cross validation

set.seed(2356)
resamples_kfold <- training(splits) %>%
  vfold_cv(v = 6)



resamples_kfold %>%
  tk_time_series_cv_plan() %>%
  filter(uf == mun_teste) %>%
  plot_time_series_cv_plan(
    .date_var = data_ref,
    .value = credito,
    .facet_ncol = 2
  )

# parallel processing
registerDoFuture()
n_cores <- parallel::detectCores()


# 3.1 PROPHET REGRESSION ---------------
### first tuning --------------------
model_spec_prophet_boost_tune <- prophet_boost(
  mode = "regression",
  # growth = NULL,
  changepoint_num = tune(),
  # changepoint_range = NULL,
  seasonality_yearly = FALSE,
  seasonality_weekly = FALSE,
  seasonality_daily = FALSE,
  # season = NULL,
  # prior_scale_changepoints = NULL,
  # prior_scale_seasonality = NULL,
  # prior_scale_holidays = NULL,
  # logistic_cap = NULL,
  # logistic_floor = NULL,
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  # sample_size = NULL,
  # stop_iter = NULL
) %>%
  set_engine("prophet_xgboost")

wflw_spec_prophet_boost_tune <- workflow() %>%
  add_model(model_spec_prophet_boost_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_prophet_boost_tune
extract_parameter_set_dials(model_spec_prophet_boost_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_boost_tune) %>%
    update(mtry = mtry(range = c(2, 40))),
  size = 20
)
grid_spec_1



## tune
tic()
tune_results_prophet_boost_1 <- wflw_spec_prophet_boost_tune %>%
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

tune_results_prophet_boost_1 %>%
  show_best("rmse", n = Inf)


tune_results_prophet_boost_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_prophet_boost_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_boost_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -3.0)),
      min_n = min_n(range = c(8, 20))
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
tune_results_prophet_boost_2 <- wflw_spec_prophet_boost_tune %>%
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

tune_results_prophet_boost_2 %>%
  show_best("rsq", n = 2)
tune_results_prophet_boost_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_prophet_boost_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_boost_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -3.0)),
      min_n = min_n(range = c(8, 20))
    ),
  trees = trees(range = c(950, 1700)),
  loss_reduction = loss_reduction(range = c(-9, -6)),
  size = 20
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_prophet_boost_3 <- wflw_spec_prophet_boost_tune %>%
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
wflw_fit_prophet_boost_tuned <- wflw_spec_prophet_boost_tune %>%
  finalize_workflow(
    select_best(tune_results_prophet_boost_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_prophet_boost_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_prophet_boost_tuned_rsq <- wflw_spec_prophet_boost_tune %>%
  finalize_workflow(
    select_best(tune_results_prophet_boost_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_prophet_boost_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_prophet_xgb <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_prophet_boost_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_prophet_boost_1,
    round2 = tune_results_prophet_boost_2,
    round3 = tune_results_prophet_boost_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_prophet_boost_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_prophet_xgb %>%
  write_rds(here::here("data", "artifacts", "tuned_prophet_xgb.rds"))


# 3.2 RF -------------
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


tune_results_prophet_boost_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_rf_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_rf_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      min_n = min_n(range = c(20, 40)),
      trees = trees(range = c(800,1200))
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

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_rf_tune) %>%
    update(
      mtry = mtry(range = c(8, 12)),
      trees = trees(range = c(900,1200)),
      min_n = min_n(range = c(15, 20))
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

### acurácia cai bastante
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

# modelo piora MUITO
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
  write_rds(here::here("data", "artifacts", "tuned_rf.rds"))



# 3.3 XGBOOST --------------
### first tuning --------------------

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
show_notes(.Last.tune.result)

plan(strategy = sequential)

tune_results_xg_boost_1 %>%
  show_best("rmse", n = Inf)


tune_results_xg_boost_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_xg_boost_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

plotly::ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_xg_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -3.0)),
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

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_xg_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -3.0)),
      min_n = min_n(range = c(8, 20))
    ),
  trees = trees(range = c(950, 1700)),
  loss_reduction = loss_reduction(range = c(-9, -6)),
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

# Fitting round 3 best RMSE model
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


# Fitting round 3 best RSQmodel

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
  write_rds(here::here("data", "artifacts", "tuned_xgb.rds"))


#3.4 Prophet ---------------
### first tuning --------------------
model_spec_prophet_tune <- prophet_reg(
  mode = "regression",
  growth = tune(),#"linear", "logistic"
  changepoint_num = tune(),
  changepoint_range = tune(),
  seasonality_yearly = tune(),# TRUE, FALSE
  prior_scale_changepoints = tune(),
  prior_scale_seasonality = tune()
) %>%
  set_engine("prophet")

wflw_spec_prophet_tune <- workflow() %>%
  add_model(model_spec_prophet_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_prophet_tune
extract_parameter_set_dials(model_spec_prophet_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_tune),
  size = 10
)
grid_spec_1



## tune
tic()
tune_results_prophet_1 <- wflw_spec_prophet_tune %>%
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

tune_results_prophet_1 %>%
  show_best("rmse", n = Inf)


tune_results_prophet_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_prophet_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_tune) %>%
    update(
      growth = growth(values = "linear"),#"linear", "logistic"
      changepoint_num = changepoint_num(range = c(25,50)),
      changepoint_range = changepoint_range(range = c(0.6, 0.9)),
      seasonality_yearly = seasonality_yearly(values = TRUE),# TRUE, FALSE
      prior_scale_changepoints = prior_scale_changepoints(range = c(-3, 2), 
                                                          trans = log10_trans()),
      prior_scale_seasonality = prior_scale_seasonality(range = c(-3, 2), 
                                                        trans = log10_trans())
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
tune_results_prophet_2 <- wflw_spec_prophet_tune %>%
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

tune_results_prophet_2 %>%
  show_best("rsq", n = 2)
tune_results_prophet_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_prophet_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_prophet_tune) %>%
    update(
      growth = growth(values = "linear"),#"linear", "logistic"
      changepoint_num = changepoint_num(range = c(25,50)),
      changepoint_range = changepoint_range(range = c(0.6, 0.9)),
      seasonality_yearly = seasonality_yearly(values = TRUE),# TRUE, FALSE
      prior_scale_changepoints = prior_scale_changepoints(range = c(-3, 2), 
                                                          trans = log10_trans()),
      prior_scale_seasonality = prior_scale_seasonality(range = c(-3, 2), 
                                                        trans = log10_trans())
    ),
  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_prophet_3 <- wflw_spec_prophet_tune %>%
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
wflw_fit_prophet_tuned <- wflw_spec_prophet_tune %>%
  finalize_workflow(
    select_best(tune_results_prophet_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_prophet_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_prophet_tuned_rsq <- wflw_spec_prophet_tune %>%
  finalize_workflow(
    select_best(tune_results_prophet_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_prophet_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_prophet <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_prophet_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_prophet_1,
    round2 = tune_results_prophet_2,
    round3 = tune_results_prophet_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_prophet_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_prophet %>%
  write_rds(here::here("data", "artifacts", "tuned_prophet.rds"))


# 3.5 Thief--------------
### first tuning --------------------
model_spec_thief_tune <- temporal_hierarchy(
  seasonal_period = "12 months",
  combination_method = tune(),
  use_model = tune()
) %>%
  set_engine("thief")

wflw_spec_thief_tune <- workflow() %>%
  add_model(model_spec_thief_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_thief_tune
extract_parameter_set_dials(wflw_spec_thief_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_thief_tune),
  size = 20
)
grid_spec_1



## tune
tic()
tune_results_thief_1 <- wflw_spec_thief_tune %>%
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

tune_results_thief_1 %>%
  show_best("rmse", n = Inf)


tune_results_thief_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_thief_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)

## select best models -----------------

# Fitting round 3 best RMSE model
set.seed(123)
wflw_fit_thief_tuned <- wflw_spec_thief_tune %>%
  finalize_workflow(
    select_best(tune_results_thief_1, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_thief_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()
#  select

set.seed(335)
wflw_fit_thief_tuned_rsq <- wflw_fit_thief_tuned %>%
  finalize_workflow(
    select_best(tune_results_thief_1, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_thief_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_thief <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_thief_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_thief_1
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_thief_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_thief %>%
  write_rds(here::here("data", "artifacts", "tuned_thief.rds"))



# 3.6 ARIMA --------------------
### first tuning --------------------
model_spec_arima_tune <- 
  arima_reg(
    mode = "regression",
    seasonal_period = "yearly",
    non_seasonal_ar = tune(),
    non_seasonal_differences = tune(),
    non_seasonal_ma = tune(),
    seasonal_ar = tune(),
    seasonal_differences = tune(),
    seasonal_ma = tune()
  ) %>%
  set_engine("auto_arima")

wflw_spec_arima_tune <- workflow() %>%
  add_model(model_spec_arima_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_arima_tune
extract_parameter_set_dials(model_spec_arima_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_arima_tune),
  size = 10
)
grid_spec_1



## tune
tic()
tune_results_arima_1 <- wflw_spec_arima_tune %>%
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

tune_results_arima_1 %>%
  show_best("rmse", n = Inf)


tune_results_arima_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_arima_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_arima_tune) %>%
    update(
      non_seasonal_ar = non_seasonal_ar(range = c(0,5)),
      non_seasonal_differences = non_seasonal_differences(range = c(1,2)),
      non_seasonal_ma = non_seasonal_ma(range = c(0,1)),
      #      seasonal_ar = seasonal_ar(),
      #      seasonal_differences = seasonal_differences(),
      #      seasonal_ma = seasonal_ma()
      
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
tune_results_arima_2 <- wflw_spec_arima_tune %>%
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

tune_results_arima_2 %>%
  show_best("rsq", n = 2)
tune_results_arima_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_arima_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_arima_tune) %>%
    update(
      non_seasonal_ar = non_seasonal_ar(range = c(0,5)),
      non_seasonal_differences = non_seasonal_differences(range = c(1,2)),
      non_seasonal_ma = non_seasonal_ma(range = c(0,1)),
      seasonal_ar = seasonal_ar(range = c(1,2)),
      #      seasonal_differences = seasonal_differences(range = c(0,1)),
      seasonal_ma = seasonal_ma(range = c(1,2))
      
    ),
  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_arima_3 <- wflw_spec_arima_tune %>%
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

tune_results_arima_3 %>%
  show_best("rsq", n = 2)
tune_results_arima_3 %>%
  show_best("rmse", n = 2)

## select best models -----------------

# Fitting round 3 best RMSE model
set.seed(123)
wflw_fit_arima_tuned <- wflw_spec_arima_tune %>%
  finalize_workflow(
    select_best(tune_results_arima_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_arima_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_arima_tuned_rsq <- wflw_spec_arima_tune %>%
  finalize_workflow(
    select_best(tune_results_arima_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_arima_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_arima <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_arima_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_arima_1,
    round2 = tune_results_arima_2,
    round3 = tune_results_arima_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_arima_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_arima %>%
  write_rds(here::here("data", "artifacts", "tuned_arima.rds"))

# 3.7 NNETAR --------------------
### first tuning --------------------
model_spec_nnetar_tune <- 
  nnetar_reg("regression",
             seasonal_period = "yearly",
             non_seasonal_ar = tune(),
             seasonal_ar = tune(),
             hidden_units = tune(),
             num_networks = tune(),
             penalty = tune(),
             epochs = tune()
  ) %>%
  set_engine("nnetar")

wflw_spec_nnetar_tune <- workflow() %>%
  add_model(model_spec_nnetar_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_nnetar_tune
extract_parameter_set_dials(model_spec_nnetar_tune)

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_nnetar_tune),
  size = 10
)
grid_spec_1



## tune
tic()
tune_results_nnetar_1 <- wflw_spec_nnetar_tune %>%
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

tune_results_nnetar_1 %>%
  show_best("rmse", n = Inf)


tune_results_nnetar_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_nnetar_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_nnetar_tune) %>%
    update(
      non_seasonal_ar = non_seasonal_ar(range = c(3,5)),
      seasonal_ar = seasonal_ar(range = c(1,2)),
      hidden_units = hidden_units(range = c(5,8)),
      #      num_networks = tune(),
      #      penalty = tune(),
      #      epochs = tune()
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
tune_results_nnetar_2 <- wflw_spec_nnetar_tune %>%
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

tune_results_nnetar_2 %>%
  show_best("rsq", n = 2)
tune_results_nnetar_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_nnetar_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_nnetar_tune) %>%
    update(
      non_seasonal_ar = non_seasonal_ar(range = c(3,5)),
      seasonal_ar = seasonal_ar(range = c(1,2)),
      hidden_units = hidden_units(range = c(5,8)),
      #      num_networks = tune(),
      #      penalty = tune(),
      #      epochs = tune()
    ),  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_nnetar_3 <- wflw_spec_nnetar_tune %>%
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
wflw_fit_nnetar_tuned <- wflw_spec_nnetar_tune %>%
  finalize_workflow(
    select_best(tune_results_nnetar_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_nnetar_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_nnetar_tuned_rsq <- wflw_spec_nnetar_tune %>%
  finalize_workflow(
    select_best(tune_results_nnetar_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_nnetar_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_nnetar <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_nnetar_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_nnetar_1,
    round2 = tune_results_nnetar_2,
    round3 = tune_results_nnetar_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_nnetar_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_nnetar %>%
  write_rds(here::here("data", "artifacts", "tuned_nnetar.rds"))


# 3.8 ETS --------------------
### first tuning --------------------
model_spec_ets_tune <- 
  exp_smoothing(
    #    seasonal_period = "additive",
    #    error = "additive",
    #    trend = tune(),
    #    season = "additive",
    #    damping = tune(),
    smooth_level = tune(),
    smooth_trend = tune(),
    smooth_seasonal = tune()
  ) %>%
  set_engine("ets")

wflw_spec_ets_tune <- workflow() %>%
  add_model(model_spec_ets_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_ets_tune
extract_parameter_set_dials(model_spec_ets_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_ets_tune),
  size = 10
)
grid_spec_1



## tune
tic()
tune_results_ets_1 <- wflw_spec_ets_tune %>%
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
show_notes(.Last.tune.result)

tune_results_ets_1 %>%
  show_best("rmse", n = Inf)


tune_results_ets_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_ets_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_ets_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -3.0)),
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
tune_results_ets_2 <- wflw_spec_ets_tune %>%
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

tune_results_ets_2 %>%
  show_best("rsq", n = 2)
tune_results_ets_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_ets_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_ets_tune) %>%
    update(
      mtry = mtry(range = c(8, 16)),
      learn_rate = learn_rate(range = c(-1.0, -3.0)),
      min_n = min_n(range = c(8, 20))
    ),
  trees = trees(range = c(950, 1700)),
  loss_reduction = loss_reduction(range = c(-9, -6)),
  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_ets_3 <- wflw_spec_ets_tune %>%
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
wflw_fit_ets_tuned <- wflw_spec_ets_tune %>%
  finalize_workflow(
    select_best(tune_results_ets_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_ets_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_ets_tuned_rsq <- wflw_spec_ets_tune %>%
  finalize_workflow(
    select_best(tune_results_ets_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_ets_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()



tuned_ets <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_ets_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_ets_1,
    round2 = tune_results_ets_2,
    round3 = tune_results_ets_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_ets_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_ets %>%
  write_rds(here::here("data", "artifacts", "tuned_ets.rds"))


# 3.9 MA20 ---------------------------
### first tuning --------------------
model_spec_ma_tune <- 
  window_reg(
    mode = "regression",
    window_size = tune()
  ) %>%
  set_engine(
    engine = "window_function")

wflw_spec_ma_tune <- workflow() %>%
  add_model(model_spec_ma_tune) %>%
  add_recipe(artifacts$recipes$recipe_spec)

wflw_spec_ma_tune
extract_parameter_set_dials(model_spec_ma_tune)

artifacts$recipes$recipe_spec %>%
  update_role(data_ref, new_role = "indicator") %>%
  prep() %>%
  summary() %>%
  group_by(role) %>%
  summarise(n = n())

## grid
set.seed(235)
grid_spec_1 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_ma_tune),
  size = 10
)
grid_spec_1



## tune
tic()
tune_results_ma_1 <- wflw_spec_ma_tune %>%
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

tune_results_ma_1 %>%
  show_best("rmse", n = Inf)


tune_results_ma_1 %>%
  show_best("rsq", n = Inf)

gr1 <- tune_results_ma_1 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr1)



### second -------------------------
grid_spec_2 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_ma_tune) %>%
    update(
      window_size = window_size(range = c(10,20))
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
tune_results_ma_2 <- wflw_spec_ma_tune %>%
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

tune_results_ma_2 %>%
  show_best("rsq", n = 2)
tune_results_ma_2 %>%
  show_best("rmse", n = 2)


# analyze results

gr2 <- tune_results_ma_2 %>%
  autoplot() +
  geom_smooth(se = FALSE)

ggplotly(gr2)

## terceiro runing ---------------

set.seed(123)
grid_spec_3 <- grid_latin_hypercube(
  extract_parameter_set_dials(model_spec_ma_tune) %>%
    update(
      window_size = window_size(range = c(20,30))
      
    ),
  size = 10
)


plan(
  strategy = cluster,
  workers  = parallel::makeCluster(n_cores)
)

tic()
tune_results_ma_3 <- wflw_spec_ma_tune %>%
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
wflw_fit_ma_tuned <- wflw_spec_ma_tune %>%
  finalize_workflow(
    select_best(tune_results_ma_3, "rmse", n = 1)
  ) %>%
  fit(training(splits))

### acurácia cai bastante
modeltime_table(wflw_fit_ma_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


residuals_tbl <- 
  modeltime_table(wflw_fit_ma_tuned) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_residuals()

residuals_test <- 
  residuals_tbl %>%
  modeltime_residuals_test(lag = 3)



# Fitting round 3 best RSQmodel

set.seed(335)
wflw_fit_ma_tuned_rsq <- wflw_spec_ma_tune %>%
  finalize_workflow(
    select_best(tune_results_ma_3, "rsq", n = 1)
  ) %>%
  fit(training(splits))

# modelo piora MUITO
modeltime_table(wflw_fit_ma_tuned_rsq) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_accuracy()


#### artifacts

tuned_ma <- list(
  
  # Workflow spec
  tune_wkflw_spec = wflw_spec_ma_tune,
  # Grid spec
  tune_grid_spec = list(
    round1 = grid_spec_1,
    round2 = grid_spec_2,
    round3 = grid_spec_3
  ),
  # Tuning Results
  tune_results = list(
    round1 = tune_results_ma_1,
    round2 = tune_results_ma_2,
    round3 = tune_results_ma_3
  ),
  # Tuned Workflow Fit
  tune_wflw_fit = wflw_fit_ma_tuned,
  # from FE
  splits = artifacts$splits,
  data = artifacts$data,
  recipes = artifacts$recipes,
  standardize = artifacts$standardize,
  normalize = artifacts$normalize
)

tuned_ma %>%
  write_rds(here::here("data", "artifacts", "tuned_ma.rds"))





# 04  CALIBRATION -------------
submodels_tbl # modelos originais

submodels_all_tbl <- modeltime_table(
  tuned_prophet_xgb$tune_wflw_fit,
  tuned_rf$tune_wflw_fit,
  tuned_xgb$tune_wflw_fit,
  tuned_prophet$tune_wflw_fit,
  tuned_thief$tune_wflw_fit,
  tuned_arima$tune_wflw_fit,
  tuned_nnetar$tune_wflw_fit,
  #  tuned_ets$tune_wflw_fit,
  tuned_ma$tune_wflw_fit
)  %>%
  update_model_description(1, "PROPHET W/ XGBOOST ERRORS - Tuned") %>%
  update_model_description(2, "RANGER - Tuned") %>%
  update_model_description(3, "XGBOOST ERRORS - Tuned") %>%  
  update_model_description(4, "PROPHET W/ REGRESSORS - Tuned") %>%  
  update_model_description(5, "TEMPORAL HIERARCHICAL FORECASTING MODEL - Tuned") %>%  
  update_model_description(6, "REGRESSION WITH ARIMA(0,0,0) ERRORS  - Tuned") %>%  
  update_model_description(7, "NNAR(4,1,7)[12]   - Tuned") %>%  
  update_model_description(8, "WINDOW FUNC [29]   - Tuned") %>%  
  combine_modeltime_tables(submodels_tbl)

submodels_all_tbl

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
  write_rds(here::here("data", "artifacts","credito",
                       "workflows_NonandTuned_artifacts_list.rds"))

submodels_all_tbl %>%
  write_rds(here::here("data", "artifacts","credito",
                       "submodels_all_tbl.rds"))


# 05 ENSEMBLES ------------------------
library(modeltime.ensemble)
calibration_tbl <- read_rds(here::here("data", "artifacts", "credito","workflows_NonandTuned_artifacts_list.rds"))
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

loadings_tbl <- submodels_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  mutate(rank = min_rank(-rmse)) %>%
  select(.model_id, rank)

ensemble_fit_wt <- submodels_tbl %>%
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
  modeltime_calibrate(testing(splits)) %>%
  combine_modeltime_tables(calibration_tbl)

calibration_all_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)

#### rolou algum overffiting ne fiote
calibration_all_tbl %>%
  modeltime_accuracy(training(splits)) %>%
  arrange(rmse)






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
artifacts$data$future_tbl |> View()

artifacts$data$future_tbl |> drop_na() |> View()

forecast_stacking_tbl <- refit_stacking_tbl %>%
  modeltime_forecast(
    new_data = artifacts$data$future_tbl,
    actual_data = artifacts$data$data_prepared_tbl %>%
      drop_na(),
    keep_data = TRUE
  )
forecast_stacking_tbl
forecast_stacking_tbl |> View()
plan(sequential)

forecast_stacking_tbl |> names()
forecast_stacking_tbl |> View()

lforecasts <- lapply(X = 1:length(total_mun), FUN = function(x) {
  forecast_stacking_tbl %>%
    filter(uf == total_mun[x]) %>%
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
lforecasts[[1]] |> View()

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
  saveRDS(here::here(
    "data","forecast",
    "credito_forecast.RDS"))

