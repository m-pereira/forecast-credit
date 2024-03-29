library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)
my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS"))  

my_tbl_uf %>% glimpse()
selic <- 
  readRDS( here::here("data","selic.RDS"))   %>% 
  mutate(data_ref = as.Date(paste0(lubridate::year(date),"-",
                          lubridate::month(date),"-",
                          "01"
  ))) %>% select(-date)
my_tbl_uf <- 
my_tbl_uf %>% inner_join(selic) 
 
FORECAST_HORIZON <- 12


## split data -------------
my_tbl_nest <-
  my_tbl_uf %>%
    group_by(uf) %>%
  extend_timeseries(
    .id_var = uf,
    .date_var = data_ref,
    .length_future = FORECAST_HORIZON
  ) %>%
  nest_timeseries(
    .id_var = uf,
    .length_future = FORECAST_HORIZON
  ) %>%
  split_nested_timeseries(
    .length_test = 12
  )

my_tbl_nest
my_tbl_nest %>% glimpse()
my_tbl_nest$.actual_data[[1]]
my_tbl_nest$.future_data[[1]]
my_tbl_nest <- 
my_tbl_nest %>% 
  mutate(
    .future_data = map(.future_data,~select(.,data_ref,credito)),
    .future_data = map(.future_data, ~inner_join(.x,selic))
  )
my_tbl_nest$.future_data[[1]]

# modelling ------------
## recipes
recipe1 <- recipe(
  credito ~ .,
  extract_nested_train_split(my_tbl_nest)
) %>% 
  step_log(credito) %>% 
  step_dummy(all_nominal_predictors())



recipe1 %>%
  prep()

recipe1 %>%
  prep() %>%
  bake(
    extract_nested_best_model_report(my_tbl_nest)
  ) %>% glimpse()

recipe1 %>%
  prep() %>%
  bake(
    extract_nested_best_model_report(my_tbl_nest)
  ) %>% View()


## workflow -----------------------
# prophet
wflw_prophet <- workflow() %>%
  add_model(
    prophet_reg("regression") %>%
      set_engine("prophet")
  ) %>%
  add_recipe(recipe1)

## arima
wflw_arima <- workflow() %>%
  add_model(
    arima_reg(seasonal_period = 12) %>%
      set_engine("auto_arima")
  ) %>%
  add_recipe(recipe1)

#arima boost
wflw_arima_boost <- workflow() %>%
  add_model(
    arima_boost(seasonal_period = 12) %>%
      set_engine("auto_arima_xgboost")
  ) %>%
  add_recipe(recipe1)



## ets
wflw_ets <-
  workflow() %>%
  add_model(
    exp_smoothing() %>%
      set_engine("ets")
  ) %>%
  add_recipe(recipe1)



### teste com 1 serie
try_sample_tbl <-
  my_tbl_nest %>%
  slice(25) %>%
  modeltime_nested_fit(
    model_list = list(
      wflw_prophet,
      wflw_arima,
      wflw_arima_boost,
      wflw_ets
    ),
    control = control_nested_fit(
      verbose = TRUE,
      allow_par = FALSE
    )
  )


try_sample_tbl


## check errors

try_sample_tbl %>% extract_nested_error_report()

## scale------

parallel_start(6)
library(tictoc)
tic()
nested_modeltime_tbl <-
  my_tbl_nest %>%
  modeltime_nested_fit(
    model_list = list(
      wflw_prophet,
      wflw_arima,
      #wflw_ets#,
      #   wflw_arima_boost
    ),
    control = control_nested_fit(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()
parallel_stop()

#Finished in: 33.2733 secs.
nested_modeltime_tbl %>%
  extract_nested_error_report()

## test accuracy ----
nested_modeltime_tbl

nested_modeltime_tbl %>%
  extract_nested_test_accuracy()  %>%
  table_modeltime_accuracy(.interactive = F)

nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  filter(uf == 11) %>%
  group_by(uf) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = TRUE
  )

## select best ----------------------------
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric                = "rmse", 
    minimize              = TRUE, 
    filter_test_forecasts = TRUE
  )
nest_best_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE)

report_nest_best_tbl <-
  nest_best_tbl %>%
  extract_nested_best_model_report()


report_nest_best_tbl |> pull(.model_desc) |> table()

## visualize best models

### refit ----------------------

nest_best_tbl_refit <-
  nest_best_tbl %>%
  modeltime_nested_refit(
    control = control_refit(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
nest_best_tbl_refit
nest_best_tbl_refit %>%
  write_rds(here::here("artifacts","trained-external-reg.RDS"))

# review any errors

# ## visualize

nest_best_tbl_refit %>%
  extract_nested_future_forecast() %>%
  group_by(uf) %>%
  plot_modeltime_forecast()

best_refit_models <- nest_best_tbl_refit %>%
  extract_nested_best_model_report()
best_refit_models



# basic forecast ----
forecast_tbl <-
  nest_best_tbl_refit %>%
  extract_nested_future_forecast()


## Ensemble -----------------
library(modeltime.ensemble)

nested_ensemble_tbl_mean <- 
  nested_modeltime_tbl %>%
  ensemble_nested_average(
    type           = "mean", 
    keep_submodels = TRUE
  )
nested_ensemble_tbl_mean
## select best
nested_ensemble_tbl_mean  %>%
  extract_nested_test_accuracy() %>%
  group_by(uf) %>%
  table_modeltime_accuracy(.interactive = FALSE)


#
best_nested_modeltime_tbl <-
  nested_ensemble_tbl_mean %>%
  modeltime_nested_select_best(
    metric                = "rmse",
    minimize              = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>% 
  extract_nested_best_model_report() %>%
  table_modeltime_accuracy(.interactive = FALSE)

best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(uf) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = FALSE
  )


# forecast -----------------------
nested_modeltime_refit_tbl <- nested_ensemble_tbl_mean %>%
  modeltime_nested_refit(
    control = control_nested_refit(verbose = TRUE)
  )

nested_modeltime_refit_tbl
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(uf) %>%
  plot_modeltime_forecast(
    .interactive = TRUE,
    .facet_ncol  = 2
  )

nested_modeltime_refit_tbl %>%
  write_rds(here::here("artifacts","trained-nested-external.RDS"))

nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>% 
  saveRDS(here::here("artifacts",
                     "nested_external.RDS"))
my_forecast <- 
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast()
my_forecast %>% View()
