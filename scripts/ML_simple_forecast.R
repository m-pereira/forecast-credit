# 01 BASICS --------------------
library(tidyverse)
library(timetk)
library(tidymodels)
# importar dados por UF
my_tbl <- m4_monthly %>% filter(id == "M750") %>% 
  select(-id)

# 02 FEATURE ENGINEERING WITH RECIPES---------------


splits <- my_tbl %>% 
  time_series_split(
    assess     = "6 months", 
    cumulative = TRUE
  )

rec_obj <- recipe(value ~ .,
                      data = training(splits)
)  %>%
#  step_log(matches("credito"),offset = 1)%>%
  step_scale("value") %>%
  step_timeseries_signature(date) %>%
  step_fourier(date,period = 12, K = 3) %>%
  step_rm(date) %>%
  step_corr() %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) 

rec_obj %>%
  prep() %>%
  summary()

rec_obj %>%
  prep() %>%
  bake(testing(splits))


wflw_xgb <- workflow() %>%
  add_model(
    boost_tree("regression") %>% set_engine("xgboost")
  ) %>%
  add_recipe(rec_obj) %>%
  fit(training(splits))

wflw_xgb %>% extract_fit_engine()
wflw_xgb %>% extract_fit_parsnip()


model_tbl <- modeltime_table(
  wflw_xgb
)

calib_tbl <- model_tbl %>%
  modeltime_calibrate(
    new_data = testing(splits)
  )

calib_tbl %>% 
  modeltime_accuracy() %>% 
  table_modeltime_accuracy(.interactive = FALSE)


calib_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = my_tbl
  ) %>%
testing(splits) %>% 
  plot_modeltime_forecast(
    .interactive = FALSE
  )


refit_tbl <- calib_tbl %>%
  modeltime_refit(data = my_tbl)

future_tbl <- my_tbl %>%
  future_frame(.length_out = 52, .bind_data = FALSE)

refit_tbl %>%
  modeltime_forecast(
    new_data    = future_tbl,
    actual_data = my_tbl
  ) %>%
  plot_modeltime_forecast(
    .interactive = F,
    .facet_ncol  = 2
  )


wflw_xgb %>% extract_fit_engine()
parsnip_xgb <- 
wflw_xgb %>% extract_fit_parsnip()

library(vip)
wflw_xgb %>% 
  extract_fit_parsnip() %>% 
  vip()

library(DALEX)
library(DALEXtra)

## this is used if the model is classification
my_func <- function(model,df){
  return(predict(model,df,type="prob") %>% pull(2))}
new_observations <- testing(splits)
#  rec_obj %>% prep() %>% bake(testing(splits))

predict(parsnip_xgb,new_observations)

explainer <- 
  explain_tidymodels(
    model = parsnip_xgb,
    data = my_tbl %>% select(-value),
    y = my_tbl$value,
    labels = "xgboost_with_tune"
  )

modelStudio::modelStudio(
  explainer,new_observations
)

modelDown::modelDown(
  explainer,
  modules = c("variable_importance","variable_response")
)

my_prof <- model_profile(explainer)
plot(my_perf,type = "partial",variable = c("x","y"))

library(localModel)
library(lime)
my_lime <- 
  predict_surrogate(
    explainer = explainer,
    new_observation =new_observations,
    n_features = 3,
    n_permunations = 100,
    type = "lime"
  )