library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)
my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS"))  

my_tbl_uf %>% glimpse()
my_tbl_uf %>% select(data_ref) %>% tail()
selic <- 
  readRDS( here::here("data","selic.RDS"))   %>% 
  mutate(data_ref = as.Date(paste0(lubridate::year(date),"-",
                                   lubridate::month(date),"-",
                                   "01"
  ))) %>% select(-date)

selic_pessimista <-
  selic %>%
  mutate(
    selic = ifelse(data_ref> as.Date("2024-01-01"),selic+5,selic))

selic_otimista <-
  selic %>%
  mutate(
    selic = ifelse(data_ref> as.Date("2024-01-01"),selic-5,selic))

selic
my_tbl_uf <-
  my_tbl_uf %>% inner_join(selic)
selic_future <-
  bind_rows(selic,selic_pessimista,selic_otimista)
FORECAST_HORIZON <- 12

#my_tbl_uf <- my_tbl_uf %>% filter(uf %in% c("11","27","15"))
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
    .length_test = 16
  )

my_tbl_nest
my_tbl_nest %>% glimpse()
my_tbl_nest$.actual_data[[1]]
my_tbl_nest$.future_data[[1]]
my_tbl_nest <- 
  my_tbl_nest %>% 
  mutate(
    .future_data = map(.future_data,
                       ~select(.,data_ref,credito)),
    .future_data = map(.future_data, 
                       ~inner_join(.x,selic_future))
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
  slice(2) %>%
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
      #wflw_prophet,
      wflw_arima,
     wflw_ets
    ),
    control = control_nested_fit(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
toc()
parallel_stop()
gc()
#Finished in: 33.2733 secs.
nested_modeltime_tbl %>%
  extract_nested_error_report()
nested_modeltime_tbl$.future_data[[1]]

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

nested_modeltime_good_tbl <- 
  nested_modeltime_tbl %>% 
  mutate(
    .modeltime_residuals = map(.modeltime_tables ,
                       ~modeltime_residuals(.)),
    .residuals = map(.modeltime_tables ,
                               ~modeltime_residuals(.) %>% 
                                 pull(.residuals)),
    
  )

nested_modeltime_good_tbl <- 
  nested_modeltime_good_tbl%>% 
  mutate(
    .ljung_box = map(.residuals,
                     ~Box.test(.,type = "Ljung-Box", lag = 12) %>% 
                       pluck("p.value"))
  )

?unnest
nested_modeltime_good_tbl$.residuals[1]
nested_modeltime_good_tbl %>% 
  select(uf, .modeltime_tables,
         .residuals) %>% 
  unnest(.modeltime_tables) %>% 
  mutate(test = Box.test(as.vector(.residuals)))

mutate(
    .modeltime_residuals = (.modeltime_tables ,
                               ~modeltime_residuals(.)),
    .residuals = map(.modeltime_tables ,
                     ~modeltime_residuals(.) %>% 
                       pull(.residuals)),
    
  )
  
mutate(.p_value  = unnest(.ljung_box),
         .modeltime_tables = unn)
nested_modeltime_good_tbl %>% 
  unnest(.ljung_box) %>% View()

nested_modeltime_good_tbl$.ljung_box[1] 

?Box.test
serial.test 
?modeltime_residuals_test
forecast::gghistogram()
Box.test()
?vars::serial.test()
nested_modeltime_good_tbl$.residuals
nested_modeltime_good_tbl$.residuals


teste <- 
nested_modeltime_good_tbl$.modeltime_tables[1]
teste %>% unnest(.calibration_data)

attr_getter(nested_modeltime_tbl)

modeltime_residuals()
extract_nested_test_accuracy()


## forecast best ----------------------------
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



nest_best_tbl_refit <-
  nest_best_tbl %>%
  modeltime_nested_refit(
    control = control_refit(
      verbose = TRUE,
      allow_par = TRUE
    )
  )
nest_best_tbl_refit$.actual_data[[1]] %>% tail()


nest_best_tbl_refit %>% 
  extract_nested_future_forecast() %>% 
  filter(uf == "11",
         .key == "prediction") %>% 
  View()

reps <- nest_best_tbl_refit %>% pull(uf) %>% length()

cenarios <- rep(c("selic base","selic pessimista","selic otimista"),
                reps * FORECAST_HORIZON)

my_scenario_tbl <- 
  nest_best_tbl_refit %>% 
  extract_nested_future_forecast()  %>% 
  filter(.key == "prediction") %>% 
  mutate(
    cenario = cenarios
  ) %>% 
  bind_rows(
    nest_best_tbl_refit %>% 
      extract_nested_future_forecast()  %>% 
      filter(.key != "prediction") %>% 
      mutate(
        cenario = "historico"
      )
    
  )
my_scenario_tbl %>% View()

?plot_modeltime_forecast

f_cenario_base <- 
  my_scenario_tbl %>% 
  filter(cenario %in% c("historico","selic base")) %>% 
  group_by(uf) %>% 
  plot_modeltime_forecast(
    .title = "Cenario Base",
    .interactive = FALSE
    # .facet_ncol = 3,
    # .facet_nrow = 3
  ) 

f_cenario_pessimista <- 
  my_scenario_tbl %>% 
  filter(cenario %in% c("historico","selic pessimista")) %>% 
  group_by(uf) %>% 
  plot_modeltime_forecast(
    .title = "Cenario pessimista",
    .interactive = FALSE    
    # .facet_ncol = 3,
    # .facet_nrow = 3
  ) 

f_cenario_otimista <- 
  my_scenario_tbl %>% 
  filter(cenario %in% c("historico","selic otimista")) %>% 
  group_by(uf) %>% 
  plot_modeltime_forecast(
    .title = "Cenario otimista",
    .interactive = FALSE
    # .facet_ncol = 3,
    # .facet_nrow = 3
  ) 

library(patchwork) # for putting ggplot objects together 
wrap_plots(f_cenario_base, f_cenario_pessimista, f_cenario_otimista)


my_scenario_tbl %>% 
  group_by(uf) %>% 
  plot_time_series(
    .date_var = .index,
    .value = .value,
    .color_var = cenario,
    .facet_ncol = 2)

library(ggplot2)
library(tidyquant)
my_scenario_tbl %>% 
  #mutate(.value = exp(.value)) %>% 
  ggplot(aes(x = .index,y=.value,color=cenario)) +
  geom_line() +
  #  geom_smooth(method = "loess") +
  labs(title = "Forecasting mercado",
       subtitle = "Comparaçao de múltiplos cenários",
       caption = "A maior confiança é no cenário base",
       x = "", y = "mercado",
       color = "cenario") +
  facet_wrap(~ uf,scales = 'free')+
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar)

