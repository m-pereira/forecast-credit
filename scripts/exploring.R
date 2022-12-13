# 01 EXPLORING BASICS --------------------
## bibliotecas usadas para exploração
library(tidyverse)
library(timetk)
library(tidymodels)
# importar dados por UF
my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS"))  
my_tbl_uf %>% glimpse()
# agrupar o mercado para entender o mercado de crédito do Brasil
my_tbl <- my_tbl_uf %>% 
  group_by(data_ref) %>% 
  summarise(credito = sum(credito))
## limitar algumas UFs pra visualização dos dados
my_tbl_uf <- my_tbl_uf %>% 
  filter(uf %in% c("35","33","43","41","51","27"))

###  como é nossa série temporal?
# é possível ver alguns picos e uma clara tendência 
my_tbl %>% 
  plot_time_series(data_ref,credito)


### destrinchando por UF 
my_tbl_uf %>% 
  group_by(uf) %>% 
  plot_time_series(data_ref,credito,
                   .facet_ncol = 3,
                   .interactive = FALSE)

## buscando visualizar sazonalidade, aparentemente temos
## algum tipo de sazonalidade em julho e agosto com picos menores

my_tbl %>% 
  plot_seasonal_diagnostics(data_ref,credito)


## em termos de anomalias, os últimos meses parecem ser mais importantes
## é o papel do cientisa de dados entender as anomalias
## como conheço bem a fonte de dados, sei que isso 
## é porque os bancos tem até uns 60 dias para atualizar os
## dados no ESTBAN, então é normal os dados mais recentes
## não ter confiáveis

my_tbl %>% 
  plot_anomaly_diagnostics(data_ref,credito)

## A ACF apresenta uma queda lenta, enquanto a PACF
## uma queda brusca. Isso é um comportamento autorregressivo
## O primeiro lag provavelmente vai ser significativo em um
## modelo ARIMA
my_tbl %>% 
  plot_acf_diagnostics(data_ref,credito)


## antes de brincar com modelagem,lembro que os dados do
## ESTBAN são bastante concentrados, vamos ver isso?
## São paulo concentra a maior parte do mercado
## São paulo possui 60% do mercado de crédito
library(ggplot2)
library(forcats)
my_tbl_uf %>% ungroup() %>% 
  filter(data_ref == max(my_tbl_uf$data_ref)) %>% 
  slice_max(credito,n=10) %>% 
  mutate(uf = fct_reorder(uf, desc(credito))) %>% 
  ggplot(aes(x = uf,y = credito))+
  geom_bar(stat="identity",fill="#f68060", alpha=.6, width=.4)+
  #coord_flip() +
  theme_bw()
  
my_tbl_uf %>% 
  ungroup() %>% 
  filter(data_ref == max(my_tbl_uf$data_ref)) %>% 
  slice_max(credito,n=10) %>% 
  mutate(mercado = sum(credito),
         prop = round(credito/mercado*100,1)) %>% 
  select(-mercado)


## Temos alguma ideia sobre nossa série, mas será que um modelo
## linear ajusta bem? Pra isso vamos criar algumas features
## clássicas, baseadas na série temporal. Desde features clássicas,
## até diferenças, defasagens e médias móveis

signature_series <- 
  my_tbl %>% 
  tk_augment_timeseries_signature(data_ref) %>% 
  tk_augment_lags(.value = credito,.lags = c(3,6)) %>% 
  tk_augment_slidify(
    .value = c(credito_lag3),
    .f = ~mean(.x, na.rm = TRUE),
    .period = 3,
    .partial = TRUE,
    .align = "center"
  ) 

## como o tk_agument cria muitas features, vamos remover algumas
## que não fazem sentido. Os dados são mensais, então features
## semanais não tem valor algum.
signature_series %>% names()
contain <- c("wday","week","day","minute",
             "second","hour","am.pm")

## O modelo de regressão ajusta relativamente bem, 
## provavelmente vamos conseguir ajustar modelos simples
## que façam boas previsões


signature_series %>% 
  select(-contains(contain)) %>% 
  plot_time_series_regression(
    .date_var = data_ref,
    .formula = credito ~.,
    .show_summary = TRUE
  )

## o que aprendemos na exploração?
## A série tem tendência
## possui algum tipo de sazonalidade
## possui componentes autorregressivos
## tem uma anomalia nas duas últimas observações, 
## devido a erros de coleta

## Conclusão: Um modelo simples é capaz de realizar a previsão

# 02 FEATURE ENGINEERING WITH RECIPES---------------
## deveríamos remover essas observações, mas vou deixar
## porque coletei poucos dados
#my_tbl <- 
# my_tbl %>% 
#   filter(data_ref < as.Date("2022-06-02"))

my_tbl_sig <- 
  my_tbl %>%
  arrange(data_ref) %>%
  future_frame(data_ref, .length_out = "12 months", 
               .bind_data = TRUE) #%>%
#lag_roll_transformer() 

future_data <- my_tbl_sig %>%
  filter(is.na(credito))


my_tbl_sig <- my_tbl_sig %>% drop_na()

## creating a recipe -----

splits <- my_tbl_sig %>%
  time_series_split(data_ref, 
                    assess = "6 months", 
                    cumulative = TRUE)


recipe_spec <- recipe(credito ~ .,
                      data = training(splits)
)  %>%
  step_log(matches("credito"),offset = 1)

recipe_spec %>%
  prep() %>%
  summary()

juiced_ <-
  recipe_spec %>%
  prep() %>%
  juice()

juiced_ %>% glimpse()
training(splits)
testing(splits)

#
# 03 MACHINE LEARNING ---------------------
library(modeltime) # ML models specifications and engines
library(tictoc) # measure training elapsed time
# prophet --------------

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


prophet_table <- 
  wflw_fit_prophet |> 
  modeltime_table()

## o resíduo tem um comportamento diferente do esperado,
## o modelo não ficou bom 
prophet_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  plot_modeltime_residuals(.interactive = FALSE)

## os testes estatísticos do resíduo confirmam
## qe não tá bom o resíduo
prophet_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  modeltime_residuals_test()

## vamos ver como é a previsão?
# O valor projetado extrapola e muito a previsão
## esse modelo não ficou bom
prophet_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_forecast(
    actual_data = my_tbl_sig,
    new_data      = future_data,
    keep_data     = TRUE) |>
  plot_modeltime_forecast()


# ARIMA -----------------

tic()
wflw_fit_arima <- workflow() %>%
  add_model(
    arima_reg(
      mode = "regression"
    ) %>%
      set_engine("auto_arima")) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits)
  ) 
toc()

arima_table <- 
  wflw_fit_arima |> 
  modeltime_table()

## O resíduo do modelo arima parece estar em torno
## de uma média próxima de zero, mas 
##  nos últimos 2 meses ele erra feio, o que era esperado
## então parece um resíduo bom

arima_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  plot_modeltime_residuals(.interactive = FALSE)

## Apesar do resíduo não ser normalmente distribuído,
## o resíduo tem comportamento melhor
arima_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  modeltime_residuals_test()


arima_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_forecast(
    actual_data = my_tbl_sig,
    new_data      = future_data,
    keep_data     = TRUE) |>
  plot_modeltime_forecast()

# ets --------------------
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

ets_table <- 
  wflw_fit_ets |> 
  modeltime_table()

ets_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  plot_modeltime_residuals(.interactive = FALSE)

ets_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  modeltime_residuals_test()

ets_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_forecast(
    actual_data = my_tbl_sig,
    new_data      = future_data,
    keep_data     = TRUE) |>
  plot_modeltime_forecast()


#TBATS -----------------
tic()
wflw_fit_tbats <-
  workflow() %>%
  add_model(
    seasonal_reg(
      mode = "regression"
    ) %>% #stlm_ets stlm_arima
      set_engine("tbats")
  ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))
toc()

tbats_table <- 
  wflw_fit_tbats |> 
  modeltime_table()

tbats_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  plot_modeltime_residuals(.interactive = FALSE)

tbats_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() |> 
  modeltime_residuals_test()

tbats_table |> 
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_forecast(
    actual_data = my_tbl_sig,
    new_data      = future_data,
    keep_data     = TRUE) |>
  plot_modeltime_forecast()



## modeltime table
submodels_tbl <- modeltime_table(
#  wflw_fit_prophet, # vou deixar o prophet de lado
  wflw_fit_arima,
  wflw_fit_ets, ## não ficou bom
  wflw_fit_tbats
)


submodels_tbl


calibrated_wflws_tbl <- submodels_tbl %>%
  modeltime_calibrate(new_data = testing(splits),
                      quiet = FALSE)

calibrated_wflws_tbl


calibrated_wflws_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse, mae, mape)


artifacts <- list(
  workflows = list(
    wflw_arima = wflw_fit_arima,
    wflw_ets = wflw_fit_ets,
#    wflw_prophet = wflw_fit_prophet,
    wflw_tbats = wflw_fit_tbats
  ),
  calibration = list(calibration_tbl = calibrated_wflws_tbl)
)

# 04 ENSEMBLE ------------------
library(future)
library(doFuture)
library(plotly)
library(modeltime.ensemble)

calibration_tbl <- calibrated_wflws_tbl
calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

ensemble_fit_mean <- submodels_tbl %>%
  ensemble_average(type = "mean")

ensemble_fit_median <- submodels_tbl %>%
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

calibration_all_tbl %>%
  modeltime_accuracy(training(splits)) %>%
  arrange(rmse)

## forecast ----------------------


forecast_ensemble <- 
  calibration_all_tbl |> 
  modeltime_forecast(
    new_data = future_data,
    actual_data = my_tbl_sig,
    keep_data = TRUE
  )
forecast_ensemble |> glimpse()
#forecast_ensemble <- 
# forecast_ensemble |> 
#   mutate(.value = expm1(.value))


forecast_ensemble %>%
  plot_modeltime_forecast(
    .title = "Forecast",
    .conf_interval_show = FALSE,
    .interactive = TRUE
  )





