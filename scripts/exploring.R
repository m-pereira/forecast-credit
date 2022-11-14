#
library(tidyverse)
library(timetk)
library(tidymodels)
my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS"))  
my_tbl_uf %>% glimpse()
my_tbl <- my_tbl_uf %>% 
  group_by(data_ref) %>% 
  summarise(credito = sum(credito))
my_tbl_uf <- my_tbl_uf %>% 
  filter(uf %in% c("35","33","43","41","51","27"))
my_tbl %>% 
  plot_time_series(data_ref,credito)

my_tbl_uf %>% 
  group_by(uf) %>% 
  plot_time_series(data_ref,credito,
                   .facet_ncol = 3,
                   .interactive = FALSE)

## sazonalidade
my_tbl %>% 
  plot_seasonal_diagnostics(data_ref,credito)
# anomalias
my_tbl %>% 
  plot_anomaly_diagnostics(data_ref,credito)

## ACF
my_tbl %>% 
  plot_acf_diagnostics(data_ref,credito)

## modelo linear funciona?
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
signature_series %>% 
  plot_time_series_regression(
    .date_var = data_ref,
    .formula = credito ~.
  )
