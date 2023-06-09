# global methods with tune
library(tidyverse)  # loading dplyr, tibble, ggplot2, .. dependencies
library(timetk)  # using timetk plotting, diagnostics and augment operations
library(tsibble)  # for month to Date conversion
library(tsibbledata)  # for aus_retail dataset
d
nest_f <- readRDS(here::here("artifacts",
                            "nested_forecast.RDS"))

nest_f %>% glimpse()
nest_f %>% count(.model_desc)

nest_f %>%
  filter(.model_desc %in% c("ACTUAL","ENSEMBLE MEAN  MODELS")) %>%
  group_by(.index) %>% 
  summarise(.value = sum(.value)) %>% 
  plot_time_series(.date_var = .index,
                   .value = .value)

nest_f_f <- readRDS(here::here("artifacts",
            "nested_forecast_f.RDS"))
nest_f_f%>%
  filter(.model_desc %in% c("ACTUAL","ENSEMBLE MEAN  MODELS")) %>%
  group_by(.index) %>% 
  summarise(.value = sum(.value)) %>% 
  plot_time_series(.date_var = .index,
                   .value = .value)


ml_f <- read_rds(here::here(
  "artifacts",
  "credito_forecast_ml.RDS"))
ml_f %>% 
filter(.model_desc %in% c("ACTUAL","ENSEMBLE (MEAN): 6 MODELS")) %>%
  group_by(.index) %>% 
  summarise(.value = sum(.value)) %>% 
  plot_time_series(.date_var = .index,
                   .value = .value)


ml_f_f <- 
read_rds(here::here(
       "artifacts",
       "filtered_credito_forecast_ml.RDS"))


ml_f_f %>% 
  filter(.model_desc %in% c("ACTUAL","ENSEMBLE (MEAN): 6 MODELS")) %>%
  group_by(.index) %>% 
  summarise(.value = sum(.value)) %>% 
  plot_time_series(.date_var = .index,
                   .value = .value)
