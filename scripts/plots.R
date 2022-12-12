# global methods with tune
library(tidyverse)  # loading dplyr, tibble, ggplot2, .. dependencies
library(timetk)  # using timetk plotting, diagnostics and augment operations
library(tsibble)  # for month to Date conversion
library(tsibbledata)  # for aus_retail dataset
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

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
