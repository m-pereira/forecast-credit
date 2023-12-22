library(dtwclust)
library(dtw)
library(tidyverse)
library(ggplot2)

my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS")) %>% 
  pivot_wider(names_from = uf,
              values_from=credito)

my_tbl_uf %>% select(-data_ref)

normalized_series <- scale(my_tbl_uf %>% select(-data_ref))
?dtwclust::tsclust
dtwclust::tsclust()
# Realizar clustering usando K-means com DTW
kmeans_result <- tsclust(
  normalized_series, k = 4,
  type = "partitional",
  distance = "dtw_basic",
  centroid = "pam", keep = TRUE,
  seed = 235)
kmeans_result
kmeans_result %>% plot()
kmeans_result %>% plot(type = "sc")


#Converter para um objeto de clustering
clustering <- as.clustering(kmeans_result)
fpc::cluster.stats(dist(normalized_series, method = "DTW"),kmeans_result@cluster)



