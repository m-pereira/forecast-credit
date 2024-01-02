library(dtwclust)
library(dtw)
library(tidyverse)
library(ggplot2)

my_tbl <- 
  readRDS( here::here("data","cluster_data.RDS")) %>% 
  ungroup()
my_tbl <- 
my_tbl %>% 
  filter(codmun_ibge != 0) %>% 
  select(-deposito) %>% 
  pivot_wider(names_from = data_ref,
              values_from=credito) %>% 
  mutate_all(~tidyr::replace_na(.,0)) %>% 
  select(where(~ any(. != 0)))
my_tbl %>% glimpse()
my_tbl %>% View()
library(funtimes)
d <- my_tbl %>% select(-data_ref)
t <-  reshape2::melt(t(d))
set.seed(123)
Clus_sync <- sync_cluster(d ~ t, Window = 3, B = 100)

Clus_sync
save.image(file = "clusetr.Rdata")
normalized_series <- scale(my_tbl %>% select(-data_ref))


# Realizar clustering usando K-means com DTW
dtw_basic_3 <- 
tsclust(
  normalized_series, type = "h", k = 2L,
        distance = "dtw_basic",
        control = hierarchical_control(method = "ward.D2"),
        args = tsclust_args(dist = list(window.size = 10L)))

dtw_basic_3@clusinfo
10+16+8+2

dtw_basic_3 <- tsclust(
  normalized_series, k = 3,
  type = "partitional",
  distance = "dtw_basic",
  centroid = "pam", keep = TRUE,
  seed = 235)

dtw_basic_4 <- tsclust(
  normalized_series, k = 4,
  type = "partitional",
  distance = "dtw_basic",
  centroid = "pam", keep = TRUE,
  seed = 235)

dtw_basic_5 <- tsclust(
  normalized_series, k = 5,
  type = "partitional",
  distance = "dtw_basic",
  centroid = "pam", keep = TRUE,
  seed = 235)

dtw_basic_3@clusinfo 
dtw_basic_4@clusinfo
dtw_basic_5@clusinfo

kmeans_result
kmeans_result %>% plot()
kmeans_result %>% plot(type = "sc")


#Converter para um objeto de clustering
clustering <- kmeans_result@cluster
fpc::cluster.stats(dist(normalized_series, method = "DTW"),kmeans_result@cluster)



# sbd_result <- tsclust(
#   normalized_series, k = 4,
#   type = "partitional",
#   distance = "sbd",
#   centroid = "pam", keep = TRUE,
#   seed = 235)
# 
# gak_result <- tsclust(
#   normalized_series, k = 4,
#   type = "partitional",
#   distance = "gak",
#   centroid = "pam", keep = TRUE,
#   seed = 235)
# 
# 
# sdtw_result <- tsclust(
#   normalized_series, k = 4,
#   type = "partitional",
#   distance = "gak",
#   centroid = "sdtw", keep = TRUE,
#   seed = 235)
# 

# $separation: Mede a separação média entre os clusters. Quanto maior o valor, melhor, indicando uma boa distinção entre os clusters.
# $diameter: Representa o diâmetro médio dos clusters. O diâmetro de um cluster é a maior distância entre dois pontos dentro do cluster. Clusters com diâmetros menores são geralmente desejáveis.
# $noisen: Número de pontos considerados como ruído (não atribuídos a nenhum cluster). É útil em conjuntos de dados onde há pontos que não pertencem claramente a nenhum cluster.
# $average.toother: Média das distâncias médias de cada ponto para pontos em outros clusters. Valores mais altos indicam que os pontos estão mais separados dos outros clusters.
# $pearsongamma: Mede a força da associação linear entre a dissimilaridade (distância) e a proximidade entre os pontos no espaço original. Pode indicar a presença de estruturas lineares.
# $ch: Índice de Calinski-Harabasz, que avalia a coesão intracluster em relação à separação entre clusters. Quanto maior, melhor.
# $widestgap: Largura da maior lacuna entre clusters. Pode indicar se há uma lacuna clara entre os clusters.
# $sindex: Índice de Silhouette Médio. Mede a coesão intracluster em relação à separação entre clusters. Varia de -1 a 1, onde valores mais próximos de 1 são desejáveis.

# separation:
# Interpretação: Mede a separação média entre os clusters.
# Orientação Geral: Quanto maior, melhor. Indica que os clusters estão bem separados.
# $diameter:
#   Interpretação: Representa o diâmetro médio dos clusters.
# Orientação Geral: Quanto menor, melhor. Diâmetros menores indicam clusters mais compactos.
# $noisen:
#   Interpretação: Número de pontos considerados como ruído.
# Orientação Geral: Menor é geralmente melhor, pois clusters mais puros têm menos pontos de ruído.
# $average.toother:
#   
#   Interpretação: Média das distâncias médias de cada ponto para pontos em outros clusters.
# Orientação Geral: Quanto maior, melhor. Indica que os pontos estão mais separados dos outros clusters.
# $pearsongamma:
#   
#   Interpretação: Mede a força da associação linear entre a dissimilaridade e a proximidade entre os pontos no espaço original.
# Orientação Geral: Mais próximo de 1 indica uma associação mais forte, mas a interpretação depende do contexto específico.
# $ch:
#   
#   Interpretação: Índice de Calinski-Harabasz, avaliando a coesão intracluster em relação à separação entre clusters.
# Orientação Geral: Quanto maior, melhor. Indica clusters mais coesos e bem separados.
# $widestgap:
#   
#   Interpretação: Largura da maior lacuna entre clusters.
# Orientação Geral: Quanto maior, melhor. Uma lacuna maior pode indicar uma estrutura mais clara entre clusters.
# $sindex:
#   
#   Interpretação: Índice de Silhouette Médio.
# Orientação Geral: Mais próximo de 1 é geralmente melhor. Indica clusters bem definidos e separados.
# 


teste <- kmeans(dist(normalized_series),3)
?EMCluster
?kohonen
?dbscan

?dtwclust::tsclust
?hclust
hierarchical_result <- hclust(
  dist(normalized_series,"euclidean"), method = "ward.D2")

plot(hierarchical_result)
hierarchical_result
plot(hierarchical_result)