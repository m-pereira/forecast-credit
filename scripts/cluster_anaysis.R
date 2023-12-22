library(tidyverse)
library(TSclust)

my_tbl_uf <- 
  readRDS( here::here("data","cleaned.RDS")) %>% 
  pivot_wider(names_from = uf,
              values_from=credito) 

IP.dis <- diss(my_tbl_uf %>% select(-data_ref), "INT.PER")
# hierarchical cluster solution
IP.hclus <- cutree(hclust(IP.dis), k = 4)
IP.hclus %>% table()


IP.pamclus <- pam(IP.dis, k = 6)$clustering

# analyzed Autocorrelation based solution
ACF.dis <- diss(my_tbl_uf %>% select(-data_ref), "ACF", p = 0.05)
ACF.hclus <- cutree(hclust(ACF.dis), k = 6)
ACF.hclus %>% table()

ACF.pamclus <- pam(ACF.dis, k = 6)$clustering
ACF.pamclus %>% table()


# dissimilarity based on ARMA models, take the p-value

AR.MAH.PVAL.dis <- diss(my_tbl_uf %>% select(-data_ref), "AR.MAH")$p_value
# p-value clustering method
AR.MAH.PVAL.dis
AR.MAH.PVAL.clus <- pvalues.clust(AR.MAH.PVAL.dis, 0.05)
AR.MAH.PVAL.clus
pvalues.clust(AR.MAH.PVAL.dis, 0.6)


# nonparametric spectral approximation dissimilarity, high computational
# requirements
LLR.dis <- diss(my_tbl_uf %>% select(-data_ref), "SPEC.LLR", meth = "LK", n = 500)
LLR.pam <- pam(LLR.dis, k = 3)$clustering
cluster.evaluation(true_cluster, LLR.pam)
LLR.pam
