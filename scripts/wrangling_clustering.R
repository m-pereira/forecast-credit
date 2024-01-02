library(dplyr)
my_tbl <- 
my_tbl %>% 
  janitor::clean_names() %>% 
  select(
    contains(
      c("161","162","163","164","165","166","167","168","169","160","420","432",
        "codmun_ibge","data"))) %>% 
  mutate_if(is.numeric,tidyr::replace_na,replace = 0) 
my_tbl %>% glimpse()
my_tbl <- 
my_tbl %>% 
  transmute(
    x_data_base,codmun_ibge,
    credito = verbete_160_operacoes_de_credito,
    credito_comercial = verbete_161_empres_e_tit_descontados+verbete_162_financiamentos,
    credito_rural = verbete_163_fin_rurais_agricul_cust_invest +
      verbete_164_fin_rurais_pecuar_cust_invest + verbete_165_fin_rurais_agricul_comercializ +
      verbete_166_fin_rurais_pecuaria_comercializ +
      verbete_167_financiamentos_agroindustriais_verbete_168_rendas_a_apropriar_financ_rurais_agroindustriais ,
    captacao = verbete_432_depositos_a_prazo + verbete_420_depositos_de_poupanca,
    poupanca = verbete_420_depositos_de_poupanca
  ) %>%
  tibble()
my_tbl <- 
my_tbl %>% clean_date(.,x_data_base)
my_tbl <- 
my_tbl %>% 
  group_by(data_ref,codmun_ibge) %>% 
  summarise(credito = sum(credito),
            deposito =  sum(captacao))
my_tbl %>% 
  saveRDS(file = here::here("data","cluster_data.RDS"))  
