importing_estban <- function(n_meses){
  n_meses <- 10
  library(httr)
  library(rvest)
  library(tidyverse)
  estban <- httr::GET("https://www4.bcb.gov.br/fis/cosif/estban.asp?frame=1")
  html_estban <- content(estban, 'text')
  xpath  <-  '//label [@for="ESTBAN_MUNICIPIO"]'
  lista_dados <- read_html(html_estban) %>% 
    rvest::html_nodes(xpath = '//*[@id = "ESTBAN_MUNICIPIO"]') %>%
    rvest::html_nodes('option') %>% 
    rvest::html_attr('value') 
  substr(lista_dados, start = 34, stop = 50)
  lista_dados <- lista_dados[1:n_meses]
  paste0('https://www4.bcb.gov.br',lista_dados)
  files <- paste0('https://www4.bcb.gov.br',lista_dados)
  destfiles <- substr(files,57,80)
  download.file(
    files,
    destfile = here::here("data","zip",destfiles),
    mode = "wb")
  list.files(path = here::here("data","zip"),
             pattern = "*.ZIP",
             full.names = TRUE)  %>% 
    walk(~ unzip(.,exdir = "data/zip/"))
  
  my_tbl <- list.files(path = "./data/zip",
                    pattern =  "*.CSV",
                    full.names = TRUE) %>%
    map_df(~read.csv(.,header = TRUE,
           sep = ";", skip = 2))
  do.call(file.remove, 
          list(list.files(path = here::here("data","zip"), 
                          full.names = TRUE)))
  saveRDS(my_tbl,file = here::here("data","raw","estban2.RDS"))  
  }

formating <- function(df){
  require("dplyr")
  require("janitor")
  
df <- df %>% 
  clean_names() %>%
  select(
    contains(
      c("161","162","163","164","165","166","167","168","169","160","420","432",
        "codmun_ibge","data"))) %>% 
  mutate_if(is.numeric,tidyr::replace_na,replace = 0) %>% 
  transmute(
    x_data_base,codmun_ibge,
    credito = verbete_160_operacoes_de_credito,
    credito_comercial = verbete_161_empres_e_tit_descontados+verbete_162_financiamentos,
    credito_rural = verbete_163_fin_rurais_agricul_cust_invest +
      verbete_164_fin_rurais_pecuar_cust_invest + verbete_165_fin_rurais_agricul_comercializ +
      verbete_166_fin_rurais_pecuaria_comercializ +
      verbete_167_financiamentos_agroindustriais_verbete_168_rendas_a_apropriar_financ_rurais_agroindustriais +
      verbete_167_financiamentos_agroindustriais,
      captacao = verbete_432_depositos_a_prazo + verbete_420_depositos_de_poupanca,
      poupanca = verbete_420_depositos_de_poupanca
  ) %>%
  tibble()
return(df)
}


clean_date <- function(df,column_date){
  require(tidyr)
  df <- 
  df %>% 
    mutate(
      ano = (substr(as.character({{ column_date }}),1,4)),
      mes = (substr(as.character({{ column_date }}),5,6)),
      dia = 01 
    ) %>% 
    unite(.,
          col = "data_ref",c("dia","mes","ano"),
          sep = "/",
          remove = TRUE) %>% 
    mutate(data_ref = as.Date(data_ref,format = "%d/%m/%Y")) %>% 
    select(-{{ column_date }})
  return(df)
}



aggregate_uf <- function(df,cod_ibge,data_ref){
  require(dplyr)
  df <- df %>% 
    group_by({{ cod_ibge }},{{ data_ref }}) %>%
    summarise_if(is.numeric,sum) %>%
    mutate(uf = substr({{ cod_ibge }},1,2)) %>%
    ungroup() %>% 
    select(-{{ cod_ibge }}) %>% 
    group_by(uf,{{ data_ref }}) %>% 
    summarise_if(is.numeric,sum) %>% 
    filter(uf > 0) 
  return(df)
  
}



