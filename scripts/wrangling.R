library(dplyr)
## buscando as funçõees usadas
source(here::here("scripts","functions.R"))
## importando 36 amostras dos dados 
## importing_estban(36) 

## lendo no R o arquivore gerado
my_tbl <- 
  readRDS(here::here("data","raw","estban.RDS"))  %>% 
  # selecionando colunas de interesse
  formating() %>% 
  # limpando a coluna data
  clean_date(.,x_data_base) %>%
  # agregando por UF
  aggregate_uf(.,codmun_ibge,data_ref) 

my_tbl %>% glimpse()
# salvando o arquivo
my_tbl %>% 
  saveRDS(file = here::here("data","cleaned.RDS"))  
