## importando 36 amostras dos dados 
importing_estban(36) 

## lendo no R o arquivore gerado
my_tbl <- readRDS(here::here("data","raw","estban.RDS"))  %>% 
  formating() %>% 
  clean_date(.,x_data_base) %>% 
  aggregate_uf(.,codmun_ibge,x_data_base) 
my_tbl
