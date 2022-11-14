importing_estban <- function(n_meses){
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
  
  tbl <- list.files(path = "./data/zip",
                    pattern =  "*.CSV",
                    full.names = TRUE) %>%
    map_df(~read.csv(.,header = TRUE,
           sep = ";", skip = 2))
  do.call(file.remove, 
          list(list.files(path = here::here("data","zip"), 
                          full.names = TRUE)))
  saveRDS(tbl,file = here::here("data","raw","estban.RDS"))  
  }



