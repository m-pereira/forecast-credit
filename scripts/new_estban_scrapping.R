library(tidyverse)
caminho_do_diretorio <- "data/zip"
# Use dir.create() para criar o diretório
if (!file.exists(caminho_do_diretorio)) {
  dir.create(caminho_do_diretorio, recursive = TRUE)
  cat("Diretório criado com sucesso.\n")
} else {
  cat("O diretório já existe.\n")
}

# Vetor com as partes finais das URLs
anos <- 2023
mes <- c("01","02","03","04","05","06","07","08","09",10,11,12) 
partes_finais_url <- expand.grid(anos,mes) %>% 
  mutate(anomes= paste0(Var1,Var2,"_ESTBAN_AG")) %>% 
  pull(anomes)

# Crie um diretório temporário
# Loop sobre as partes finais do URL
for (parte_final_url in partes_finais_url) {
  # Construa a URL completa
  url <- paste0("https://www.bcb.gov.br/content/estatisticas/estatistica_bancaria_estban/agencia/", parte_final_url, ".csv.zip")
  
  # Combine o diretório temporário com o nome do arquivo zip
  caminho_local <- file.path(caminho_do_diretorio, paste0(parte_final_url, ".zip"))
  
  # Use a função download.file para baixar o arquivo zip
  download.file(url, caminho_local, mode = "wb")
  
  # Verifique se o download foi bem-sucedido
  if (file.exists(caminho_local)) {
    cat("Download concluído com sucesso. O arquivo está em:", caminho_local, "\n")
  } else {
    cat("Erro ao baixar o arquivo.\n")
  }
}



# Loop sobre as partes finais do URL
for (parte_final_url in partes_finais_url) {
  tryCatch({
    # Construa a URL completa
    url <- paste0("https://www.bcb.gov.br/content/estatisticas/estatistica_bancaria_estban/agencia/", parte_final_url, ".csv.zip")
    
    # Combine o diretório temporário com o nome do arquivo zip
    caminho_local <- file.path(caminho_do_diretorio, paste0(parte_final_url, ".zip"))
    
    # Use a função download.file para baixar o arquivo zip
    download.file(url, caminho_local, mode = "wb")
    
    # Verifique se o download foi bem-sucedido
    if (file.exists(caminho_local)) {
      cat("Download concluído com sucesso. O arquivo está em:", caminho_local, "\n")
    } else {
      cat("Erro ao baixar o arquivo.\n")
    }
  }, error = function(e) {
    cat("Erro durante o download. Mensagem de erro:", conditionMessage(e), "\n")
  })
}



list.files(path = caminho_do_diretorio,
           pattern = "*.zip",
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
saveRDS(my_tbl,file = here::here("data","raw","estban.RDS"))  


