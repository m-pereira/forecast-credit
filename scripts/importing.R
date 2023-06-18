library(tidyverse)
library(ipeadatar)
library(BETS)
library(rbcb)
# Estoque de empregos formais - Total - Encadeamento do antigo e novo CAGED
# Taxa de câmbio - Livre - Venda - Média mensa
# Índice Nacional de Preços ao Consumidor Amplo (IPCA)
# Taxa de juros pré fixada - Estrutura a termo - LTN - 12 meses - Anbima
# PIB mensal - Valores correntes
# Meios de pagamento amplos - M4 - Saldo em final de período
# Imposto sobre a renda (IR) - Total - Receita bruta - ME
# Índice de volume de vendas no comércio varejista ampliado - PMC
# Vendas de veículos pelas concessionárias - Automóveis - Fenabrave
# Consumo de energia elétrica - Comercial - Quantidade - Eletrobras
#ipeadatar::search_series(terms = 'Taxa de câmbio nominal', fields = c('name'), language = c("br"))
rbcb::rbcb_search("ipca")
#selic_search1    <-  rbcb::get_series(11        ,start_date = "2019-06-01")
selic_search  <-  rbcb::get_series(1178,start_date = "2019-01-01")
selic_search <- 
selic_search %>% mutate(ano = lubridate::year(date),
                        mes = lubridate::month(date)) %>% 
  group_by(ano,mes) %>% 
  slice_min(date) %>% 
  ungroup() 
selic_search%>% 
  ggplot(
    aes(x = date, y = `1178`)
  ) + geom_line()

#selic_mes       <-  rbcb::get_series(4390        ,start_date = "2019-06-01")
desc_search     <-  rbcb::get_series(24369       ,start_date = "2019-06-01")
prod_ind_search <-  rbcb::get_series("industrial",start_date = "2019-06-01")
pib_search      <-  rbcb::get_series(24115       ,start_date = "2019-06-01")
ipca_search     <-  rbcb::get_series(4449        ,start_date = "2019-06-01")

expectations <- 
  rbcb::get_annual_market_expectations(c(
    'Selic','Taxa de desocupação',
#    'Produção industrial',
    'PIB Total', 'IPCA',
    'Taxa de câmbio',
    'Meta para taxa over-selic',
    'Fiscal'),
    start_date = '2016-06-01')
expectations %>% View()
expectations %>% filter(indic == "Selic") %>% 
  filter(reference_date == 2024) %>% 
  group_by(date) %>% summarise(median = median(median)) %>% 
  ggplot(aes(x = date, y = median)) + geom_line()

monthly_expectations <- 
  rbcb::get_monthly_market_expectations(c(
    'Selic','Taxa de desocupação',
 #   'Produção industrial',
    'PIB Total', 'IPCA',
    'Taxa de câmbio',
    'Meta para taxa over-selic',
    'Fiscal'),
    start_date = '2023-01-01')

sliced_exp <- 
expectations %>% 
  filter(indic == "Selic") %>% 
  filter(reference_date == 2024) 

selic_24 <- 
sliced_exp %>% 
  filter(date > as.Date('2023-04-01')) %>% 
  summarise(
    selic = median(median)
  ) %>% 
  mutate(
    date = as.Date("2024-12-01")
  )

selic_mensal <- 
selic_search %>% 
  rename(selic = `1178`) %>% 
  mutate(anomes = as.numeric(paste0(lubridate::year(date),lubridate::month(date)))
         ) %>%
  group_by(anomes) %>% 
  slice_max(date,n=1) %>% 
  ungroup()
selic_mensal %>%
  ggplot(aes(x = date, y = selic))+
  geom_line()
selic_final <- 
selic_mensal %>% 
  bind_rows(tibble(
    date = seq(selic_mensal %>% pull(date) %>% max()+30,
               as.Date('2024-12-30')-30,
               by="month"),
    selic = NA
  )
  ) %>%
  bind_rows(selic_24) 
selic_final <- 
  selic_final %>% select(date,selic) %>% 
  mutate(selic = zoo::na.approx(selic)) 

selic_final %>% 
  left_join(selic_mensal %>% 
               rename(selic_ajustada =selic)) %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = selic), color = "darkred", linetype="twodash")+
  geom_line(aes(y = selic_ajustada), color = "steelblue")+
  
  theme_minimal()

selic_final %>% saveRDS(here::here("data","selic.RDS"))
