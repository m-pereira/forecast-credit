
# forecast-credit

<!-- badges: start -->
<!-- badges: end -->

Cada vez mais empresas previsam de previsões em centenas e até milhares de produtos, entretanto fazer previsões com qualidade não é tão fácil. Passei por situações assim algumas vezes e acho duas abordagens muito interessantes para essa atividade: `Nested Forecast` e o uso de ML em modelos globais. Nesse projeto <https://github.com/m-pereira/forecast-credit> aplico de forma prática esses conceitos, realizando previsões sobre o mercado de crédito para o próximo ano e explicando passo a passo os prós e contras de cada decisão tomada. Como já trabalhei em banco, prever crédito é uma tarefa importante, podendo assim aproveitar de uma alta do ciclo de crédito, ou se proteger de um ciclo de crédito em baixa.

Para usar esse repositório você precisar instalar alguns pacotes:
```
install.packages("tidyverse","timetk","tidymodels","modeltime",
                 "httr","rvest","tictoc","modeltime.ensemble",
                 "tsibble", "fastDummies","tidymodels","future",
                 "doFuture","plotly")
```

A série que analisada é o mercado de crédito, o volume de crédito é uma série que apresenta uma tendência clara, isso é normal, uma vez que a série não é deflacionada, então é natural que anualmente o mercado seja nominalmente maior que o ano anterior. Devido essa característica forte na série, é provável que modelos estatísticos clássicos sejam úteis para estimar o mercado de crédito futuro. A queda abrupta dos últimos dois meses se destacam, entretanto é algo esperado, pois é comum as informações do ESTBAN serem divulgadas faltando alguma instituição financeira, assim a queda os dois últimos valores são viesados para baixo, o correto seria remover as duas obervações, mas vou manter nesse projeto por curiosidade sobre como vai afetar os modelos estimados.


![alt text](https://github.com/m-pereira/forecast-credit/blob/main/ts.png)

A série também apresenta correlações com lags passados, indicando que informações passadas podem ajudar a prever a séie futura, utilizando a metodologia Box e Jenkins em modelos ARIMA. Com um decaimento exponencial da ACF e informações estatisticamente significativas na PACF.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/acf_pacf.png)

Ainda explorando o mercado como um todo, estima-se um modelo linear simples, usando o informações clássicas da série como tendências e valores passados.  Dado o gráfico dos valores realizados e previstos, não parece um modelo ruim, assim, modelos avançados como ensemble de modelos estatísticos e modelos de machine learning devem gerar previsões muito satisfatórias.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/lm_classic.png)

A previsão pelo ensemble de modelos é um mercado em que se recupera da queda que dos últimos dois meses observados, asism os modelos parecem captar a tendência da série do do mercado de crédito.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/ts.png)

