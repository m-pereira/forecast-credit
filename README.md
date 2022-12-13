
# forecast-credit

<!-- badges: start -->
<!-- badges: end -->

Cada vez mais empresas previsam de previsões em centenas e até milhares de produtos, entretanto fazer previsões com qualidade não é tão fácil. Passei por situações assim algumas vezes e acho duas abordagens muito interessantes para essa atividade: `Nested Forecast` que cria modelo por entidade, no caso unidade federativa e o uso de ML em modelos globais que cria um modelo e usa a informação da UF para a previsão. 

Nesse projeto <https://github.com/m-pereira/forecast-credit> aplico de forma prática esses conceitos, realizando previsões sobre o mercado de crédito para o próximo ano e explicando passo a passo os prós e contras de cada decisão tomada. 

Para usar esse repositório você precisar instalar alguns pacotes:
```
install.packages("tidyverse","timetk","tidymodels","modeltime",
                 "httr","rvest","tictoc","modeltime.ensemble",
                 "tsibble", "fastDummies","tidymodels","future",
                 "doFuture","plotly")
```


Para importar os dados basta executar o script `wrangling.R`, já organizei um pipeline para importação e tratamento dos dados por funções que estão disponíveis em `functions.R`. A etapa de exploração pode ser vista em `exploring.R`, enquanto o modelo nested (um modelo por UF) em `nested_forecast.R`, o modelo de Machine Learning em `ML.R`. Depois ainda criei versões dos modelos com o subscrito `_filtered`, isso significa que filtrei as duas últimas observações, que na etapa de exploração identificamos como erradas, resolvi manter os dados na primeira tentativa por curiosidade, isso permite uma comparação interessante.


A série que analisada é o mercado de crédito, o volume de crédito é uma série que apresenta uma tendência clara, isso é normal, uma vez que a série não é deflacionada, então é natural que anualmente o mercado seja nominalmente maior que o ano anterior. Devido essa característica forte na série, é provável que modelos estatísticos clássicos sejam úteis para estimar o mercado de crédito futuro. A queda abrupta dos últimos dois meses se destacam, entretanto é algo esperado, pois é comum as informações do ESTBAN serem divulgadas faltando alguma instituição financeira, assim a queda os dois últimos valores são viesados para baixo, o correto seria remover as duas obervações, mas vou manter nesse projeto por curiosidade sobre como vai afetar os modelos estimados.


![alt text](https://github.com/m-pereira/forecast-credit/blob/main/ts.png)

A série também apresenta correlações com lags passados, indicando que informações passadas podem ajudar a prever a séie futura, utilizando a metodologia Box e Jenkins em modelos ARIMA. Com um decaimento exponencial da ACF e informações estatisticamente significativas na PACF.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/acf_pacf.png)

Ainda explorando o mercado como um todo, estima-se um modelo linear simples, usando o informações clássicas da série como tendências e valores passados.  Dado o gráfico dos valores realizados e previstos, não parece um modelo ruim, assim, modelos avançados como ensemble de modelos estatísticos e modelos de machine learning devem gerar previsões muito satisfatórias.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/lm_classic.png)

A previsão pelo ensemble de modelos por UF, apresenta  um mercado em que se recupera da queda que dos últimos dois meses observados, assim os modelos parecem captar a tendência da série do mercado de crédito. Mas os dados desajustados chegam a afetar bastante o modelo, sem as últimas observações, os modelos captariam perfeitamente a tendência.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/forecast-ensemble.png)


Já o modelo de previsão por Ensemble de Machine learning (Random Forest, XGBOOST e ARIMA + XGBOOST). Resumidamente são criados 3 modelos, que são tunados e agregados por média. Na prática os modelos de machine learning performaram muito mal com essas duas últimas observações viesadas.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/forecast-ensemble.png)

Para efeitos de comparação fiz os mesmos modelos sem as duas últimas observações. O modelo de Machine Learning capta perfeitamente a tendência dos dados, replicando pro futuro. No caso, é uma boa projeção.

![alt text](https://github.com/m-pereira/forecast-credit/blob/main/ML_f_filtered.png)


Por questões bônus, recordei o quanto o mercado de crédito é concentrado segundo os dados do ESBTAN. Isso pode ser visto na figura abaixo, a UF de São Paulo (35) concentra cerca de 60% do mercado de crédito, a segunda UF que mais concentra crédito é Rio de Janeiro (31) que concentra "apenas" 7,6% desse mercado. Isso afeta bastante a projeção agregada, pois exige que tenhamos atenção sobre como o modelo prevê os dados para essa UF.


![alt text](https://github.com/m-pereira/forecast-credit/blob/main/conc.png)

