# Lista 4: Decomposição clássica de séries temporais em R (28/08/2026)

## Objetivo

Simular uma série temporal com tendência quadrática e sazonalidade, estimar os coeficientes por mínimos quadrados ordinários (OLS) com as equações normais, analisar os resíduos e prever 12 períodos. A sazonalidade é representada por Fourier e por dummies mensais, com os resultados apresentados no console e em gráficos feitos com ggplot2.

O modelo é Xₜ = β₀ + β₁t + β₂t² + Σₖ [αₖ·cos(2πkt/s) + bₖ·sen(2πkt/s)] + εₜ, com εₜ ~ N(0, σ²) e k = 1, …, K.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | simulação, OLS, previsão e gráficos em R base |
| `parametros.json` | 2 | parâmetros da simulação, dos gráficos e da eficiência |
| `scripts/config.R` | 2 | leitura dos parâmetros e configuração da simulação |
| `scripts/dados.R` | 2 | matrizes de tendência, Fourier e dummies, além da simulação |
| `scripts/processamento.R` | 2 | média móvel, OLS, diagnósticos, avaliação e previsão |
| `scripts/visualizacao.R` | 2 e 4 | gráficos com ggplot2 e exportação |
| `main.R` | 3 | execução das etapas e geração de 12 gráficos |
| `ex4_eficiencia.R` | 4 | validação com `lm()`, comparação de tempos e gráfico de eficiência |
| `README.md` | | execução e resultados |
| `modelagem.md` | 3 | arquitetura, fluxo de dados, chamadas e paradigma procedural |

A arquitetura e o funcionamento do Exercício 3 estão descritos em [modelagem.md](modelagem.md).

## Como executar

No RStudio, com a pasta da lista como diretório de trabalho, instalar os pacotes ggplot2 e jsonlite se necessário e executar os scripts.

```r
install.packages(c("ggplot2", "jsonlite"))
source("main.R")
source("ex4_eficiencia.R")
```

O Exercício 1 pode ser executado separadamente com `source("ex1_implementacao_procedural.R")`. O `main.R` apresenta os resultados no console e salva 12 gráficos, enquanto o script de eficiência gera mais um. Os parâmetros ficam em `parametros.json`, que define a semente 42, dez anos de observações mensais, desvio do ruído igual a 5 e dois harmônicos de Fourier, além das configurações de previsão, divisão temporal e eficiência.

## Fourier x dummies

No Exercício 1.3, a sazonalidade de Fourier é representada por 2K colunas de senos e cossenos, com K < s/2, o que permite ajustar um padrão periódico suave com poucos parâmetros. As dummies usam s − 1 indicadoras e deixam cada mês com seu próprio efeito, retirando o mês de referência para evitar dependência linear com o intercepto. Com s = 12 e K = 2, os modelos têm três coeficientes de tendência e, respectivamente, quatro e 11 coeficientes sazonais, totalizando 7 parâmetros para Fourier e 14 para dummies.

## Validação dos resultados

Com a semente 42, foram simuladas 120 observações, com média 10.0334 e variância do ruído igual a 25. A média móvel centrada utiliza 11 pontos, enquanto o OLS resolve as equações normais por `solve(XtX, Xty)`, com os produtos matriciais calculados por loops. Os coeficientes estimados de Fourier, na ordem intercepto, tendência linear, tendência quadrática, cosseno 1, seno 1, cosseno 2 e seno 2, foram 2.8646, 0.0236, 0.0012, 1.3523, -1.6327, 0.7060 e 0.0716, diante dos valores verdadeiros 2, 0.05, 0.001, 2, -1, 1 e 0.5.

| Medida | Fourier | Dummies |
|---|---:|---:|
| Média dos resíduos | 0 | 0 |
| Variância dos resíduos | 26.2615 | 24.5124 |
| RMSE do ajuste | 5.1032 | 4.9303 |
| R² | 0.5979 | 0.6247 |
| RMSE da previsão | 0.8775 | 1.5812 |

As dummies tiveram melhor ajuste dentro da amostra, com R² de 0.6247 contra 0.5979, o que também reflete o maior número de parâmetros. Fourier apresentou menor RMSE na previsão do sinal verdadeiro sem ruído, passando de 0.8775 contra 1.5812 das dummies no horizonte de t = 121 a 132. No primeiro período, as previsões foram 23.7829 e 24.3790, diante do valor verdadeiro 24.8561, enquanto no último foram 28.6178 e 30.6464, diante de 29.0240.

A FAC dos resíduos foi analisada com bandas de ±1.96/√n. Os coeficientes calculados manualmente também foram comparados com `lm()`, com maior diferença arredondada a quatro casas igual a zero, e `all.equal()` retornou `TRUE` tanto para os coeficientes quanto para os valores ajustados.

## Treino, validação e teste

A divisão foi feita por data, sem embaralhamento, com 60% das observações para treino, de t = 1 a 72, 20% para validação, de t = 73 a 96, e 20% para teste, de t = 97 a 120. Os dois métodos foram ajustados no treino e comparados pelo RMSE da validação, que definiu a escolha antes de qualquer avaliação no teste. Depois, ambos foram reajustados com treino e validação para prever o conjunto de teste, calculando RMSE, MAE e R², com a média dos dados de ajuste como referência para o R².

| Conjunto | Método | RMSE | MAE | R² |
|---|---|---:|---:|---:|
| Treino | Fourier | 5.2815 | 4.0663 | 0.3270 |
| Treino | Dummies | 5.0465 | 3.9275 | 0.3856 |
| Validação | Fourier | 7.5578 | 6.1116 | 0.3469 |
| Validação | Dummies | 7.6130 | 5.8367 | 0.3373 |
| Teste | Fourier | 6.3061 | 5.4237 | 0.7407 |
| Teste | Dummies | 6.3688 | 5.4176 | 0.7355 |

Embora as dummies tenham apresentado menor RMSE no treino, Fourier foi escolhido pelo resultado da validação, com 7.5578 contra 7.6130, e também obteve menor RMSE no teste, com 6.3061 contra 6.3688. Os gráficos em `graficos/previsao/` mostram as divisões temporais, as previsões de cada método e a comparação dos RMSEs nos três conjuntos.

## Análise de eficiência

No Exercício 4.3, `calcular_XtX()` utiliza três loops aninhados, com custo aproximado de n·p², enquanto `calcular_Xty()` e a multiplicação de matriz por vetor usam dois loops, com custo n·p. A versão vetorizada substitui os produtos por `crossprod()` ou `%*%`, que executam as operações em código compilado, mantendo `solve()` na estimação dos coeficientes. Foram realizadas dez repetições com uma matriz de 200000 linhas e sete colunas.

| Operação vetorizada | Loops | Vetorizada | Ganho |
|---|---:|---:|---:|
| `crossprod(X)` | 14.88 s | 0.19 s | 78.3x |
| `t(X) %*% X` | 14.88 s | 0.40 s | 37.2x |
| `crossprod(X, y)` | 1.99 s | 0.06 s | 33.2x |
| `t(X) %*% y` | 1.99 s | 0.17 s | 11.7x |
| `X %*% beta` | 2.76 s | 0.06 s | 46.0x |

As cinco comparações dos coeficientes entre loops, operações vetorizadas e `lm()` retornaram `TRUE`, assim como as comparações numéricas das operações, cuja maior diferença relativa apresentada foi zero. Os ganhos variaram de 11.7 a 78.3 vezes e mostram a vantagem da vetorização em matrizes grandes, embora os tempos dependam da máquina utilizada. O gráfico em `graficos/eficiencia/` compara as implementações para 1000, 10000, 50000 e 200000 observações.

## Gráficos e ggplot2

O ggplot2 foi utilizado no Exercício 4.2 para manter a mesma apresentação entre os gráficos, facilitar a comparação das séries e organizar a decomposição aditiva em quatro painéis com `facet_wrap()`. Os 13 PNGs foram distribuídos entre `graficos/decomposicao/`, `graficos/previsao/`, `graficos/residuos/` e `graficos/eficiencia/`, mostrando os componentes estimados e verdadeiros, a média móvel, os ajustes, as previsões, os diagnósticos dos resíduos e os tempos de execução.
