# Lista 4: Decomposição clássica de séries temporais em R (28/08/2026)

## Objetivo

Simular uma série com tendência quadrática e sazonalidade, estimar os coeficientes por OLS com equações normais, analisar os resíduos, testar hipóteses e prever 12 períodos. A sazonalidade é ajustada por Fourier e por dummies, com gráficos feitos em ggplot2.

$$X_t=\beta_0+\beta_1t+\beta_2t^2+\sum_{k=1}^{K}\left[\alpha_k\cos(2\pi kt/s)+b_k\operatorname{sen}(2\pi kt/s)\right]+\varepsilon_t,\quad\varepsilon_t\sim N(0,\sigma^2).$$

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | simulação, OLS, testes, previsão e gráficos em R base |
| `parametros.json` | 2 | parâmetros da simulação, dos gráficos e da eficiência |
| `scripts/config.R` | 2 | `ler_parametros()`, semente e vetores de tempo e coeficientes |
| `scripts/dados.R` | 2 | matrizes de tendência, Fourier e dummies; simulação |
| `scripts/processamento.R` | 2 | média móvel, OLS, diagnósticos, testes e previsão |
| `scripts/visualizacao.R` | 2 e 4 | gráficos, tema e exportação por `salvar_grafico()` |
| `main.R` | 3 | execução das etapas e geração de dez gráficos |
| `ex4_eficiencia.R` | 4 | validação com `lm()`, tempos e gráfico de eficiência |
| `README.md` | | execução e resultados |
| `modelagem.md` | 3 | arquitetura, fluxo, chamadas e paradigma procedural |

A modelagem do Exercício 3 está em [modelagem.md](modelagem.md).

## Como executar

No RStudio, usar a pasta da lista como diretório de trabalho, instalar `ggplot2` e `jsonlite` se necessário e executar:

```r
install.packages(c("ggplot2", "jsonlite"))
source("main.R")
source("ex4_eficiencia.R")
```

O Exercício 1 roda separadamente com `source("ex1_implementacao_procedural.R")`. O `main.R` mostra os resultados e salva dez PNGs; `ex4_eficiencia.R` salva mais um.

Em `parametros.json`: `seed = 42`, `n_anos = 10`, `s = 12`, `sigma = 5`, `grau_tendencia = 2`, `beta_tendencia = (2, 0.05, 0.001)`, `alpha = (2, 1)`, `beta_h = (-1, 0.5)`, `K = 2`, `q = 5`, `max_lag_box = 12`, `n_futuro = 12`, `n_eficiencia = 200000` e `repeticoes_eficiencia = 10`. São 120 observações, variância do ruído 25 e média móvel centrada de 11 pontos. O arquivo também define `n_grafico_eficiencia`.

## Validação dos resultados

A média da série simulada foi 10.0334. O ajuste de Fourier usa três colunas de tendência e quatro sazonais; dummies usa três de tendência e 11 indicadoras (mês 1 como referência). OLS estima 7 e 14 parâmetros, respectivamente. `ajustar_ols()` resolve `solve(XtX, Xty)`, com produtos calculados por `calcular_XtX()` e `calcular_Xty()`.

| Coeficiente de Fourier | Verdadeiro | Estimado | t | Valor-p |
|---|---:|---:|---:|---:|
| Intercepto | 2 | 2.8646 | 1.9533 | 0.0533 |
| Tendência linear | 0.05 | 0.0236 | 0.4227 | 0.6733 |
| Tendência quadrática | 0.001 | 0.0012 | 2.6396 | 0.0095 |
| Cosseno 1 | 2 | 1.3523 | 1.9913 | 0.0489 |
| Seno 1 | -1 | -1.6327 | -2.3978 | 0.0181 |
| Cosseno 2 | 1 | 0.7060 | 1.0397 | 0.3007 |
| Seno 2 | 0.5 | 0.0716 | 0.1054 | 0.9162 |

`teste_t_coeficientes()` testa $H_0:\beta_j=0$. A 5%, rejeita a hipótese nula para os coeficientes 3, 4 e 5. No teste de Box-Pierce dos resíduos de Fourier, com 12 defasagens, $Q=8.3199$ e valor-p 0.7597: não se rejeita a ausência de autocorrelação. A FAC usa bandas de $\pm1.96/\sqrt{n}$.

| Medida | Fourier | Dummies |
|---|---:|---:|
| Média dos resíduos | 0 | 0 |
| Variância dos resíduos | 26.2615 | 24.5124 |
| RMSE do ajuste | 5.1032 | 4.9303 |
| R² | 0.5979 | 0.6247 |
| RMSE da previsão | 0.8775 | 1.5812 |

As dummies ajustaram melhor a amostra, com mais parâmetros; Fourier previu melhor o sinal verdadeiro sem ruído, usado para gerar a série. Para $t=121$, as previsões foram 23.7829 (Fourier) e 24.3790 (dummies), contra 24.8561 verdadeiro. Para $t=132$, foram 28.6178 e 30.6464, contra 29.0240. O horizonte foi $t=121:132$.

Os coeficientes manuais e de `lm()` tiveram maior diferença arredondada a quatro casas igual a 0. `all.equal()` retornou `TRUE` para coeficientes e valores ajustados.

## Análise de eficiência (Exercício 4.3)

`calcular_XtX()` usa três loops, com custo aproximado $np^2$; `calcular_Xty()` e `multiplicar_matriz_vetor()` usam dois, com custo $np$. O `ex4_eficiencia.R` compara os loops com `crossprod()`, `t(X) %*% X`, `t(X) %*% y` e `X %*% beta`, mantendo `solve()` no OLS.

| Operação vetorizada | Loops | Vetorizada | Ganho |
|---|---:|---:|---:|
| `crossprod(X)` | 14.88 s | 0.19 s | 78.3x |
| `t(X) %*% X` | 14.88 s | 0.40 s | 37.2x |
| `crossprod(X, y)` | 1.99 s | 0.06 s | 33.2x |
| `t(X) %*% y` | 1.99 s | 0.17 s | 11.7x |
| `X %*% beta` | 2.76 s | 0.06 s | 46.0x |

Os tempos são de dez repetições com matriz de 200000 × 7. As cinco comparações de coeficientes entre loops, métodos vetorizados e `lm()` retornaram `TRUE`; as comparações numéricas das operações também retornaram `TRUE`, com maior diferença relativa apresentada como 0. O gráfico usa amostras de 1000, 10000, 50000 e 200000 linhas. Os tempos variam conforme a máquina.

## Gráficos e ggplot2 (Exercício 4.2)

O ggplot2 padroniza tema e eixos com `tema_graficos()` e `ajustar_eixos()`. `plot_decomposicao()` usa `facet_wrap()` para mostrar série, tendência, sazonalidade e resíduos em quatro painéis; os outros gráficos comparam ajustes, previsões, FAC e tempos. `salvar_grafico()` exporta os 11 PNGs com `ggsave()`, 300 dpi, dimensões 10 × 12 na decomposição e 10 × 6 nos demais.

- `graficos/decomposicao/`: `01_decomposicao_aditiva.png`, `02_serie_observada_ajustada.png`, `03_tendencia_estimada_real.png`, `04_sazonalidade_estimada_real.png`, `05_serie_media_movel_tendencia.png`, `06_sazonalidade_mensal_fourier_dummies.png`.
- `graficos/previsao/`: `01_previsao_fora_amostra.png`.
- `graficos/residuos/`: `01_serie_temporal_residuos.png`, `02_funcao_autocorrelacao_residuos.png`, `03_histograma_residuos.png`.
- `graficos/eficiencia/`: `01_tempo_loop_vetorizada.png`.
