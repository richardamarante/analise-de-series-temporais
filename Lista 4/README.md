# Lista 4: Decomposição clássica de séries temporais em R (28/08/2026)

## Objetivo

Simular uma série com tendência quadrática e sazonalidade, estimar os coeficientes por OLS com equações normais, analisar os resíduos e prever 12 períodos. A sazonalidade é ajustada por Fourier e por dummies, com gráficos feitos em ggplot2.

Modelo: Xₜ = β₀ + β₁t + β₂t² + Σₖ [αₖ·cos(2πkt/s) + bₖ·sen(2πkt/s)] + εₜ, com εₜ ~ N(0, σ²) e k = 1, …, K.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | simulação, OLS, previsão e gráficos em R base |
| `parametros.json` | 2 | parâmetros da simulação, dos gráficos e da eficiência |
| `scripts/config.R` | 2 | `ler_parametros()`, semente e vetores de tempo e coeficientes |
| `scripts/dados.R` | 2 | matrizes de tendência, Fourier e dummies; simulação |
| `scripts/processamento.R` | 2 | média móvel, OLS, diagnósticos, avaliação e previsão |
| `scripts/visualizacao.R` | 2 e 4 | gráficos, tema e exportação por `salvar_grafico()` |
| `main.R` | 3 | execução das etapas e geração de 12 gráficos |
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

O Exercício 1 roda separadamente com `source("ex1_implementacao_procedural.R")`. O `main.R` mostra os resultados e salva 12 PNGs; `ex4_eficiencia.R` salva mais um.

Em `parametros.json`: `seed = 42`, `n_anos = 10`, `s = 12`, `sigma = 5`, `grau_tendencia = 2`, `beta_tendencia = (2, 0.05, 0.001)`, `alpha = (2, 1)`, `beta_h = (-1, 0.5)`, `K = 2`, `q = 5`, `n_futuro = 12`, `n_eficiencia = 200000` e `repeticoes_eficiencia = 10`. São 120 observações, variância do ruído 25 e média móvel centrada de 11 pontos. O arquivo também define `n_grafico_eficiencia`, `proporcao_treino` e `proporcao_validacao`.

## Validação dos resultados

A média da série simulada foi 10.0334. O ajuste de Fourier usa três colunas de tendência e quatro sazonais; dummies usa três de tendência e 11 indicadoras (mês 1 como referência). OLS estima 7 e 14 parâmetros, respectivamente. `ajustar_ols()` resolve `solve(XtX, Xty)`, com produtos calculados por `calcular_XtX()` e `calcular_Xty()`.

| Coeficiente de Fourier | Verdadeiro | Estimado |
| --- | ---: | ---: |
| Intercepto | 2 | 2.8646 |
| Tendência linear | 0.05 | 0.0236 |
| Tendência quadrática | 0.001 | 0.0012 |
| Cosseno 1 | 2 | 1.3523 |
| Seno 1 | -1 | -1.6327 |
| Cosseno 2 | 1 | 0.7060 |
| Seno 2 | 0.5 | 0.0716 |

A FAC usa bandas de ±1.96/√n.

| Medida | Fourier | Dummies |
|---|---:|---:|
| Média dos resíduos | 0 | 0 |
| Variância dos resíduos | 26.2615 | 24.5124 |
| RMSE do ajuste | 5.1032 | 4.9303 |
| R² | 0.5979 | 0.6247 |
| RMSE da previsão | 0.8775 | 1.5812 |

As dummies ajustaram melhor a amostra, com mais parâmetros; Fourier previu melhor o sinal verdadeiro sem ruído, usado para gerar a série. Para t = 121, as previsões foram 23.7829 (Fourier) e 24.3790 (dummies), contra 24.8561 verdadeiro. Para t = 132, foram 28.6178 e 30.6464, contra 29.0240. O horizonte foi t = 121 a 132.

Os coeficientes manuais e de `lm()` tiveram maior diferença arredondada a quatro casas igual a 0. `all.equal()` retornou `TRUE` para coeficientes e valores ajustados.

## Treino, validação e teste

A série foi dividida por data, sem embaralhamento, em treino (60%, t = 1 a 72), validação (20%, t = 73 a 96) e teste (20%, t = 97 a 120). Fourier e dummies foram ajustados no treino e avaliados na validação, que definiu o método com menor RMSE. Depois, os dois modelos foram reajustados com treino e validação para prever o teste, usado apenas após a escolha. A função `avaliar_previsao()` calcula RMSE, MAE e R² com loops, usando a média dos dados de ajuste como referência para o R².

| Conjunto | Método | RMSE | MAE | R² |
|---|---|---:|---:|---:|
| Treino | Fourier | 5.2815 | 4.0663 | 0.3270 |
| Treino | Dummies | 5.0465 | 3.9275 | 0.3856 |
| Validação | Fourier | 7.5578 | 6.1116 | 0.3469 |
| Validação | Dummies | 7.6130 | 5.8367 | 0.3373 |
| Teste | Fourier | 6.3061 | 5.4237 | 0.7407 |
| Teste | Dummies | 6.3688 | 5.4176 | 0.7355 |

Dummies teve menor RMSE no treino, mas Fourier foi escolhido pelo menor RMSE na validação (7.5578 contra 7.6130) e também apresentou menor RMSE no teste (6.3061 contra 6.3688). Os gráficos `02_previsao_validacao_teste.png` e `03_rmse_treino_validacao_teste.png`, em `graficos/previsao/`, mostram os cortes temporais com as previsões e os RMSEs por método e conjunto.

## Análise de eficiência (Exercício 4.3)

`calcular_XtX()` usa três loops, com custo aproximado n·p²; `calcular_Xty()` e `multiplicar_matriz_vetor()` usam dois, com custo n·p. O `ex4_eficiencia.R` compara os loops com `crossprod()`, `t(X) %*% X`, `t(X) %*% y` e `X %*% beta`, mantendo `solve()` no OLS.

| Operação vetorizada | Loops | Vetorizada | Ganho |
|---|---:|---:|---:|
| `crossprod(X)` | 14.88 s | 0.19 s | 78.3x |
| `t(X) %*% X` | 14.88 s | 0.40 s | 37.2x |
| `crossprod(X, y)` | 1.99 s | 0.06 s | 33.2x |
| `t(X) %*% y` | 1.99 s | 0.17 s | 11.7x |
| `X %*% beta` | 2.76 s | 0.06 s | 46.0x |

Os tempos são de dez repetições com matriz de 200000 × 7. As cinco comparações de coeficientes entre loops, métodos vetorizados e `lm()` retornaram `TRUE`; as comparações numéricas das operações também retornaram `TRUE`, com maior diferença relativa apresentada como 0. O gráfico usa amostras de 1000, 10000, 50000 e 200000 linhas. Os tempos variam conforme a máquina.

## Gráficos e ggplot2 (Exercício 4.2)

O ggplot2 padroniza tema e eixos com `tema_graficos()` e `ajustar_eixos()`. `plot_decomposicao()` usa `facet_wrap()` para mostrar série, tendência, sazonalidade e resíduos em quatro painéis; os outros gráficos comparam ajustes, previsões, FAC e tempos. `salvar_grafico()` exporta os 13 PNGs com `ggsave()`, 300 dpi, dimensões 10 × 12 na decomposição e 10 × 6 nos demais.

- `graficos/decomposicao/`: `01_decomposicao_aditiva.png`, `02_serie_observada_ajustada.png`, `03_tendencia_estimada_real.png`, `04_sazonalidade_estimada_real.png`, `05_serie_media_movel_tendencia.png`, `06_sazonalidade_mensal_fourier_dummies.png`.
- `graficos/previsao/`: `01_previsao_fora_amostra.png`, `02_previsao_validacao_teste.png`, `03_rmse_treino_validacao_teste.png`.
- `graficos/residuos/`: `01_serie_temporal_residuos.png`, `02_funcao_autocorrelacao_residuos.png`, `03_histograma_residuos.png`.
- `graficos/eficiencia/`: `01_tempo_loop_vetorizada.png`.
