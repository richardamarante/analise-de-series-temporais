# Lista 3: Série temporal com tendência polinomial em R (21/08/2026)

## Objetivo

Simular uma série com tendência polinomial (Xₜ = β₀ + β₁t + β₂t² + εₜ), calcular a média móvel centrada, a média amostral, as autocovariâncias e a FAC empírica, comparar a FAC com `acf()` e gerar gráficos com ggplot2.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | implementação procedural documentada |
| `parametros.json` | 2 | parâmetros da simulação, eficiência e gráficos |
| `scripts/config.R` | 2 | leitura dos parâmetros, semente, tempo e matriz X |
| `scripts/dados.R` | 2 | tendência, ruído branco e série simulada |
| `scripts/processamento.R` | 2 | média móvel, média, autocovariâncias e FAC |
| `scripts/visualizacao.R` | 2 | construção e salvamento dos gráficos |
| `main.R` | 3 | execução do pipeline e comparação com `acf()` |
| `modelagem.md` | 3 | modelagem dos itens 3.2 e 3.3 |
| `ex4_eficiencia.R` | 4 | comparação entre loops e vetorização |
| `graficos/serie_tendencia/` | 2 e 3 | cinco gráficos do pipeline |
| `graficos/eficiencia/` | 4 | gráfico dos tempos de execução |

## Como executar

Pré-requisitos: R e os pacotes ggplot2, jsonlite e patchwork. No RStudio, com a pasta da lista como diretório de trabalho:

```r
install.packages(c("ggplot2", "jsonlite", "patchwork"))
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` lê `parametros.json` com `ler_parametros()`, imprime os resultados e gera cinco gráficos em `graficos/serie_tendencia/`:

| Arquivo | Conteúdo |
|---|---|
| `01_serie_temporal_tendencia.png` | série com tendência polinomial |
| `02_serie_temporal_media_movel_tendencia.png` | série, média móvel e tendência verdadeira |
| `03_funcao_autocorrelacao_empirica_tendencia.png` | FAC com limites de 95% |
| `04_comparacao_fac_empirica_acf_r_tendencia.png` | FAC manual e `acf()` |
| `05_grafico_integrado_tendencia.png` | série, média móvel, tendência e FAC |

Com `salvar_graficos = true`, os PNGs são salvos em 10 × 6 polegadas e 300 dpi. O gráfico integrado usa fator de altura 1.6. O gráfico 02 tem o título "Série Temporal com Média Móvel e Tendência". O `ex4_eficiencia.R` gera `graficos/eficiencia/01_tempo_loop_vetorizada.png`.

A modelagem do Exercício 3 está em `modelagem.md`.

## Validação dos resultados

1. **Parâmetros:** `seed = 123`, `n = 90`, `sigma = 15`, `q = 3`, `max_lag = 20` e `beta = (2, 0.01, 0.01)`. A tendência verdadeira é 2 + 0.01t + 0.01t², com ruído branco normal de desvio padrão 15.
2. **Estatísticas:** média amostral = 30.6849 e autocovariância empírica na defasagem 1 = 588.9894, calculada com divisor n.
3. **Média móvel:** janela de 2q + 1 = 7 pontos, com 84 médias, de t = 4 até t = 87. A tendência é calculada por `calcular_tendencia()` como X·beta, sem ruído.
4. **FAC:** valores 1 na defasagem 0, 0.7285 na defasagem 1 e 0.2910 na defasagem 20. A FAC permanece positiva, com queda geral, como esperado para a série com tendência não estacionária.
5. **Comparação:** `acf(Xt, lag.max = 20, plot = FALSE)` e FAC manual apresentam maior diferença absoluta 0, com `all.equal()` igual a `TRUE`.

## Análise de eficiência (Exercício 4.3)

O `ex4_eficiencia.R` compara loops `for` e versões vetorizadas em quatro operações. A matriz X usa `criar_matriz_regressao_vetorizada()` com `cbind(1, t, t^2)`; a simulação usa `simular_serie_com_tendencia_vetorizada()` com `X %*% beta` e `rnorm()`; a média móvel usa `calcular_media_movel_vetorizada()` com `cumsum()`; e a autocovariância usa `calcular_autocovariancia_empirica_vetorizada()` com `mean()`, produtos de vetores e `sum()`. A função `calcular_autocovariancias_vetorizada()` aplica esse cálculo às defasagens com `sapply()`.

As funções `medir_tempo()` e `medir_tempos_por_n()` medem os tempos com `system.time()`, `formatar_diferenca()` formata as diferenças relativas e `plot_tempo_eficiencia()` gera o gráfico para n = 1000, 25000 e 50000.

**Resultados para n = 50000, com 10 repetições por operação:**

| Operação | Loops `for` | Vetorizada | Ganho | Iguais | Maior diferença relativa |
|---|---:|---:|---:|---|---:|
| Matriz de regressão | 0.31 s | 0.05 s | 6.2x | TRUE | 0 |
| Simulação da série | 2.46 s | 0.06 s | 41x | TRUE | 0 |
| Média móvel | 0.75 s | 0.05 s | 15x | TRUE | 3.47e-13 |
| Autocovariâncias | 3.34 s | 0.47 s | 7.1x | TRUE | 9.28e-15 |

Os tempos correspondem ao total das 10 repetições e podem variar entre execuções. A semente é fixada na comparação da simulação. Todas as operações retornaram `TRUE` em `all.equal()`. A vetorização reduziu os tempos, com maior ganho na simulação da série (41x).
