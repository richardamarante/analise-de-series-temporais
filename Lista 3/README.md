# Lista 3: Série temporal com tendência polinomial em R (21/08/2026)

## Objetivo

Simular uma série temporal com tendência polinomial, dada por Xₜ = β₀ + β₁t + β₂t² + εₜ, e calcular a média móvel centrada, a média amostral, as autocovariâncias e a função de autocorrelação empírica (FAC). A função `acf()` do R serve de referência para conferir os resultados, que também são apresentados em gráficos feitos com ggplot2.

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

É necessário ter o R e os pacotes ggplot2, jsonlite e patchwork instalados. No RStudio, com a pasta da lista como diretório de trabalho, os scripts podem ser executados com os seguintes comandos.

```r
install.packages(c("ggplot2", "jsonlite", "patchwork"))
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` lê as configurações de `parametros.json`, executa a simulação e imprime as estatísticas no console. Também gera cinco gráficos em `graficos/serie_tendencia/` para apresentar a série simulada, a média móvel, a tendência verdadeira, a FAC e sua comparação com `acf()`, além de um gráfico integrado. O `ex4_eficiencia.R` faz a comparação dos tempos de execução e salva o resultado em `graficos/eficiencia/`.

A modelagem do Exercício 3 está em `modelagem.md`.

## Validação dos resultados

A simulação utiliza `seed = 123`, `n = 90` e `sigma = 15`, com tendência verdadeira dada por 2 + 0.01t + 0.01t² e ruído branco normal. A média amostral obtida foi 30.6849, enquanto a autocovariância empírica na defasagem 1 foi 588.9894, calculada com divisor n.

Com `q = 3`, a média móvel centrada utiliza uma janela de 7 pontos e produz 84 valores, de t = 4 até t = 87. A FAC foi calculada até a defasagem 20 e apresentou valor 1 na origem, 0.7285 na defasagem 1 e 0.2910 na defasagem 20, permanecendo positiva e com queda geral, como esperado para uma série com tendência não estacionária.

A implementação da FAC manual foi conferida com `acf(Xt, lag.max = 20, plot = FALSE)`. A maior diferença absoluta encontrada foi 0 e o teste com `all.equal()` retornou `TRUE`, o que confirma que os dois cálculos produziram os mesmos valores.

## Análise de eficiência (Exercício 4.3)

O `ex4_eficiencia.R` compara as implementações com loops `for` e as versões vetorizadas em quatro operações. Na vetorização, a matriz de regressão é construída com `cbind()`, enquanto a simulação usa operações matriciais, a média móvel é calculada por somas acumuladas e as autocovariâncias por produtos de vetores, sem percorrer os elementos individualmente em R. Os tempos foram medidos para n = 1000, 25000 e 50000, com 10 repetições por operação.

Os resultados para n = 50000 foram os seguintes.

| Operação | Loops `for` | Vetorizada | Ganho | Iguais | Maior diferença relativa |
|---|---:|---:|---:|---|---:|
| Matriz de regressão | 0.31 s | 0.05 s | 6.2x | TRUE | 0 |
| Simulação da série | 2.46 s | 0.06 s | 41x | TRUE | 0 |
| Média móvel | 0.75 s | 0.05 s | 15x | TRUE | 3.47e-13 |
| Autocovariâncias | 3.34 s | 0.47 s | 7.1x | TRUE | 9.28e-15 |

Os tempos correspondem ao total das 10 repetições e podem variar entre execuções, sendo que a semente foi fixada para comparar as simulações. Todas as operações retornaram `TRUE` em `all.equal()`, com diferenças numéricas nulas ou muito pequenas. A vetorização reduziu o tempo em todos os casos, principalmente na simulação da série, que apresentou ganho de 41 vezes.
