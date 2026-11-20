# Lista 2: Processos estocásticos em R (14/08/2026)

## Objetivo

Simular ruído branco, passeio aleatório e AR(1), calcular manualmente média, autocovariâncias e função de autocorrelação (FAC), comparar os resultados com `acf()` e com a FAC teórica do AR(1), e gerar gráficos com ggplot2.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | implementação procedural sem divisão em funções, com gráficos base do R |
| `parametros.json` | 2 | parâmetros das simulações, gráficos e análise de eficiência |
| `scripts/config.R` | 2 | `ler_parametros()` e `fixar_semente()` |
| `scripts/dados.R` | 2 | simulação dos três processos e acúmulo dos choques |
| `scripts/processamento.R` | 2 | média, autocovariâncias e FACs empírica e teórica |
| `scripts/visualizacao.R` | 2 e 4 | criação, exibição e salvamento dos gráficos |
| `main.R` | 3 | coordenação das etapas, comparações e saídas no console |
| `ex4_eficiencia.R` | 4 | comparação entre loops e vetorização, tempos e gráfico |
| `graficos/` | 2 e 4 | 11 arquivos PNG por processo e análise de eficiência |

A modelagem do Exercício 3 está em `modelagem.md`.

## Como executar

Pré-requisitos: R e os pacotes ggplot2 e jsonlite. No RStudio, com a pasta `Lista 2` como diretório de trabalho:

```r
install.packages(c("ggplot2", "jsonlite"))
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` lê `parametros.json` e executa configuração, simulação, cálculo, análise e exibição. Os parâmetros principais são `seed = 123`, `n = 200`, `max_lag = 20`, `sigma2 = 1` e `phi = 0.7`. Os gráficos são exibidos e salvos em `graficos/`: três em `ruido_branco/`, três em `passeio_aleatorio/`, quatro em `ar1/` e um em `eficiencia/`.

O Exercício 1 também pode ser executado separadamente com `source("ex1_implementacao_procedural.R")`.

## Validação dos resultados

- **Reprodutibilidade:** com a semente 123 e a ordem ruído branco, passeio aleatório e AR(1), as médias empíricas foram -0.0086, 7.2628 e 0.0895, respectivamente.
- **Comparação com o R:** a maior diferença entre a FAC manual e `acf()` foi 0 nos três processos.
- **Comparação com a teoria:** no AR(1), com φ = 0.7, a FAC teórica é ρ(h) = φ^h. No lag 1, a FAC empírica foi 0.6190 e a teórica 0.7000. No lag 2, foram 0.3959 e 0.4900. A maior diferença absoluta foi 0.1147, no lag 20.
- **Comportamento:** o ruído branco tem autocorrelações próximas de zero fora do lag 0; o passeio aleatório apresenta autocorrelações persistentes; o AR(1) apresenta decaimento conforme a defasagem aumenta.

As FACs empíricas são apresentadas com limites de confiança de 95%, calculados por ±1.96/√n.

## Análise de eficiência (Exercício 4.3)

O `ex4_eficiencia.R` compara `acumular_passeio_aleatorio()` com `cumsum(eps)` e `calcular_autocovariancia_empirica()` com uma versão vetorizada. O vetor `eps` é gerado fora da medição. Na autocovariância, a média é calculada uma vez, e os produtos dos desvios são somados com `sum()`. O loop das defasagens de 0 a `max_lag` é mantido, assim como o loop recursivo do AR(1).

As funções `medir_tempo()`, `medir_tempos_por_n()` e `plot_tempo_eficiencia()` medem os tempos e produzem o gráfico. A comparação principal usou `n = 100000`, `max_lag = 20` e cinco repetições.

| Operação | Loop `for` | Vetorizada | Ganho |
|---|---:|---:|---:|
| Passeio aleatório | 0.1087 s | 0.0036 s | 30.141x |
| Autocovariâncias | 2.9927 s | 0.3133 s | 9.552x |

| Operação | Maior diferença absoluta | Maior diferença relativa | `all.equal()` |
|---|---:|---:|---|
| Passeio aleatório | 2.9 × 10⁻¹² | 8.28 × 10⁻¹⁵ | TRUE |
| Autocovariâncias | 2.8 × 10⁻¹⁰ | 1.37 × 10⁻¹⁴ | TRUE |
| FAC empírica | 2.23 × 10⁻¹⁴ | | TRUE |

As implementações produziram resultados equivalentes, com diferenças de arredondamento. O gráfico `graficos/eficiencia/01_tempo_loop_vetorizada.png` compara `n = 1000`, `10000`, `50000` e `100000`.
