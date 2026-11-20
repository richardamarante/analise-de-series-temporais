# Lista 2: Processos estocásticos em R (14/08/2026)

## Objetivo

Simular ruído branco, passeio aleatório e AR(1), calcular manualmente a média, as autocovariâncias e a função de autocorrelação (FAC), comparando os resultados com `acf()` e com a FAC teórica do AR(1). Os processos também são representados em gráficos com ggplot2.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | implementação procedural sem divisão em funções, com gráficos base do R |
| `parametros.json` | 2 | parâmetros das simulações, gráficos e análise de eficiência |
| `scripts/config.R` | 2 | leitura dos parâmetros e definição da semente |
| `scripts/dados.R` | 2 | simulação dos três processos e acúmulo dos choques |
| `scripts/processamento.R` | 2 | média, autocovariâncias e FACs empírica e teórica |
| `scripts/visualizacao.R` | 2 e 4 | criação, exibição e salvamento dos gráficos |
| `main.R` | 3 | coordenação das etapas, comparações e resultados no console |
| `ex4_eficiencia.R` | 4 | comparação entre loops e vetorização, tempos e gráfico |
| `graficos/` | 2 e 4 | gráficos dos processos e da análise de eficiência |

A modelagem do Exercício 3, com a arquitetura, o fluxo de dados e a sequência de chamadas, está em `modelagem.md`.

## Como executar

É necessário ter o R e os pacotes ggplot2 e jsonlite instalados. No RStudio, com a pasta `Lista 2` definida como diretório de trabalho, a execução pode ser feita com os comandos abaixo.

```r
install.packages(c("ggplot2", "jsonlite"))
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` lê `parametros.json` e executa as etapas de configuração, simulação, cálculo, análise e exibição, usando a semente 123, `n = 200`, `max_lag = 20`, `sigma2 = 1` e `phi = 0.7`. Os 11 gráficos gerados são exibidos e salvos em `graficos/`, nas pastas `ruido_branco/`, `passeio_aleatorio/`, `ar1/` e `eficiencia/`. O Exercício 1 também pode ser executado separadamente com `source("ex1_implementacao_procedural.R")`.

## Validação dos resultados

Com a semente 123 e a simulação na ordem ruído branco, passeio aleatório e AR(1), as médias empíricas foram -0.0086, 7.2628 e 0.0895, respectivamente. Para verificar os cálculos manuais, as FACs dos três processos foram comparadas com `acf()`, e a maior diferença encontrada foi zero em todos os casos.

No AR(1), com φ = 0.7, a FAC teórica é dada por ρ(h) = φ^h. A tabela mostra a comparação nas duas primeiras defasagens, em que os valores empíricos ficaram abaixo dos teóricos.

| Defasagem | FAC empírica | FAC teórica |
|---|---:|---:|
| 1 | 0.6190 | 0.7000 |
| 2 | 0.3959 | 0.4900 |

A maior diferença absoluta entre as FACs empírica e teórica foi 0.1147, na defasagem 20, o que reflete a variação amostral da série simulada. Os gráficos mostram o comportamento esperado dos três processos, com autocorrelações próximas de zero no ruído branco, persistência no passeio aleatório e decaimento no AR(1). As FACs empíricas são apresentadas com limites de confiança de 95%, calculados por ±1.96/√n.

## Análise de eficiência (Exercício 4.3)

O `ex4_eficiencia.R` compara as implementações com loops e versões vetorizadas do passeio aleatório e das autocovariâncias. No passeio, a acumulação dos choques é substituída por `cumsum(eps)`, enquanto na autocovariância a média passa a ser calculada uma única vez e os produtos dos desvios são somados com `sum()`. O loop das defasagens é mantido, assim como a recorrência do AR(1), que depende do valor calculado no instante anterior.

O vetor `eps` é gerado fora da medição para que seu tempo de geração não entre na comparação. Foram usadas séries de `n = 100000`, com `max_lag = 20` e cinco repetições, obtendo os seguintes tempos.

| Operação | Loop `for` | Vetorizada | Ganho |
|---|---:|---:|---:|
| Passeio aleatório | 0.1087 s | 0.0036 s | 30.141x |
| Autocovariâncias | 2.9927 s | 0.3133 s | 9.552x |

| Operação | Maior diferença absoluta | Maior diferença relativa | `all.equal()` |
|---|---:|---:|---|
| Passeio aleatório | 2.9 × 10⁻¹² | 8.28 × 10⁻¹⁵ | TRUE |
| Autocovariâncias | 2.8 × 10⁻¹⁰ | 1.37 × 10⁻¹⁴ | TRUE |
| FAC empírica | 2.23 × 10⁻¹⁴ | | TRUE |

As versões vetorizadas foram mais rápidas e produziram resultados equivalentes aos cálculos com loops, com diferenças apenas de arredondamento. A redução no tempo foi maior no passeio aleatório, embora também tenha sido significativa nas autocovariâncias. O gráfico de eficiência compara os tempos para `n = 1000`, `10000`, `50000` e `100000`, mostrando como a diferença entre as implementações aumenta com o tamanho da série.
