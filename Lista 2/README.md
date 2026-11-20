# Lista 2: Processos estocásticos em R (14/08/2026)

Análise de Séries Temporais (GET00127)

## Objetivo

Simular três processos estocásticos (**ruído branco**, **passeio aleatório** e **AR(1)**), calcular manualmente a média, a autocovariância e a função de autocorrelação (FAC), comparar os resultados com a função `acf()` do R e com a FAC teórica do AR(1), e apresentar os resultados em gráficos com ggplot2.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `Lista 2.pdf` | 1 a 4 | enunciado da lista |
| `ex1_implementacao_procedural.R` | 1 | implementação procedural completa, com entradas, saídas, estruturas de dados, operações e fluxo documentados |
| `parametros.json` | 2 | parâmetros das simulações, semente, opções dos gráficos e análise de eficiência |
| `scripts/config.R` | 2 | leitura dos parâmetros com `ler_parametros()` e definição da semente com `fixar_semente()` |
| `scripts/dados.R` | 2 | simulação do ruído branco, passeio aleatório e AR(1) |
| `scripts/processamento.R` | 2 | média, autocovariâncias e FAC empírica e teórica |
| `scripts/visualizacao.R` | 2 e 4 | gráficos com ggplot2, formatação, impressão e salvamento |
| `main.R` | 3 | execução das cinco etapas: configuração, simulação, cálculo, análise e exibição |
| `ex4_eficiencia.R` | 4 | comparação entre loops `for` e versões vetorizadas |
| `graficos/` | 2 e 4 | 10 gráficos PNG organizados por processo |

As funções dos módulos têm entradas, saídas e pré-condições documentadas nos comentários dos arquivos `.R`.

## Como executar

Pré-requisitos: R e os pacotes ggplot2 e jsonlite.

```r
install.packages(c("ggplot2", "jsonlite"))
```

No RStudio, com a pasta `Lista 2` como diretório de trabalho:

```r
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` lê o arquivo `parametros.json`, fixa a semente e executa as simulações, os cálculos estatísticos e as comparações. No console, apresenta os parâmetros, as médias empíricas, as diferenças em relação à função `acf()` do R e a comparação da FAC empírica com a teórica do AR(1), para as defasagens de 0 a 20.

Os gráficos são exibidos no RStudio e salvos em `graficos/`, separados nas pastas `ruido_branco/`, `passeio_aleatorio/` e `ar1/`. São três gráficos para o ruído branco, três para o passeio aleatório e quatro para o AR(1), totalizando 10 arquivos PNG.

O `ex4_eficiencia.R` compara os tempos de execução das versões com loops e vetorizadas do passeio aleatório e das autocovariâncias, além de verificar se os resultados coincidem.

O `ex1_implementacao_procedural.R` pode ser executado separadamente com `source("ex1_implementacao_procedural.R")`. Ele contém a implementação do Exercício 1 diretamente no script, sem dividir as operações em funções, e utiliza os gráficos base do R.

## Modelagem do sistema (Exercício 3)

O sistema segue o paradigma **procedural**, com funções simples, vetores numéricos, uma lista de parâmetros e execução sequencial. O `main.R` coordena as etapas e passa os resultados entre os módulos.

### 3.2 (a) Arquitetura dos módulos

O código foi separado em quatro módulos dentro da pasta `scripts/`, cada um com uma responsabilidade. O arquivo `main.R` carrega os módulos e controla a ordem de execução.

```text
+----------------------------------------------------------------+
|                            main.R                              |
|          Coordena o pipeline e utiliza source()                 |
+----------------------------------------------------------------+
       |               |                 |               |
       v               v                 v               v
+---------------+ +---------------+ +----------------+ +----------------+
|   config.R    | |    dados.R    | | processamento.R| | visualizacao.R |
|---------------| |---------------| |----------------| |----------------|
|               | |               | |                | |                |
| ler_parametros| | simular_ruido_| | calcular_media_| | tema_graficos  |
|               | | branco        | | empirica       | |                |
| fixar_semente | |               | |                | | ajustar_eixos |
|               | | simular_      | | calcular_auto- | |                |
| parametros.   | | passeio_      | | covariancia_    | | plot_serie_    |
| json          | | aleatorio     | | empirica       | | temporal       |
|               | |               | |                | |                |
|               | | simular_ar1   | | calcular_auto- | | plot_fac_      |
|               | |               | | covariancias   | | empirica       |
|               | |               | |                | |                |
|               | |               | | calcular_fac_  | | plot_fac_      |
|               | |               | | empirica       | | comparacao     |
|               | |               | |                | |                |
|               | |               | | calcular_fac_  | | plot_          |
|               | |               | | teorica_ar1    | | comparacao_    |
|               | |               | |                | | acf_r          |
|               | |               | |                | |                |
|               | |               | |                | | salvar_grafico |
|               | |               | |                | |                |
|               | |               | |                | | imprimir_      |
|               | |               | |                | | titulo_console |
+---------------+ +---------------+ +----------------+ +----------------+
```

| Arquivo | Responsabilidade | Carrega outros arquivos com `source()`? |
|---|---|---|
| `parametros.json` | armazenar os parâmetros e as opções de execução | não |
| `scripts/config.R` | ler os parâmetros e fixar a semente | não |
| `scripts/dados.R` | gerar as três séries temporais | não |
| `scripts/processamento.R` | calcular as estatísticas e as FACs | não |
| `scripts/visualizacao.R` | construir, formatar e salvar os gráficos | não |
| `main.R` | coordenar as etapas, comparar resultados e imprimir as saídas | sim, os quatro módulos |
| `ex4_eficiencia.R` | medir os tempos e comparar as implementações | sim, configuração, dados e processamento |

O arquivo `parametros.json` fica na raiz da lista e guarda `seed`, `n`, `max_lag`, `sigma2`, `phi`, `n_eficiencia`, `repeticoes_eficiencia` e as opções de salvamento dos gráficos. A função `ler_parametros()`, definida em `config.R`, lê esse arquivo e devolve os valores em uma lista.

### 3.2 (b) Dependências e fluxo de dados

No pipeline principal, apenas `main.R` utiliza `source()` para carregar os quatro módulos. Os módulos não carregam uns aos outros. As funções recebem os dados por argumentos e devolvem os resultados ao `main.R`, que os utiliza nas etapas seguintes.

```text
parametros.json
       |
       v
scripts/config.R
       |
       | ler_parametros("parametros.json")
       | fixar_semente(parametros)
       |
       v
+---------------------------------------------------------+
|                         main.R                          |
+---------------------------------------------------------+
       |
       | n = 200, max_lag = 20, sigma2 = 1, phi = 0.7
       | seed = 123
       |
       v
scripts/dados.R
       |
       | simular_ruido_branco(n, sigma2)
       | simular_passeio_aleatorio(n, sigma2)
       | simular_ar1(n, phi, sigma2)
       |
       v
eps, X, X_ar1
       |
       v
scripts/processamento.R
       |
       | calcular_media_empirica(serie)
       | calcular_fac_empirica(serie, max_lag)
       | calcular_fac_teorica_ar1(phi, max_lag)
       |
       v
media_eps, media_X, media_ar1
fac_eps, fac_X, fac_ar1, fac_ar1_teorica
       |
       v
main.R
       |
       | acf() do R para as três séries
       | diferenças entre FAC manual e acf()
       | diferenças entre FAC empírica e teórica do AR(1)
       |
       v
scripts/visualizacao.R
       |
       | plot_serie_temporal()
       | plot_fac_empirica()
       | plot_fac_comparacao()
       | plot_comparacao_acf_r()
       |
       v
Objetos ggplot
       |
       | print()
       | salvar_grafico()
       |
       v
graficos/*.png

main.R -> cat() -> parâmetros, médias, FACs e diferenças no console
```

A execução segue cinco etapas:

```text
Configuração
     |
     v
Simulação de Séries
     |
     v
Cálculo Estatístico
     |
     v
Análise e Comparação
     |
     v
Exibição de Resultados
```

Na configuração, os parâmetros são lidos do JSON e a semente é fixada. Na simulação, são gerados os vetores do ruído branco, passeio aleatório e AR(1), nessa ordem. O processamento calcula as médias empíricas e as FACs. A análise compara os resultados manuais com `acf()` e a FAC empírica do AR(1) com sua expressão teórica. A última etapa imprime os resultados, apresenta os gráficos e salva os arquivos PNG.

### 3.2 (c) Sequência temporal de chamadas (AR(1))

O exemplo abaixo mostra a sequência de chamadas para o processo AR(1), desde a configuração até a apresentação dos resultados.

```text
main.R
  |
  | source("scripts/config.R")
  | source("scripts/dados.R")
  | source("scripts/processamento.R")
  | source("scripts/visualizacao.R")
  |
  | parametros <- ler_parametros("parametros.json")
  | fixar_semente(parametros)
  |
  | n = 200, max_lag = 20, sigma2 = 1, phi = 0.7
  |
  |---------------------------------------------------+
  |                                                   |
  | simular_ar1(n, phi, sigma2)                        |
  |                                                   v
  |                                            scripts/dados.R
  |                                                   |
  |                                     simular_ruido_branco()
  |                                     X[1] = eps[1]
  |                                     for t = 2, ..., n:
  |                                     X[t] = phi*X[t-1] + eps[t]
  |                                                   |
  |<--------------------- X_ar1 -----------------------+
  |
  |---------------------------------------------------+
  |                                                   |
  | calcular_media_empirica(X_ar1)                     |
  |                                                   v
  |                                        scripts/processamento.R
  |                                                   |
  |<--------------------- media_ar1 -------------------+
  |
  | calcular_fac_empirica(X_ar1, max_lag)              |
  |                                                   |
  |                         calcular_autocovariancias()
  |                                     |
  |                         calcular_autocovariancia_empirica()
  |                                     |
  |                         calcular_media_empirica()
  |                                     |
  |                         gamma(h) / gamma(0)
  |                                                   |
  |<--------------------- fac_ar1 ---------------------+
  |
  | calcular_fac_teorica_ar1(phi, max_lag)             |
  |                                     |
  |                         rho(h) = phi^h
  |                                     |
  |<--------------------- fac_ar1_teorica -------------+
  |
  | acf(X_ar1, lag.max = max_lag, plot = FALSE)
  |
  | diferenca_ar1 <- abs(fac_ar1 - fac_ar1_teorica)
  | erro_fac_ar1 <- max(diferenca_ar1)
  |
  | cat() imprime média e diferenças
  |
  |---------------------------------------------------+
  |                                                   |
  | plot_serie_temporal()                              |
  | plot_fac_empirica()                                |
  | plot_fac_comparacao()                              |
  | plot_comparacao_acf_r()                            |
  |                                                   v
  |                                        scripts/visualizacao.R
  |                                                   |
  |<---------------- objetos ggplot -------------------+
  |
  | print(grafico)
  | salvar_grafico(grafico, nome_arquivo, parametros)
  |
  |---------------------------------------------------+
  |                                                   |
  v                                                   v
Console                                         graficos/ar1/
                                               4 arquivos PNG
```

A simulação do AR(1) utiliza a relação \(X_t = \phi X_{t-1} + \varepsilon_t\), com \(X_1 = \varepsilon_1\). O loop começa em `t = 2`, pois cada valor depende do anterior. Com `phi = 0.7`, a influência dos valores passados diminui a cada defasagem.

A função `calcular_fac_empirica()` utiliza as autocovariâncias calculadas de `h = 0` até `max_lag` e divide cada uma pela autocovariância de lag zero. A função `calcular_fac_teorica_ar1()` calcula \(\rho(h) = \phi^h\). O `main.R` compara as duas FACs, calcula a diferença absoluta em cada lag e apresenta a maior diferença.

Os quatro gráficos do AR(1) são a série temporal, a FAC empírica, a comparação entre FAC empírica e teórica e a comparação entre a FAC manual e a função `acf()` do R.

### 3.3 Por que é procedural

- O programa executa as etapas em sequência, com o `main.R` controlando as chamadas e a passagem dos resultados entre os módulos.
- As séries temporais são armazenadas em vetores numéricos de tamanho `n`. As autocovariâncias e FACs são vetores de tamanho `max_lag + 1`, com a defasagem `h` na posição `h + 1`.
- As funções recebem os dados por argumentos e retornam valores ou vetores. As funções de configuração, impressão e salvamento também executam suas operações específicas.
- O controle das operações utiliza estruturas como `for` e `if`, incluindo os loops de recorrência temporal e os cálculos manuais das autocovariâncias.
- Os módulos são separados por responsabilidade, sem definição de classes ou uso de herança. O programa utiliza funções e estruturas de dados simples, além dos objetos ggplot necessários para gerar os gráficos.

## Validação dos resultados

1. **Reprodutibilidade:** `ex1_implementacao_procedural.R` e `main.R` utilizam a semente 123 e a mesma ordem de simulação: ruído branco, passeio aleatório e AR(1). As médias empíricas obtidas foram -0.0086 para o ruído branco, 7.2628 para o passeio aleatório e 0.0895 para o AR(1).

2. **Comparação com o R:** o `main.R` compara a FAC calculada manualmente com a função `acf()` do R para os três processos. A maior diferença apresentada no console foi 0.0000 em todos os casos, com arredondamento para quatro casas decimais.

3. **Comparação com a teoria:** no AR(1), com φ = 0.7, a FAC teórica é dada por ρ(h) = φ^h. No lag 1, a FAC empírica foi 0.6190 contra 0.7000 da teórica. No lag 2, foi 0.3959 contra 0.4900. A maior diferença absoluta foi 0.1147, no lag 20. As diferenças são resultado da variação amostral com n = 200.

4. **Comportamento dos processos:** o ruído branco apresenta autocorrelações próximas de zero fora do lag 0. O passeio aleatório tem autocorrelações persistentes, pois acumula os choques e não é estacionário. O AR(1), com |φ| < 1, apresenta decaimento da autocorrelação conforme aumenta a defasagem.

Os gráficos da FAC empírica incluem limites de confiança de 95%, calculados por ±1.96/√n. As comparações com `acf()` permitem verificar os cálculos manuais, enquanto a comparação com a FAC teórica mostra o comportamento do AR(1).

## Análise de eficiência (Exercício 4.3)

**Gargalos dos loops `for`:**

- `simular_passeio_aleatorio()` percorre os valores de 2 até `n`, somando cada choque ao valor anterior. O cálculo é feito passo a passo pelo interpretador do R.
- `calcular_autocovariancia_empirica()` percorre os valores da série para cada defasagem, com aproximadamente n × (max_lag + 1) operações.
- A média empírica é recalculada a cada chamada de `calcular_autocovariancia_empirica()`, mesmo sendo a mesma para todas as defasagens.

**Melhorias em `ex4_eficiencia.R`:**

- `cumsum(eps)` substitui o loop do passeio aleatório. A função calcula a soma acumulada dos choques, mantendo a relação X_t = ε_1 + ... + ε_t.
- `mean(serie)` calcula a média uma única vez. O vetor `d = serie - mean(serie)` armazena os desvios em relação à média.
- `sum(d[1:(n-h)] * d[(1+h):n]) / n` substitui o loop interno da autocovariância. Os dois trechos de `d` representam a mesma série deslocada em h posições. A multiplicação elemento a elemento seguida de `sum()` calcula a soma dos produtos dos desvios. Para h = 0, o resultado corresponde à variância, sem precisar de uma condição separada.
- O loop sobre as defasagens de 0 até `max_lag` foi mantido, pois percorre apenas 21 valores.
- A FAC vetorizada é calculada dividindo as autocovariâncias pela autocovariância de lag zero.
- O AR(1) mantém o loop de simulação, pois cada X_t depende do valor anterior X_{t-1}, que precisa estar calculado.

**Resultados:** a comparação utilizou n = 100000, max_lag = 20 e cinco repetições para medir o tempo de cada implementação.

| Operação | Loop `for` | Vetorizada | Ganho | Maior diferença |
|---|---|---|---|---|
| Passeio aleatório | 0.08 s | 0.05 s | 1.6x | 0.0000 |
| Autocovariâncias | 1.61 s | 0.21 s | 7.667x | 0.0000 |

Os tempos correspondem às cinco repetições de cada operação. A comparação das séries do passeio aleatório e das autocovariâncias retornou `TRUE` com `all.equal()`, confirmando a equivalência dos resultados.

A FAC empírica também foi calculada pelas duas implementações. A maior diferença apresentada foi 0.0000 e a comparação com `all.equal()` retornou `TRUE`.

A vetorização reduziu o tempo de execução nas duas operações. O maior ganho ocorreu no cálculo das autocovariâncias, que passou de 1.61 s para 0.21 s. Os resultados estatísticos foram preservados, com diferenças exibidas como zero nas comparações.
