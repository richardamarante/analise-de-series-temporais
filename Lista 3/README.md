# Lista 3: Série temporal com tendência polinomial em R (21/08/2026)

Análise de Séries Temporais (GET00127)

## Objetivo

Simular uma série temporal com tendência polinomial (Xₜ = β₀ + β₁t + β₂t² + εₜ), suavizá-la com **média móvel centrada**, calcular manualmente a média amostral, a autocovariância e a **função de autocorrelação empírica (FAC)**, comparar os resultados com a função `acf()` do R e gerar gráficos com ggplot2.

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `Lista 3.pdf` | 1 a 4 | enunciado da lista |
| `ex1_implementacao_procedural.R` | 1 | implementação procedural completa, com documentação de entradas, saídas, estruturas, funções e fluxo |
| `parametros.json` | 2 | parâmetros da simulação, análise de eficiência e opções de salvamento dos gráficos |
| `scripts/config.R` | 2 | leitura dos parâmetros, semente, vetor de tempo e matriz de regressão |
| `scripts/dados.R` | 2 | cálculo da tendência, simulação do ruído branco e da série |
| `scripts/processamento.R` | 2 | média móvel, média amostral, autocovariâncias e FAC empírica |
| `scripts/visualizacao.R` | 2 | gráficos com ggplot2 e patchwork, formatação e salvamento |
| `main.R` | 3 | pipeline: Configuração → Simulação → Média Móvel → FAC → Visualização |
| `ex4_eficiencia.R` | 4 | comparação entre loops e versões vetorizadas, medição de tempos e gráfico de eficiência |
| `graficos/serie_tendencia/` | 2 e 3 | cinco gráficos gerados pelo pipeline |
| `graficos/eficiencia/` | 4 | gráfico de comparação dos tempos de execução |

As funções dos módulos estão documentadas com entradas, saídas e pré-condições. Os parâmetros ficam em `parametros.json`, na raiz da lista, e são lidos pela função `ler_parametros()` de `config.R`.

## Como executar

Pré-requisitos: R e os pacotes ggplot2, jsonlite e patchwork.

```r
install.packages(c("ggplot2", "jsonlite", "patchwork"))
```

No RStudio, com a pasta da lista como diretório de trabalho:

```r
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` executa o pipeline completo e imprime no console os parâmetros, a média amostral, a autocovariância de defasagem 1, a FAC empírica e sua comparação com `acf()`. Também gera cinco gráficos PNG em `graficos/serie_tendencia/`:

| Arquivo | Conteúdo |
|---|---|
| `01_serie_temporal_tendencia.png` | série temporal com tendência polinomial |
| `02_serie_temporal_media_movel_tendencia.png` | série original, média móvel centrada e tendência verdadeira |
| `03_funcao_autocorrelacao_empirica_tendencia.png` | FAC empírica com limites de 95% |
| `04_comparacao_fac_empirica_acf_r_tendencia.png` | comparação entre a FAC manual e `acf()` do R |
| `05_grafico_integrado_tendencia.png` | série, média móvel e tendência no painel superior, com a FAC no inferior |

Os gráficos são salvos quando `salvar_graficos = true`, com largura de 10 polegadas, altura de 6 polegadas e resolução de 300 dpi. O gráfico integrado utiliza um fator de altura de 1.6. O gráfico 02 tem o título "Série Temporal com Média Móvel e Tendência".

O `ex4_eficiencia.R` compara os tempos das implementações com loops e vetorizadas, verifica se os resultados são iguais e imprime a maior diferença relativa. Também gera `graficos/eficiencia/01_tempo_loop_vetorizada.png`, que mostra os tempos em função de n = 1000, 25000 e 50000, com um painel para cada operação.

## Validação dos resultados

1. **Parâmetros:** a simulação utiliza `seed = 123`, `n = 90`, `sigma = 15`, `q = 3`, `max_lag = 20` e `beta = (2, 0.01, 0.01)`. A tendência é dada por 2 + 0.01t + 0.01t², com ruído branco normal de desvio padrão 15.
2. **Estatísticas:** o `main.R` retorna média amostral de 30.6849 e autocovariância empírica de defasagem 1 igual a 588.9894. A autocovariância é calculada com divisor n, para todas as defasagens.
3. **Média móvel:** com n = 90 e q = 3, a janela possui 2q + 1 = 7 pontos. São calculadas n − 2q = 84 médias, correspondentes aos instantes t = 4 até t = 87.
4. **Tendência:** a função `calcular_tendencia()` calcula X·beta, sem acrescentar ruído. Essa curva é mostrada junto à série simulada e à média móvel nos gráficos 02 e 05.
5. **Comparação com o R:** a FAC manual é comparada com `acf(Xt, lag.max = 20, plot = FALSE)`. A maior diferença absoluta apresentada no console é 0. A comparação com `all.equal()` retorna `TRUE`.
6. **FAC:** a autocorrelação é 1 na defasagem 0, 0.7285 na defasagem 1 e 0.2910 na defasagem 20. Os valores permanecem positivos e apresentam uma queda geral, comportamento esperado para uma série com tendência não estacionária.

## Modelagem do sistema (Exercício 3)

O programa utiliza o paradigma **procedural**, com variáveis, vetores, matrizes, funções e chamadas em sequência. O código está dividido em módulos, e o `main.R` coordena as etapas do processamento.

### 3.2 (a) Arquitetura modular

```text
                       +------------------+
                       |      main.R      |
                       | Coordena o fluxo |
                       +------------------+
                                |
              +-----------------+-----------------+
              |                 |                 |
              v                 v                 v
       +------------+    +------------+    +----------------+
       |  config.R  |    |  dados.R   |    | processamento.R|
       +------------+    +------------+    +----------------+
       | Parâmetros |    | Tendência  |    | Média móvel    |
       | Semente    |    | Ruído      |    | Média amostral |
       | Tempo      |    | Série      |    | Autocovariância|
       | Matriz X   |    | simulada   |    | FAC empírica   |
       +------------+    +------------+    +----------------+
              |                 |                 |
              +-----------------+-----------------+
                                |
                                v
                       +------------------+
                       | visualizacao.R   |
                       +------------------+
                       | Gráficos         |
                       | Formatação       |
                       | Salvamento       |
                       +------------------+

  parametros.json -> config.R
```

| Arquivo | Responsabilidade | Faz `source` de outro arquivo? |
|---|---|---|
| `config.R` | ler parâmetros, fixar semente, criar t e matriz X | não |
| `dados.R` | calcular tendência, simular ruído branco e série | não |
| `processamento.R` | calcular média móvel, média, autocovariâncias e FAC | não |
| `visualizacao.R` | construir, formatar e salvar os gráficos | não |
| `main.R` | executar as etapas, comparar resultados e imprimir no console | sim, os quatro módulos |

Os quatro módulos não utilizam `source()` entre si. O `main.R` carrega todos eles e chama as funções na ordem necessária. O `ex4_eficiencia.R` carrega apenas `config.R`, `dados.R` e `processamento.R`.

Os cálculos estatísticos ficam em `processamento.R`, enquanto `visualizacao.R` recebe os dados já calculados e gera os gráficos. Cada módulo possui uma responsabilidade definida.

### 3.2 (b) Fluxo de dados

A dependência de código é controlada pelo `main.R`, que carrega os módulos. Os dados passam entre as funções por argumentos e retornos, seguindo o fluxo:

```text
parametros.json
       |
       v
config.R
  ler_parametros()
  fixar_semente()
  criar_tempo()
  criar_matriz_regressao()
       |
       v
  parametros, t, X
       |
       v
dados.R
  calcular_tendencia(beta, X) -> tendencia
  simular_ruido_branco()      -> eps
  simular_serie_com_tendencia() -> Xt
       |
       +-------------------------+
       |                         |
       v                         v
processamento.R           processamento.R
  calcular_media_movel()    calcular_media_amostral()
       |                    calcular_autocovariancia_empirica()
       v                    calcular_fac_empirica()
      m_q                        |
       |                         v
       |                  media_Xt, gamma_1, fac_Xt
       |                         |
       +-------------+-----------+
                     |
                     v
main.R: comparação com acf() e saída no console
                     |
                     v
visualizacao.R: construção dos cinco gráficos
                     |
                     v
       graficos/serie_tendencia/*.png
```

A matriz X contém três colunas: 1, t e t². A função `calcular_tendencia()` percorre essa matriz e os coeficientes `beta` para calcular X·beta. A função `simular_serie_com_tendencia()` utiliza essa tendência e acrescenta o ruído branco.

A média móvel é calculada sobre Xt, com uma janela centrada de 7 observações. A análise estatística utiliza a série original para calcular a média, as autocovariâncias e a FAC. Cada autocorrelação é obtida dividindo a autocovariância da defasagem h pela autocovariância de defasagem zero.

Os resultados são enviados às funções de visualização, que constroem os cinco gráficos. A função `salvar_grafico()` utiliza as opções dos parâmetros para gravar os arquivos PNG.

### 3.2 (c) Sequência procedural de chamadas

```text
main.R
  |
  |-- rm(list = ls())
  |-- library(ggplot2, jsonlite, patchwork)
  |-- source() dos quatro módulos
  |
  |-- ler_parametros()
  |-- fixar_semente()
  |-- criar_tempo()
  |-- criar_matriz_regressao()
  |
  |-- simular_serie_com_tendencia()
  |      |-- calcular_tendencia()
  |      |      loop sobre linhas e colunas de X
  |      |-- simular_ruido_branco()
  |      +-- soma tendência e ruído
  |
  |-- calcular_media_movel()
  |      loop sobre as janelas centradas
  |
  |-- calcular_media_amostral()
  |-- calcular_autocovariancia_empirica()
  |-- calcular_fac_empirica()
  |      +-- calcular_autocovariancias()
  |
  |-- acf()
  |-- max(abs(fac_Xt - fac_R))
  |-- all.equal()
  |
  |-- plot_serie_temporal()
  |-- plot_serie_media_movel()
  |-- plot_fac_empirica()
  |-- plot_comparacao_acf_r()
  |-- plot_grafico_integrado()
  |-- salvar_grafico()
  |
  v
Fim
```

O `main.R` começa carregando os pacotes e os módulos. Depois lê os parâmetros, fixa a semente e constrói o vetor de tempo e a matriz de regressão. A simulação gera Xt, que é utilizada no cálculo da média móvel e das estatísticas.

A FAC manual é comparada com a função `acf()` do R. Em seguida, as funções de visualização constroem os gráficos, que são exibidos e salvos na pasta indicada.

### 3.3 Por que é procedural

- As etapas são executadas em sequência, seguindo a ordem definida no `main.R`.
- Os dados são armazenados em variáveis, vetores, matrizes numéricas e uma lista de parâmetros.
- As funções recebem os dados por argumentos e retornam números, vetores, matrizes ou gráficos. A função `fixar_semente()` configura o gerador de números aleatórios.
- Os cálculos utilizam loops `for` e condições `if`, sem classes próprias ou métodos de programação orientada a objetos.
- Cada módulo reúne funções de uma etapa específica. O `main.R` controla o fluxo de execução e utiliza os resultados retornados pelas funções.

## Análise de eficiência (Exercício 4.3)

O arquivo `ex4_eficiencia.R` compara as implementações com loops `for` e as versões vetorizadas de quatro operações: construção da matriz de regressão, simulação da série, cálculo da média móvel e cálculo das autocovariâncias.

**Gargalos dos loops `for`:**

- Na construção da matriz X, o código percorre as n linhas para preencher as três colunas com 1, t e t².
- Na simulação, a função `calcular_tendencia()` utiliza loops aninhados para percorrer cada instante e cada coeficiente de `beta`, calculando os produtos separadamente.
- Na média móvel, cada posição exige percorrer os 2q + 1 valores da janela e calcular sua média.
- Nas autocovariâncias, cada defasagem exige percorrer os pares de observações. A média amostral também é recalculada a cada chamada da função.

**Melhorias implementadas em `ex4_eficiencia.R`:**

- `criar_matriz_regressao_vetorizada()` utiliza `cbind(1, t, t^2)` para construir a matriz X diretamente, sem percorrer suas linhas.
- `simular_serie_com_tendencia_vetorizada()` substitui os loops por `X %*% beta`, que calcula o produto matricial. O ruído é gerado de uma vez com `rnorm(n, mean = 0, sd = sigma)`.
- `calcular_media_movel_vetorizada()` utiliza `cumsum()` para calcular as somas acumuladas da série. A soma de cada janela é obtida pela diferença entre duas posições do vetor acumulado, dividida por 2q + 1.
- `calcular_autocovariancia_empirica_vetorizada()` utiliza `mean(serie)` e calcula os desvios `d = serie - media`. O produto `d[1:(n-h)] * d[(1+h):n]` multiplica os pares de observações separados pela defasagem h. A função `sum()` soma os produtos, e o resultado é dividido por n.
- `calcular_autocovariancias_vetorizada()` utiliza `sapply()` para aplicar o cálculo anterior às defasagens de 0 até `max_lag`, sem escrever um loop explícito sobre as defasagens.

As funções `medir_tempo()` e `medir_tempos_por_n()` medem os tempos de execução e permitem comparar diferentes tamanhos de série. A função `formatar_diferenca()` formata as diferenças relativas, e `plot_tempo_eficiencia()` gera o gráfico de comparação.

**Resultados** (n = 50000, 10 repetições por operação):

| Operação | Loops `for` | Vetorizada | Ganho | Resultados iguais | Maior diferença relativa |
|---|---:|---:|---:|---|---:|
| Matriz de regressão | 0.31 s | 0.05 s | 6.2x | TRUE | 0 |
| Simulação da série | 2.46 s | 0.06 s | 41x | TRUE | 0 |
| Média móvel | 0.75 s | 0.05 s | 15x | TRUE | 3.47e-13 |
| Autocovariâncias | 3.34 s | 0.47 s | 7.1x | TRUE | 9.28e-15 |

Os tempos são medidos com `system.time()` e representam o total das 10 repetições. Na simulação, a semente é fixada antes de cada execução para permitir a comparação dos mesmos resultados. Os tempos podem variar entre execuções.

Todas as versões vetorizadas retornaram `TRUE` na verificação com `all.equal()`. As maiores diferenças relativas foram 0 na matriz e na simulação, 3.47e-13 na média móvel e 9.28e-15 nas autocovariâncias.

O maior ganho foi na simulação da série, que ficou 41 vezes mais rápida. A média móvel teve ganho de 15 vezes, seguida pelo cálculo das autocovariâncias, com 7.1 vezes, e pela construção da matriz, com 6.2 vezes.

O gráfico `01_tempo_loop_vetorizada.png` compara os tempos das duas implementações para n = 1000, 25000 e 50000. Cada uma das quatro operações aparece em um painel, permitindo observar como os tempos variam com o tamanho da série.

A vetorização reduziu o tempo de execução nas quatro operações, mantendo os resultados dos cálculos originais.
