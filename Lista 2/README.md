# Lista 2 – Análise de Séries Temporais (14/08/2026)

## Objetivo

O projeto simula três processos estocásticos (ruído branco, passeio aleatório e AR(1)), calcula à mão a média empírica, as autocovariâncias e a função de autocorrelação (FAC) de cada um e compara os resultados com a teoria e com a função `acf()` do R. A FAC empírica do AR(1) é posta ao lado da teórica, rho(h) = phi^h. A lista também pede o script do professor reescrito de forma procedural, dividido em módulos, com um pipeline coordenador, gráficos em ggplot2 e uma análise de eficiência dos loops. O ponto de partida é `original/Aula_14_08_2026_Exe_PE.R`, e o enunciado está em `original/Lista_Exe_14_08_2026.pdf`.

## Estrutura de pastas

```
Lista 2/
|-- original/
|   |-- Aula_14_08_2026_Exe_PE.R      (script do professor)
|   \-- Lista_Exe_14_08_2026.pdf      (enunciado)
|-- scripts/
|   |-- 01_config.R                   (Exercício 2.1.a)
|   |-- 02_dados.R                    (Exercício 2.1.b)
|   |-- 03_processamento.R            (Exercício 2.1.c)
|   |-- 04_visualizacao.R             (Exercícios 2.1.d, 4.1 e 4.2)
|   \-- 05_eficiencia.R               (Exercício 4.3)
|-- graficos/                         (gerada pelo main.R)
|   |-- ruido_branco/
|   |-- passeio_aleatorio/
|   \-- ar1/
|-- implementacao_procedural.R        (Exercício 1)
|-- main.R                            (Exercício 3.1)
|-- eficiencia.R                      (Exercício 4.3)
|-- parametros.json                   (Exercício 2.1.a)
\-- README.md                         (Exercícios 2.3, 3.2, 3.3 e 4.4)
```

Os quatro módulos pedidos no Exercício 2 levam número no nome (`01_config.R` no lugar de `config.R`, e assim por diante) para a ordem de carga aparecer na pasta. Os valores do modelo ficam em `parametros.json`, e o `01_config.R` lê esse arquivo. Cada gráfico é salvo em `graficos/<processo>/` com o assunto no fim do nome do arquivo.

## Como executar

Pacotes necessários: `ggplot2` e `jsonlite`.

```r
install.packages(c("ggplot2", "jsonlite"))
```

No RStudio, abra a pasta `Lista 2` como diretório de trabalho (Session > Set Working Directory) e rode:

```r
source("main.R")
```

Isso roda o pipeline do Exercício 3.1: imprime os parâmetros, as médias e a tabela da FAC do AR(1) no console, mostra os gráficos e grava os PNG em `graficos/`. A análise de eficiência (Exercício 4.3) roda à parte:

```r
source("eficiencia.R")
```

Pelo terminal, dentro da pasta, valem `Rscript main.R` e `Rscript eficiencia.R`. O Exercício 1 roda sozinho em `implementacao_procedural.R` e usa os gráficos do R base.

## Interfaces dos módulos (Exercício 2.3)

As funções trazem a mesma documentação em comentário acima de cada uma. Os vetores de autocovariância e de FAC têm tamanho `max_lag + 1`, e a posição `h + 1` guarda o lag `h`, porque o R indexa a partir de 1.

### 01_config.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `ler_parametros` | `arquivo`: texto com o caminho do `.json` | lista com `seed`, `n`, `max_lag`, `sigma2`, `phi` e as opções dos gráficos | o arquivo existe e o `jsonlite` está carregado |
| `fixar_semente` | `parametros`: lista com o campo `seed` | nenhuma, só fixa o gerador | `parametros` vem de `ler_parametros` |

### 02_dados.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `simular_ruido_branco` | `n` (inteiro >= 1), `sigma2` (> 0) | vetor numérico de tamanho `n` | nenhuma |
| `simular_passeio_aleatorio` | `n`, `sigma2` | vetor numérico de tamanho `n` | semente já fixada |
| `simular_ar1` | `n`, `phi`, `sigma2` | vetor numérico de tamanho `n` | abs(`phi`) < 1 e semente já fixada |

### 03_processamento.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `calcular_media_empirica` | `serie` (vetor de tamanho `n`) | número | sem `NA` |
| `calcular_autocovariancia_empirica` | `serie`, `h` (inteiro) | número, gamma(h) | 0 <= `h` < `n` |
| `calcular_autocovariancias` | `serie`, `max_lag` | vetor com gamma(0), ..., gamma(`max_lag`) | 0 <= `max_lag` < `n` |
| `calcular_fac_empirica` | `serie`, `max_lag` | vetor com rho(0), ..., rho(`max_lag`) | série não constante e `max_lag` < `n` |
| `calcular_fac_teorica_ar1` | `phi`, `max_lag` | vetor com phi^0, ..., phi^`max_lag` | abs(`phi`) < 1 |

### 04_visualizacao.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `tema_graficos` | nenhuma | tema do ggplot2 | `ggplot2` carregado |
| `ajustar_eixos` | `grafico` | gráfico sem folga nos eixos | nenhuma |
| `salvar_grafico` | `grafico`, `nome_arquivo` (sem extensão), `parametros` | grava `graficos/<nome_arquivo>.png` | `parametros` com `salvar_graficos`, `largura_grafico`, `altura_grafico` e `dpi_grafico` |
| `plot_serie_temporal` | `serie`, `titulo`, `ylab`, `rotulo_serie` | objeto ggplot | série sem `NA` |
| `plot_fac_empirica` | `fac`, `n`, `titulo` | objeto ggplot, com limites de 95% | `fac` vem de `calcular_fac_empirica`, `n` é o tamanho da série |
| `plot_fac_comparacao` | `fac_empirica`, `fac_teorica`, `titulo`, `n` | objeto ggplot | os dois vetores com o mesmo tamanho |
| `plot_comparacao_acf_r` | `fac`, `fac_r`, `titulo` | objeto ggplot | os dois vetores com o mesmo tamanho |
| `imprimir_titulo_console` | `titulo` | nenhuma, só imprime | nenhuma |

### 05_eficiencia.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `simular_passeio_aleatorio_vetorizada` | `n`, `sigma2` | vetor de tamanho `n` (`cumsum` do ruído) | `02_dados.R` carregado |
| `calcular_autocovariancias_vetorizada` | `serie`, `max_lag` | vetor com gamma(0), ..., gamma(`max_lag`) | 0 <= `max_lag` < `n` |
| `calcular_fac_empirica_vetorizada` | `serie`, `max_lag` | vetor com rho(0), ..., rho(`max_lag`) | série não constante |
| `comparar_eficiencia` | `parametros` (com `seed`, `sigma2`, `max_lag`, `n_eficiencia`, `repeticoes_eficiencia`) | nenhuma, só imprime tempos e conferências | `01_config.R`, `02_dados.R` e `03_processamento.R` carregados |

## Modelagem procedural (Exercício 3.2)

### Arquitetura dos módulos (3.2a)

```
                        parametros.json
                               |
                               v
   +------------------------------------------------------+
   |                       main.R                         |
   |  carrega os módulos e chama as funções, em ordem     |
   +------------------------------------------------------+
        |            |              |              |
        v            v              v              v
  +-----------+ +-----------+ +---------------+ +----------------+
  |01_config.R| |02_dados.R | |03_processa-   | |04_visuali-     |
  |           | |           | |mento.R        | |zacao.R         |
  | ler_para- | | simular_  | | calcular_     | | plot_serie_    |
  |  metros   | |  ruido_   | |  media_       | |  temporal      |
  | fixar_    | |  branco   | |  empirica     | | plot_fac_      |
  |  semente  | | simular_  | | calcular_auto-| |  empirica      |
  |           | |  passeio  | |  covariancia* | | plot_fac_      |
  |           | | simular_  | | calcular_fac_ | |  comparacao    |
  |           | |  ar1      | |  empirica     | | plot_comparacao|
  |           | |           | | calcular_fac_ | |  _acf_r        |
  |           | |           | |  teorica_ar1  | | salvar_grafico |
  +-----------+ +-----------+ +---------------+ +----------------+

   +------------------------------------------------------+
   |                    eficiencia.R                      |
   |  (Exercício 4.3, roda à parte do main.R)             |
   +------------------------------------------------------+
        |            |              |              |
        v            v              v              v
   01_config.R  02_dados.R  03_processamento.R  scripts/05_eficiencia.R
```

As setas saem só do `main.R` e do `eficiencia.R`. Os módulos não se chamam entre si, com uma exceção: o `05_eficiencia.R` usa funções do 02 e do 03 para comparar a versão com loop com a vetorizada sem duplicar código.

| Arquivo | Responsabilidade única | Depende de |
|---|---|---|
| `01_config.R` | ler os parâmetros e fixar a semente | `jsonlite` |
| `02_dados.R` | simular os três processos | nenhum arquivo |
| `03_processamento.R` | média, autocovariâncias e FAC | nenhum arquivo |
| `04_visualizacao.R` | gráficos e gravação dos PNG | `ggplot2` |
| `05_eficiencia.R` | versões vetorizadas e comparação de tempo | `01_config.R`, `02_dados.R`, `03_processamento.R` |
| `main.R` | pipeline do Exercício 3.1 | `01` a `04` |
| `eficiencia.R` | análise do Exercício 4.3 | `01`, `02`, `03` e `05_eficiencia.R` |

### Dependências e fluxo de dados (3.2b)

Os módulos de cálculo não conhecem o de exibição. `03_processamento.R` não usa nada de `04_visualizacao.R`, e `04_visualizacao.R` só recebe vetores já prontos. Só o `05_eficiencia.R` reaproveita código de outros módulos. Os dados passam de um módulo ao outro por argumento e retorno, sempre pelo `main.R` (ou pelo `eficiencia.R`, no último passo):

```
parametros.json --ler_parametros--> parametros --> n, max_lag, sigma2, phi
parametros$seed --fixar_semente--> gerador de números aleatórios

n, sigma2, phi --02_dados--> eps, X, X_ar1            (vetores de tamanho n)
séries --03_processamento--> media_*, fac_*           (número e vetores de tamanho max_lag + 1)
phi, max_lag --03_processamento--> fac_ar1_teorica
fac_*, acf() do R --main.R--> diferenca_ar1, maior diferença para o acf()
séries, fac_* --04_visualizacao--> objetos ggplot --salvar_grafico--> graficos/*.png
parametros --05_eficiencia--> tempos e conferências no console
```

O pipeline linear do Exercício 3.1, com a etapa do `main.R` que executa cada passo:

```
Configuração -> Simulação de Séries -> Cálculo Estatístico -> Análise e Comparação -> Exibição de Resultados
```

### Sequência de chamadas para o AR(1) (3.2c)

```
main.R
  |-- ler_parametros("parametros.json")                  [01_config.R]
  |-- fixar_semente(parametros)                          [01_config.R]
  |-- simular_ar1(n, phi, sigma2)                        [02_dados.R]
  |      \-- simular_ruido_branco(n, sigma2)
  |      \-- loop t = 2, ..., n: X[t] = phi * X[t-1] + eps[t]
  |      <-- X_ar1 (vetor de tamanho n)
  |-- calcular_media_empirica(X_ar1)                     [03_processamento.R]
  |      <-- media_ar1
  |-- calcular_fac_empirica(X_ar1, max_lag)
  |      \-- calcular_autocovariancias(X_ar1, max_lag)
  |             \-- calcular_autocovariancia_empirica(X_ar1, h), h = 0, ..., max_lag
  |                    \-- calcular_media_empirica(X_ar1)
  |      <-- fac_ar1 (vetor de tamanho max_lag + 1)
  |-- calcular_fac_teorica_ar1(phi, max_lag)
  |      <-- fac_ar1_teorica
  |-- diferenca_ar1 = |fac_ar1 - fac_ar1_teorica|, e fac_ar1 contra acf(X_ar1)
  |-- cat(...): média, tabela lag a lag e maior diferença
  |-- plot_serie_temporal(X_ar1, ...)                    [04_visualizacao.R]
  |-- plot_fac_empirica(fac_ar1, n, ...)
  |-- plot_fac_comparacao(fac_ar1, fac_ar1_teorica, ..., n)
  |-- plot_comparacao_acf_r(fac_ar1, acf_ar1, ...)
  \-- salvar_grafico(grafico, "ar1/...", parametros)     [grava os PNG]
```

### Por que o paradigma é estritamente procedural (3.3)

O programa só tem variáveis, vetores numéricos, funções comuns e chamadas em sequência. Não há classe, objeto com estado nem método. Cada função recebe tudo o que usa por argumento, devolve um número ou um vetor e não guarda nada entre uma chamada e outra. O controle de fluxo é `for` e `if`, e o `main.R` roda de cima para baixo, sem desvio de controle entre módulos. Os módulos só organizam procedimentos e dados em arquivos, o que é modularização e não muda o paradigma. Os únicos objetos que aparecem são os do ggplot2, e eles são só o retorno das funções de gráfico, entregues para `print()` e `ggsave()`.

## Análise de eficiência (Exercício 4.3)

### Gargalos dos loops

- Soma acumulada do passeio aleatório: o loop dá n - 1 passos, e cada um lê `X[t-1]`, soma `eps[t]` e grava em `X[t]`. O custo é proporcional a n, mas cada passo passa pelo interpretador do R, que é lento para séries longas.
- Autocovariância: `calcular_autocovariancia_empirica` percorre a série uma vez por lag, e a cada chamada recalcula a média com outro loop de n passos. Para `max_lag + 1` lags são cerca de 2n(`max_lag` + 1) passos de interpretador, e a média sai igual todas as vezes.

### Melhoria proposta (em 05_eficiencia.R)

A soma acumulada do passeio virou `cumsum`, e a autocovariância virou um produto de vetores deslocados. Em `calcular_autocovariancias_vetorizada`, os desvios `d = serie - mean(serie)` são calculados uma vez só. Para cada lag h, `d[1:(n-h)]` e `d[(1+h):n]` são a mesma série deslocada h posições, então `sum(d[1:(n-h)] * d[(1+h):n]) / n` dá exatamente a soma de (X_t - média)(X_{t+h} - média) dividida por n. O produto elemento a elemento e a soma rodam em código compilado, e o loop interno sobre t desaparece. Com h = 0 a fórmula vira a soma dos quadrados, e o `if(h == 0)` deixa de ser necessário. Sobra o loop sobre os lags, que tem `max_lag + 1` passos e fica como está.

A mesma conta é feita nas duas versões, só em outra ordem de execução, então a matemática se preserva. A diferença entre as versões fica em arredondamento de ponto flutuante, e `all.equal` dá `TRUE` nas três comparações. O AR(1) continua com loop, porque cada X_t depende do X_{t-1} recém-calculado e não há função simples que faça essa recorrência de uma vez.

### Resultado

Rodando `eficiencia.R` com n = 100000, `max_lag` = 20 e 5 repetições, tempo total das 5 em segundos, na última execução:

| Cálculo | Loop `for` | Versão vetorizada | Razão |
|---|---|---|---|
| Passeio aleatório (soma acumulada) | 0,1 | 0,05 | 2 |
| Autocovariâncias de 0 a 20 | 1,86 | 0,21 | 8,857 |

O tempo muda de uma execução para outra, e o que conta é a razão. Nas execuções que fiz, a razão das autocovariâncias ficou entre 8 e 27. No passeio o ganho é pequeno porque as duas versões gastam boa parte do tempo sorteando o ruído com `rnorm`, que é o mesmo nas duas. Com n = 200, o tamanho do exercício, a diferença nem se percebe, e o ganho só importa em séries longas.

## Validação dos resultados

Com semente 123, n = 200, `max_lag` = 20, sigma2 = 1 e phi = 0,7:

| Série | Média empírica | Maior diferença da FAC manual para o `acf()` do R |
|---|---|---|
| Ruído branco | -0,0086 | 2,8e-17 |
| Passeio aleatório | 7,2628 | 6,7e-16 |
| AR(1) | 0,0895 | 2,2e-16 |

1. Contra o R: a FAC manual coincide com a do `acf()` até erro de arredondamento, porque as duas dividem a autocovariância por n. No console a diferença aparece como 0, já arredondada para 4 casas, e os três gráficos de comparação mostram os pontos do `acf()` sobre as barras da FAC manual.
2. Contra a teoria: no AR(1), a FAC empírica decai como phi^h, mas fica abaixo da teórica nos primeiros lags e oscila perto de zero depois do lag 12. A diferença vem da variação amostral com n = 200. O console imprime a tabela completa dos 21 lags, e estes são alguns deles:

| Lag | FAC empírica | FAC teórica | Diferença |
|---|---|---|---|
| 0 | 1 | 1 | 0 |
| 1 | 0,619 | 0,7 | 0,081 |
| 2 | 0,3959 | 0,49 | 0,0941 |
| 5 | 0,1438 | 0,1681 | 0,0242 |
| 10 | 0,131 | 0,0282 | 0,1028 |
| 20 | -0,1139 | 0,0008 | 0,1147 |

3. Comportamento esperado de cada processo: no ruído branco a média fica perto de zero e a FAC é pequena fora do lag 0. O passeio aleatório tem média 7,2628, longe de zero, porque a variância cresce com t e a trajetória se afasta, e sua FAC cai bem devagar (0,745 no lag 20), o que mostra que não é estacionário. O AR(1) volta para perto de zero e tem decaimento geométrico.
4. Mesma semente, mesmos números: `implementacao_procedural.R` usa a mesma semente e a mesma ordem de sorteios do `main.R`, e imprime as mesmas três médias.
