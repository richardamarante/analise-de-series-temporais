# Lista 3 – Análise de Séries Temporais (21/08/2026)

## Objetivo

Reorganizar o script de aula sobre série temporal com tendência polinomial em um projeto modular em R.
O projeto simula Xₜ = β₀ + β₁t + β₂t² + εₜ, calcula a média móvel centrada, a média amostral, a
autocovariância empírica e a FAC empírica, faz os gráficos em ggplot2 e compara as versões com loop e
vetorizadas. As funções têm os nomes do script do professor, e o ruído usa `sigma` (desvio padrão), como
no original.

## Estrutura de pastas

```
Lista 3/
|-- original/
|   |-- Aula_21_08_2026_MM_Decomp_ST_TEND_Ver_01.R
|   \-- Lista_Exe_21_08_2026.pdf
|-- scripts/
|   |-- 01_config.R
|   |-- 02_dados.R
|   |-- 03_processamento.R
|   |-- 04_visualizacao.R
|   \-- 05_eficiencia.R
|-- graficos/
|   \-- serie_tendencia/
|-- eficiencia.R
|-- implementacao_procedural.R
|-- main.R
|-- parametros.json
\-- README.md
```

O `implementacao_procedural.R` é o Exercício 1. Os cinco arquivos de `scripts/` são o Exercício 2 (config,
dados, processamento e visualização) e o 4.3 (eficiência). O `main.R` é o Exercício 3, e os gráficos
integrados do 4.1 saem de `04_visualizacao.R` chamado pelo `main.R`.

## Como executar

Pacotes necessários: `ggplot2`, `jsonlite` e `patchwork`. O `patchwork` só serve para juntar dois
gráficos numa figura; se faltar, `install.packages("patchwork")`.

No RStudio, com a pasta `Lista 3` como diretório de trabalho, rodar `source("main.R")` e, para o Exercício 4.3, `source("eficiencia.R")`. Pela linha de
comando, na mesma pasta:

```
Rscript main.R
Rscript eficiencia.R
Rscript implementacao_procedural.R
```

O `main.R` imprime os parâmetros, a média, a autocovariância de defasagem 1, a FAC e a comparação com a
`acf()`, e grava cinco PNGs em `graficos/serie_tendencia/`. O `eficiencia.R` roda só a comparação entre
loop e vetorizada, que leva alguns segundos por causa dos loops com n = 50000.

Gráficos gerados:

| Arquivo | Conteúdo |
|---|---|
| `01_serie_temporal_tendencia.png` | série com tendência |
| `02_serie_temporal_media_movel_tendencia.png` | série e média móvel na mesma figura |
| `03_funcao_autocorrelacao_empirica_tendencia.png` | FAC empírica com limites de 95% |
| `04_comparacao_fac_empirica_acf_r_tendencia.png` | FAC manual contra `acf()` |
| `05_grafico_integrado_tendencia.png` | série com média móvel em cima e FAC embaixo |

A figura 05 atende ao item 4.1 (gráfico integrado da série, da média móvel e da FAC) e reproduz o
`par(mfrow = c(2,1))` do script do professor.

## Interfaces dos módulos (Exercício 2.3)

Cada função tem, no código, um bloco com a fórmula, a entrada, a saída e a pré-condição. Aqui está o resumo.

### 01_config.R

| Função | Entradas | Saída | Pré-condição |
|---|---|---|---|
| `ler_parametros` | `arquivo` (caminho do json) | lista com os parâmetros | arquivo existe, `jsonlite` carregado |
| `fixar_semente` | `parametros` (com `seed`) | nenhuma, fixa o gerador | `seed` inteiro |
| `criar_tempo` | `parametros` (com `n`) | vetor `t = 1, ..., n` | n inteiro positivo |
| `criar_matriz_regressao` | `parametros`, `t` | matriz X (n x 3), colunas 1, t, t² | length(t) == n |

### 02_dados.R

| Função | Entradas | Saída | Pré-condição |
|---|---|---|---|
| `simular_ruido_branco` | `n`, `sigma` | vetor com n valores de N(0, sigma²) | sigma > 0 |
| `simular_serie_com_tendencia` | `beta`, `X`, `sigma` | vetor Xₜ = Xβ + ε, tamanho n | length(beta) == ncol(X) |

### 03_processamento.R

| Função | Entradas | Saída | Pré-condição |
|---|---|---|---|
| `calcular_media_movel` | `serie`, `q` | vetor com n − 2q médias centradas | 2q + 1 <= n |
| `calcular_media_amostral` | `serie` | média amostral | serie sem NA |
| `calcular_autocovariancia_empirica` | `serie`, `h` | γ(h) | 0 <= h < n |
| `calcular_autocovariancias` | `serie`, `max_lag` | vetor γ(0), ..., γ(max_lag) | 0 <= max_lag < n |
| `calcular_fac_empirica` | `serie`, `max_lag` | vetor ρ(0), ..., ρ(max_lag) | serie não constante |

### 04_visualizacao.R

| Função | Entradas | Saída | Pré-condição |
|---|---|---|---|
| `plot_serie_temporal` | `tempo`, `serie`, `titulo`, `ylab`, `rotulo_serie` | objeto ggplot | mesmo tamanho em tempo e serie |
| `plot_serie_media_movel` | `tempo`, `serie`, `media_movel`, `q`, `titulo`, `ylab` | objeto ggplot | length(media_movel) == n − 2q |
| `plot_fac_empirica` | `fac`, `n`, `titulo` | objeto ggplot com limites de 95% | fac vem de `calcular_fac_empirica` |
| `plot_comparacao_acf_r` | `fac`, `fac_r`, `titulo` | objeto ggplot | mesmo tamanho nos dois vetores |
| `plot_grafico_integrado` | `grafico_serie`, `grafico_fac` | objeto patchwork | `patchwork` carregado |
| `salvar_grafico` | `grafico`, `nome_arquivo`, `parametros`, `fator_altura` | grava o PNG em `graficos/` | `ggplot2` carregado |
| `imprimir_titulo_console` | `titulo` | texto no console | |

### 05_eficiencia.R

| Função | Entradas | Saída | Pré-condição |
|---|---|---|---|
| `criar_matriz_regressao_vetorizada` | `parametros`, `t` | matriz n x 3 com `cbind(1, t, t^2)` | length(t) == n |
| `simular_serie_com_tendencia_vetorizada` | `beta`, `X`, `sigma` | vetor Xₜ = X %*% beta + ε | length(beta) == ncol(X) |
| `calcular_autocovariancia_empirica_vetorizada` | `serie`, `h` | γ(h) | 0 <= h < n |
| `calcular_autocovariancias_vetorizada` | `serie`, `max_lag` | vetor γ(0), ..., γ(max_lag) | 0 <= max_lag < n |
| `calcular_media_movel_vetorizada` | `serie`, `q` | vetor com n − 2q médias | 2q + 1 <= n |
| `comparar_eficiencia` | `parametros` | nenhuma, imprime tempos e igualdade | scripts 01 a 03 carregados |

## Modelagem procedural (Exercício 3.2)

### (a) Arquitetura modular

```
parametros.json
      |
      v
  main.R --> 01_config.R         (parâmetros, semente, t, X)
    |------> 02_dados.R          (ruído branco, série com tendência)
    |------> 03_processamento.R  (média móvel, média, autocovariância, FAC)
    \------> 04_visualizacao.R   (ggplot2, salvar_grafico)

  eficiencia.R --> 01_config.R, 02_dados.R, 03_processamento.R
    \------------> 05_eficiencia.R   (versões vetorizadas e comparação)
```

Só o `main.R` e o `eficiencia.R` chamam `source`. Os módulos não dependem uns dos outros em tempo de
leitura: a `05_eficiencia.R` usa funções de 01 a 03, mas só depois que o `eficiencia.R` carregou todas.
O cálculo (03) não desenha nada, os gráficos (04) não calculam nada e nenhum código está duplicado entre
os módulos.

### (b) Fluxo de dados

```
parametros --> t (n) --> X (n x 3) --+
parametros --> beta (3) -------------+--> Xt (n) --> m_q (n - 2q) ----------+
parametros --> sigma --> eps (n) ----+      |                               |
                                            |--> media_Xt, gamma_1          |
                                            \--> autocovariancias (max_lag + 1)
                                                         |                  |
                                                         v                  |
                                                  fac_Xt (max_lag + 1) -----+--> gráficos
                                                         |
                                                         \--> acf() do R --> comparação
```

### (c) Sequência de chamadas no main.R

```
ler_parametros --> fixar_semente --> criar_tempo --> criar_matriz_regressao
  --> simular_serie_com_tendencia (simular_ruido_branco)
  --> calcular_media_movel
  --> calcular_media_amostral --> calcular_autocovariancia_empirica
  --> calcular_fac_empirica (calcular_autocovariancias)
  --> acf() do R
  --> plot_serie_temporal --> plot_serie_media_movel --> plot_fac_empirica
  --> plot_comparacao_acf_r --> plot_grafico_integrado
```

No `eficiencia.R` a sequência é `ler_parametros` --> `comparar_eficiencia`, que chama cada função com loop e a
sua versão vetorizada.

### Por que é procedural (Exercício 3.3)

Cada função recebe os dados por argumento e devolve um vetor ou um número, sem guardar estado entre as
chamadas. Os dados são só vetores e uma matriz numérica, sem classes nem objetos definidos por nós. O
controle é feito com `for` e `if`, e o `main.R` roda de cima para baixo, uma etapa depois da outra, que é
o que o diagrama de sequência mostra. Não há estado compartilhado entre as funções: o que uma etapa
produz entra na seguinte como argumento.

## Análise de eficiência (Exercício 4.3)

Os gargalos do original são os loops em R. A matriz X é montada linha a linha; a simulação faz um
`rnorm(1)` por instante e um loop interno para o produto X·β (n·p operações); a média móvel soma 2q + 1
valores em cada posição; cada autocovariância percorre a série duas vezes (a média e a soma), e
`calcular_autocovariancias` repete isso para cada defasagem.

As versões vetorizadas usam `cbind(1, t, t^2)` para X, `rnorm(n)` e `X %*% beta` na simulação, `cumsum`
na média móvel e produto de vetores com `sum` na autocovariância. Com a mesma semente os resultados batem
com os do loop (`all.equal`).

Tempos do `eficiencia.R`, com n = 50000, 10 repetições, sigma = 15, q = 3 e max_lag = 20 (total das 10):

| Etapa | Loop (s) | Vetorizada (s) | Razão |
|---|---|---|---|
| Matriz de regressão | 0,12 | 0,02 | 6 |
| Simulação da série | 1,21 | 0,06 | 20,2 |
| Média móvel | 0,45 | 0,04 | 11,2 |
| Autocovariâncias | 1,91 | 0,3 | 6,4 |

O tempo muda de uma execução para outra (numa delas a simulação com loop levou 29,84 s, em outra 1,22 s),
então o que conta é a razão. Na autocovariância o ganho é menor porque o `sapply` sobre as defasagens
ainda é um laço em R (21 chamadas); só o laço sobre t foi eliminado. Com n = 90, o tamanho da série
do `main.R`, a diferença de tempo não se percebe, e o ganho só aparece em série longa.

## Validação dos resultados

1. Mesma semente, mesmos números. O `main.R` e o `implementacao_procedural.R` usam a semente 123 e dão a
   mesma média (30,6849) e a mesma autocovariância de defasagem 1 (588,9894).
2. FAC contra o R. A FAC manual comparada com `acf(Xt, lag.max = 20)` difere por algo da ordem de 1e-16 e
   `all.equal` dá verdadeiro, porque os dois dividem por n e usam a média amostral.
3. Média móvel. Com n = 90 e q = 3 saem n − 2q = 84 valores, e cada um é a média de uma janela de
   2q + 1 = 7 pontos. A curva acompanha a tendência crescente e suaviza o ruído, como se vê no gráfico 02.
4. FAC da série com tendência. Fica alta e cai devagar (0,7285 na defasagem 1 e 0,2910 na 20), e todas as
   barras passam dos limites de 95%. A tendência faz a série ficar longe da média por longos trechos, então
   a série não é estacionária e a FAC não cai rápido como cairia num ruído branco.
5. Versões vetorizadas contra as com loop (n = 50000): `all.equal` verdadeiro na matriz, na simulação, na
   média móvel e nas autocovariâncias.
