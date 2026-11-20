# Lista 4: Decomposição clássica de séries temporais em R (28/08/2026)

Análise de Séries Temporais (GET00127)

## Objetivo

Simular uma série temporal com **tendência polinomial** e **sazonalidade**, estimar os coeficientes por **mínimos quadrados ordinários (OLS)** usando as equações normais, analisar os resíduos, realizar testes de hipóteses, **prever 12 períodos** à frente e representar a decomposição aditiva em gráficos com ggplot2. O ajuste é feito com harmônicos de Fourier e com variáveis dummies sazonais, permitindo comparar os dois métodos.

O modelo utilizado é:

$$ X_t = \beta_0 + \beta_1t + \beta_2t^2 + \sum_{k=1}^{K}\left[\alpha_k\cos\left(\frac{2\pi kt}{s}\right)+b_k\operatorname{sen}\left(\frac{2\pi kt}{s}\right)\right]+\varepsilon_t $$

O ruído branco segue uma distribuição normal com média zero e variância \(\sigma^2\).

## Arquivos

| Arquivo | Exercício | Conteúdo |
|---|---|---|
| `ex1_implementacao_procedural.R` | 1 | implementação completa em um único script procedural, com parâmetros, matrizes, simulação, OLS, diagnóstico, testes, previsão e gráficos do R base |
| `parametros.json` | 2 | parâmetros da simulação, dos testes, da previsão, da eficiência e da exportação dos gráficos |
| `scripts/config.R` | 2 | leitura dos parâmetros, semente, vetor de tempo e coeficientes sazonais |
| `scripts/dados.R` | 2 | matrizes de tendência, Fourier e dummies, ruído branco e simulação da série |
| `scripts/processamento.R` | 2 | média móvel, operações matriciais, OLS, diagnóstico, testes t e de Box-Pierce e previsão |
| `scripts/visualizacao.R` | 2 e 4 | gráficos com ggplot2, configuração visual e exportação dos PNGs |
| `main.R` | 3 | execução completa: configuração, simulação, estimação, diagnóstico, previsão e visualização |
| `ex4_eficiencia.R` | 4 | comparação entre operações matriciais com loops e versões vetorizadas, com validação pelo `lm()` |
| `Lista 4.pdf` | | enunciado da lista |
| `README.md` | | descrição, modelagem e resultados da implementação |

Os arquivos de `scripts/` contêm as funções de cada etapa. O `main.R` carrega esses módulos e executa o procedimento na ordem definida. Os parâmetros ficam em `parametros.json`, na raiz da lista, e são carregados pela função `ler_parametros()` de `config.R`.

As entradas, saídas e pré-condições das funções estão documentadas nos comentários dos arquivos.

## Como executar

É necessário ter o R instalado e os pacotes ggplot2 e jsonlite.

```r
install.packages(c("ggplot2", "jsonlite"))
```

No RStudio, com a pasta da lista como diretório de trabalho, executar:

```r
source("main.R")
source("ex4_eficiencia.R")
```

O `main.R` executa a simulação e o ajuste dos modelos, imprime os coeficientes, diagnósticos, testes e previsões no console, apresenta os gráficos e salva nove arquivos PNG em `graficos/`.

O `ex4_eficiencia.R` compara as operações matriciais com loops `for` e as versões vetorizadas, verifica a igualdade dos resultados e imprime os tempos de execução.

O Exercício 1 também pode ser executado separadamente:

```r
source("ex1_implementacao_procedural.R")
```

Esse arquivo contém a implementação em um único script, sem divisão em módulos. Os cálculos são feitos diretamente com variáveis, vetores, matrizes e loops.

### Parâmetros utilizados

Os valores definidos em `parametros.json` são:

| Parâmetro | Valor | Descrição |
|---|---|---|
| `seed` | 42 | semente da simulação |
| `n_anos` | 10 | número de anos simulados |
| `s` | 12 | período sazonal |
| `sigma` | 5 | desvio padrão do ruído branco |
| `grau_tendencia` | 2 | grau da tendência polinomial |
| `beta_tendencia` | (2, 0.05, 0.001) | coeficientes verdadeiros da tendência |
| `alpha` | (2, 1) | coeficientes dos cossenos |
| `beta_h` | (-1, 0.5) | coeficientes dos senos |
| `K` | 2 | número de harmônicos de Fourier |
| `q` | 5 | raio da média móvel centrada |
| `max_lag_box` | 12 | número de defasagens do teste de Box-Pierce |
| `n_futuro` | 12 | períodos previstos fora da amostra |
| `n_eficiencia` | 200000 | tamanho da amostra usada na comparação de tempos |
| `repeticoes_eficiencia` | 10 | repetições de cada operação |

A série tem \(n = 10 \times 12 = 120\) observações. A tendência é quadrática, a sazonalidade tem dois harmônicos e o ruído possui variância \(\sigma^2 = 25\).

A média móvel centrada utiliza \(2q+1=11\) observações em cada janela.

Os gráficos são salvos em PNG com largura de 10, altura de 6 e resolução de 300 dpi.

### Gráficos gerados

O `main.R` salva os nove gráficos nas seguintes pastas:

| Arquivo | Conteúdo |
|---|---|
| `graficos/decomposicao/01_decomposicao_aditiva.png` | série observada, tendência, sazonalidade e resíduos em quatro painéis |
| `graficos/decomposicao/02_serie_observada_ajustada.png` | comparação da série observada com os valores ajustados |
| `graficos/decomposicao/03_tendencia_estimada_real.png` | tendência estimada e tendência verdadeira |
| `graficos/decomposicao/04_sazonalidade_estimada_real.png` | sazonalidade estimada e sazonalidade verdadeira |
| `graficos/decomposicao/05_serie_media_movel_tendencia.png` | série observada, média móvel centrada e tendência estimada |
| `graficos/previsao/01_previsao_fora_amostra.png` | série observada, previsão de Fourier e sinal verdadeiro futuro |
| `graficos/residuos/01_serie_temporal_residuos.png` | resíduos ao longo do tempo |
| `graficos/residuos/02_funcao_autocorrelacao_residuos.png` | FAC amostral dos resíduos |
| `graficos/residuos/03_histograma_residuos.png` | histograma dos resíduos com densidade normal de variância 25 |

As funções retornam objetos ggplot. A decomposição usa `facet_wrap()` para organizar os quatro componentes, cada um com sua escala vertical. A exportação é feita por `salvar_grafico()`, que cria as subpastas necessárias e utiliza `ggsave()`.

## Fourier x dummies

Os dois métodos representam a sazonalidade com período \(s=12\), mas utilizam matrizes diferentes.

**Fourier:** a função `criar_matriz_fourier()` constrói duas colunas por harmônico, uma de cossenos e outra de senos. Com \(K=2\), são quatro colunas sazonais. O método representa um padrão periódico suave com poucos parâmetros. A condição \(K<s/2\) evita uma coluna de senos nula, que deixaria a matriz do modelo sem posto completo.

**Dummies:** a função `criar_matriz_dummies()` cria uma coluna indicadora para cada mês, exceto o primeiro. São \(s-1=11\) colunas, com o mês 1 como referência. Cada mês recebe um efeito próprio, sem a restrição de formar uma curva suave. A exclusão de uma coluna impede a dependência linear entre as dummies e o intercepto.

Com a tendência quadrática, os modelos possuem:

| Característica | Fourier | Dummies |
|---|---|---|
| Colunas de tendência | 3 | 3 |
| Colunas sazonais | 4 | 11 |
| Total de parâmetros | 7 | 14 |
| Representação sazonal | harmônicos de seno e cosseno | indicadores mensais |
| Método de estimação | OLS | OLS |

O `main.R` ajusta os dois modelos com `ajustar_modelo()`, calcula seus diagnósticos e compara as previsões com o sinal verdadeiro sem ruído.

## Modelagem do sistema (Exercício 3)

O programa segue o paradigma **procedural**, com funções que recebem vetores, matrizes e parâmetros, realizam os cálculos e devolvem os resultados. A execução é coordenada pelo `main.R`, que chama as etapas em sequência.

### 3.2 (a) Arquitetura modular

A implementação foi dividida em quatro módulos dentro de `scripts/`, além do arquivo principal e do arquivo de parâmetros.

```text
                       +----------------------+
                       |        main.R        |
                       |----------------------|
                       | Coordena a execução  |
                       | e imprime resultados |
                       +----------------------+
                                  |
              +-------------------+-------------------+
              |                   |                   |
              v                   v                   v
       +-------------+     +-------------+     +------------------+
       | config.R    |     | dados.R     |     | processamento.R  |
       |-------------|     |-------------|     |------------------|
       | ler_        |     | criar_      |     | calcular_media_   |
       | parametros  |     | matriz_     |     | movel             |
       | fixar_      |     | tendencia   |     | calcular_media_   |
       | semente     |     | criar_      |     | amostral          |
       | criar_tempo |     | matriz_     |     | multiplicar_      |
       | criar_beta_ |     | fourier     |     | matriz_vetor      |
       | sazonal     |     | criar_      |     | calcular_XtX      |
       |             |     | matriz_     |     | calcular_Xty      |
       |             |     | dummies     |     | ajustar_ols       |
       |             |     | simular_    |     | ajustar_modelo    |
       |             |     | ruido_      |     | diagnostico       |
       |             |     | branco      |     | teste_t_          |
       |             |     | simular_    |     | coeficientes      |
       |             |     | serie       |     | teste_box_pierce  |
       |             |     |             |     | prever            |
       +-------------+     +-------------+     +------------------+
              ^                   ^                   ^
              |                   |                   |
       parametros.json           +-------------------+
                                  dependencia
                                  de funcoes

                       +----------------------+
                       |   visualizacao.R     |
                       |----------------------|
                       | tema_graficos        |
                       | ajustar_eixos       |
                       | salvar_grafico       |
                       | imprimir_titulo_     |
                       | console              |
                       | plot_decomposicao    |
                       | plot_serie_ajustada  |
                       | plot_tendencia       |
                       | plot_sazonalidade    |
                       | plot_serie_media_    |
                       | movel                |
                       | plot_residuos        |
                       | plot_acf             |
                       | plot_histograma      |
                       | plot_previsao        |
                       +----------------------+
                                  ^
                                  |
                                main.R
```

| Arquivo | Responsabilidade | Dependências |
|---|---|---|
| `config.R` | carregar parâmetros, fixar a semente e construir os vetores de tempo e coeficientes sazonais | jsonlite |
| `dados.R` | construir as matrizes do modelo e simular o ruído e a série | nenhuma |
| `processamento.R` | calcular média móvel, estimar coeficientes, separar componentes, realizar diagnósticos e testes e calcular previsões | funções de `dados.R` |
| `visualizacao.R` | criar, formatar e salvar gráficos e imprimir títulos no console | ggplot2 |
| `main.R` | carregar os módulos e executar as etapas na ordem correta | os quatro módulos |
| `ex4_eficiencia.R` | comparar as operações matriciais e validar o OLS | `config.R`, `dados.R` e `processamento.R` |

O `main.R` utiliza `source()` para carregar os quatro módulos. O `ex4_eficiencia.R` carrega apenas os três módulos necessários para os cálculos.

A separação permite utilizar as funções de estimação sem gerar gráficos. O módulo de processamento constrói as matrizes necessárias por meio das funções de `dados.R`, e o módulo de visualização recebe os resultados já calculados para produzir as figuras.

### 3.2 (b) Fluxo de dados e matrizes

O processamento segue esta ordem:

```text
Configuração
     |
     v
Simulação e construção das matrizes
     |
     v
Estimação OLS
     |
     v
Diagnóstico e testes
     |
     v
Previsão
     |
     v
Visualização
```

**1. Configuração**

O arquivo `parametros.json` contém os valores utilizados pelo programa. A função `ler_parametros()` carrega esses valores, `fixar_semente()` define a semente e `criar_tempo()` constrói o vetor \(t=1,\ldots,120\).

A função `criar_beta_sazonal()` intercala os coeficientes dos cossenos e senos na mesma ordem das colunas da matriz de Fourier.

```text
parametros.json
       |
       v
config.R
       |
       +-- ler_parametros() ------> parametros
       |
       +-- fixar_semente() -------> seed = 42
       |
       +-- criar_tempo() ---------> t (120)
       |
       +-- criar_beta_sazonal() --> beta_sazonal (4)
```

**2. Simulação e matrizes**

O módulo `dados.R` constrói as matrizes que representam as componentes da série:

```text
t (120), grau_tendencia = 2, s = 12, K = 2
       |
       +-- criar_matriz_tendencia(t, 2)
       |          |
       |          v
       |     X_tendencia (120 x 3)
       |
       +-- criar_matriz_fourier(t, 12, 2)
                  |
                  v
             X_sazonal (120 x 4)

X_tendencia + beta_tendencia
X_sazonal   + beta_sazonal
sigma = 5
       |
       v
simular_serie()
       |
       +-- simular_ruido_branco()
       |
       v
Xt (120 observações)
```

A série simulada é a soma da tendência verdadeira, da sazonalidade verdadeira e do ruído branco. Os componentes verdadeiros são calculados separadamente com `multiplicar_matriz_vetor()` para comparação com os estimados.

**3. Estimação OLS**

A função `ajustar_modelo()` recebe o tempo, a série observada, o grau da tendência, o período sazonal, o número de harmônicos e o método de sazonalidade.

Ela constrói a matriz completa do modelo:

$$ X = [X_{\text{tendencia}} \mid X_{\text{sazonal}] $$

A estimação é feita pelas equações normais:

$$ \hat{\beta}=(X'X)^{-1}X'y $$

As operações são implementadas por funções próprias. `calcular_XtX()` utiliza três loops para construir \(X'X\), e `calcular_Xty()` utiliza dois loops para calcular \(X'y\). A função `ajustar_ols()` usa `solve(XtX, Xty)` para resolver o sistema linear, sem calcular explicitamente a inversa.

```text
ajustar_modelo(t, Xt, grau_tendencia, periodo, K, metodo_sazonal)
       |
       +-- criar_matriz_tendencia()
       |
       +-- criar_matriz_fourier()
       |           ou
       +-- criar_matriz_dummies()
       |
       v
X_full = cbind(X_tendencia, X_sazonal)
       |
       v
ajustar_ols(X_full, Xt)
       |
       +-- calcular_XtX(X_full) ----> XtX (p x p)
       |
       +-- calcular_Xty(X_full, Xt) -> Xty (p)
       |
       +-- solve(XtX, Xty)
       |
       v
beta_hat (p)
       |
       +-- multiplicar_matriz_vetor()
       |
       v
y_hat, tendencia, sazonalidade
       |
       v
residuos = Xt - y_hat
       |
       v
modelo (lista)
```

A matriz de Fourier possui 120 linhas e 7 colunas quando combinada com a tendência. A matriz com dummies possui 120 linhas e 14 colunas.

O resultado de `ajustar_modelo()` é uma lista com os coeficientes estimados, valores ajustados, componentes da decomposição, resíduos, matriz completa e argumentos necessários para a previsão.

**4. Diagnóstico e testes**

A função `diagnostico()` recebe a série e os resíduos e devolve média, variância, RMSE e \(R^2\).

O diagnóstico é realizado para os modelos de Fourier e dummies. Para o modelo de Fourier, também são aplicados os testes de hipóteses:

- `teste_t_coeficientes()`: testa \(H_0:\beta_j=0\) para cada coeficiente, usando a estatística t e seus valores-p.
- `teste_box_pierce()`: testa a ausência de autocorrelação dos resíduos até a defasagem 12, usando a estatística \(Q\) e o valor-p da distribuição qui-quadrado.

Os coeficientes estimados manualmente e os valores ajustados também são comparados com os resultados de `lm()`.

**5. Previsão**

A previsão é calculada para \(t=121,\ldots,132\).

A função `prever()` recebe o modelo ajustado e o vetor de tempos futuros. Ela constrói as matrizes futuras de tendência e sazonalidade, mantendo o método usado no ajuste, e multiplica a matriz completa pelos coeficientes estimados.

```text
t_futuro = 121:132
       |
       +-- prever(modelo, t_futuro)
       |          |
       |          v
       |     previsoes Fourier (12)
       |
       +-- prever(modelo_dummies, t_futuro)
                  |
                  v
             previsoes Dummies (12)

criar_matriz_tendencia(t_futuro, 2)
criar_matriz_fourier(t_futuro, 12, 2)
       |
       v
multiplicar_matriz_vetor()
       |
       v
sinal_futuro (12)
       |
       v
diagnostico()
       |
       v
RMSE das previsões
```

O sinal futuro é calculado com os coeficientes verdadeiros de tendência e Fourier, sem adicionar ruído. O RMSE mede a diferença entre as previsões de cada modelo e esse sinal.

**6. Visualização**

As funções de `visualizacao.R` recebem os vetores calculados nas etapas anteriores e retornam objetos ggplot.

```text
Série Xt
Modelo Fourier
Componentes verdadeiros
Média móvel
Resíduos
Previsões
       |
       v
visualizacao.R
       |
       +-- plot_decomposicao()
       +-- plot_serie_ajustada()
       +-- plot_tendencia()
       +-- plot_sazonalidade()
       +-- plot_serie_media_movel()
       +-- plot_residuos()
       +-- plot_acf()
       +-- plot_histograma()
       +-- plot_previsao()
       |
       v
Objetos ggplot
       |
       v
salvar_grafico()
       |
       v
graficos/decomposicao/
graficos/residuos/
graficos/previsao/
```

O `main.R` imprime cada gráfico e chama `salvar_grafico()`. A função utiliza os parâmetros de tamanho e resolução definidos no JSON.

### 3.2 (c) Sequência procedural de chamadas

O `main.R` coordena as chamadas nesta sequência:

```text
main.R
  |
  |-- rm(list = ls())
  |-- library(ggplot2)
  |-- library(jsonlite)
  |
  |-- source("scripts/config.R")
  |-- source("scripts/dados.R")
  |-- source("scripts/processamento.R")
  |-- source("scripts/visualizacao.R")
  |
  |-- ler_parametros("parametros.json")
  |-- fixar_semente(parametros)
  |-- criar_tempo(parametros)
  |-- criar_beta_sazonal(alpha, beta_h, K)
  |
  | CONFIGURAÇÃO CONCLUÍDA
  |
  |-- criar_matriz_tendencia(t, grau_tendencia)
  |-- criar_matriz_fourier(t, s, K)
  |-- simular_serie(...)
  |       |
  |       +-- simular_ruido_branco(n, sigma)
  |
  |-- multiplicar_matriz_vetor() -> tendencia_real
  |-- multiplicar_matriz_vetor() -> sazonalidade_real
  |-- calcular_media_amostral(Xt)
  |
  | SIMULAÇÃO CONCLUÍDA
  |
  |-- calcular_media_movel(Xt, q)
  |
  |-- ajustar_modelo(..., metodo_sazonal = "fourier")
  |       |
  |       +-- criar_matriz_tendencia()
  |       +-- criar_matriz_fourier()
  |       +-- ajustar_ols()
  |               |
  |               +-- calcular_XtX()
  |               +-- calcular_Xty()
  |               +-- solve()
  |       |
  |       +-- multiplicar_matriz_vetor()
  |       +-- calcula residuos
  |       |
  |       +--> modelo
  |
  |-- ajustar_modelo(..., metodo_sazonal = "dummies")
  |       |
  |       +-- criar_matriz_tendencia()
  |       +-- criar_matriz_dummies()
  |       +-- ajustar_ols()
  |       +-- multiplicar_matriz_vetor()
  |       |
  |       +--> modelo_dummies
  |
  | ESTIMAÇÃO CONCLUÍDA
  |
  |-- diagnostico(Xt, modelo$residuos)
  |-- diagnostico(Xt, modelo_dummies$residuos)
  |
  |-- teste_t_coeficientes(modelo)
  |-- teste_box_pierce(modelo$residuos, 12)
  |
  |-- lm(Xt ~ modelo$X_full - 1)
  |-- compara coeficientes e valores ajustados
  |
  | DIAGNÓSTICO CONCLUÍDO
  |
  |-- t_futuro = 121:132
  |
  |-- prever(modelo, t_futuro)
  |-- prever(modelo_dummies, t_futuro)
  |
  |-- criar_matriz_tendencia(t_futuro, grau_tendencia)
  |-- criar_matriz_fourier(t_futuro, s, K)
  |-- multiplicar_matriz_vetor() -> sinal_futuro
  |
  |-- diagnostico() -> rmse_previsao_fourier
  |-- diagnostico() -> rmse_previsao_dummies
  |
  | PREVISÃO CONCLUÍDA
  |
  |-- plot_decomposicao()
  |-- plot_serie_ajustada()
  |-- plot_tendencia()
  |-- plot_sazonalidade()
  |-- plot_serie_media_movel()
  |-- plot_residuos()
  |-- plot_acf()
  |-- plot_histograma()
  |-- plot_previsao()
  |
  |-- print() para cada gráfico
  |-- salvar_grafico() para cada gráfico
  |
  v
FIM
```

O script `ex4_eficiencia.R` utiliza as mesmas funções para gerar a série e construir a matriz do modelo. Ele compara `ajustar_ols()` com `ajustar_ols_vetorizada()` e com `lm()`, depois mede os tempos das operações `calcular_XtX()`, `calcular_Xty()` e `multiplicar_matriz_vetor()` em suas versões com loops e vetorizadas.

### 3.3 Por que é procedural

A implementação é procedural porque o programa executa uma sequência definida de operações sobre variáveis, vetores e matrizes.

- As funções recebem os dados por argumento e devolvem vetores, matrizes, listas, data frames ou gráficos, conforme a operação.
- Os cálculos matriciais manuais utilizam loops `for`, e as escolhas de método são feitas com estruturas condicionais.
- O `main.R` controla a ordem das etapas, desde a leitura dos parâmetros até a geração dos gráficos.
- Os resultados de cada etapa são armazenados em variáveis e utilizados nas chamadas seguintes.
- A divisão em módulos organiza as funções por responsabilidade, sem utilizar classes ou programação orientada a objetos.

O Exercício 1 apresenta o mesmo procedimento diretamente em um único script. O Exercício 2 separa os cálculos em funções e módulos, e o Exercício 3 organiza as chamadas no `main.R`, mantendo o paradigma procedural.

## Validação dos resultados

### 1. Simulação e coeficientes

A simulação utiliza `seed = 42` e gera uma série com 120 observações, média amostral de 10.0334, três colunas de tendência e quatro colunas de Fourier.

Os coeficientes verdadeiros e os estimados pelo OLS são:

| Coeficiente | Verdadeiro | Estimado |
|---|---:|---:|
| Intercepto | 2 | 2.8646 |
| Tendência linear | 0.05 | 0.0236 |
| Tendência quadrática | 0.001 | 0.0012 |
| Cosseno 1 | 2 | 1.3523 |
| Seno 1 | -1 | -1.6327 |
| Cosseno 2 | 1 | 0.7060 |
| Seno 2 | 0.5 | 0.0716 |

A tendência estimada acompanha a verdadeira, principalmente pelo coeficiente quadrático, que ficou próximo de 0.001. Os coeficientes sazonais apresentam diferenças maiores, pois o desvio padrão do ruído (5) é alto em relação às amplitudes dos harmônicos.

### 2. Comparação com `lm()`

Os coeficientes calculados manualmente pelas equações normais foram comparados com os obtidos por `lm()`.

A maior diferença entre os coeficientes, arredondada a quatro casas decimais, foi 0. As verificações com `all.equal()` retornaram `TRUE` para os coeficientes e para os valores ajustados. Isso confirma que a implementação manual reproduz os resultados do OLS do R dentro da tolerância numérica.

### 3. Diagnóstico dos resíduos

Os resultados dos dois modelos foram:

| Medida | Fourier | Dummies |
|---|---:|---:|
| Número de parâmetros | 7 | 14 |
| Média dos resíduos | 0 | 0 |
| Variância dos resíduos | 26.2615 | 24.5124 |
| RMSE | 5.1032 | 4.9303 |
| R² | 0.5979 | 0.6247 |

A média dos resíduos foi 0 nos dois ajustes, como esperado no OLS com intercepto. As variâncias ficaram próximas de \(\sigma^2=25\), utilizada na simulação.

O modelo com dummies apresentou R² de 0.6247, contra 0.5979 do modelo de Fourier. O RMSE também foi menor, 4.9303 contra 5.1032. As dummies utilizam 11 parâmetros sazonais, enquanto Fourier utiliza apenas quatro, permitindo um ajuste mais livre para cada mês.

### 4. Testes de hipóteses

A função `teste_t_coeficientes()` calcula a estatística t e o valor-p de cada coeficiente do modelo de Fourier, considerando a hipótese nula \(H_0:\beta_j=0\).

| Coeficiente | Estatística t | Valor-p |
|---|---:|---:|
| 1 | 1.9533 | 0.0533 |
| 2 | 0.4227 | 0.6733 |
| 3 | 2.6396 | 0.0095 |
| 4 | 1.9913 | 0.0489 |
| 5 | -2.3978 | 0.0181 |
| 6 | 1.0397 | 0.3007 |
| 7 | 0.1054 | 0.9162 |

Ao nível de significância de 5%, os coeficientes 3, 4 e 5 apresentaram valores-p inferiores a 0.05. Para esses coeficientes, a hipótese nula foi rejeitada.

O teste de Box-Pierce foi aplicado aos resíduos do modelo de Fourier com 12 defasagens. A estatística obtida foi \(Q=8.3199\), com valor-p de 0.7597. Como o valor-p é maior que 0.05, a hipótese nula de ausência de autocorrelação não foi rejeitada.

A função de autocorrelação amostral dos resíduos também foi representada em gráfico, com bandas de referência de \(\pm 1.96/\sqrt{n}\).

### 5. Previsão fora da amostra

Os modelos foram utilizados para prever os períodos de 121 a 132. As previsões foram comparadas com a soma da tendência e da sazonalidade verdadeiras, sem ruído.

| Medida | Fourier | Dummies |
|---|---:|---:|
| Períodos previstos | 12 | 12 |
| RMSE da previsão | 0.8775 | 1.5812 |

No primeiro período previsto, \(t=121\), Fourier produziu 23.7829 e dummies produziu 24.3790, enquanto o sinal verdadeiro foi 24.8561. No último período, \(t=132\), as previsões foram 28.6178 e 30.6464, respectivamente, contra o valor verdadeiro de 29.0240.

O modelo de Fourier apresentou menor RMSE fora da amostra, mesmo tendo R² menor no ajuste. Como a série foi gerada com harmônicos de Fourier, o modelo utiliza a mesma representação sazonal do processo simulado.

O RMSE de 0.8775 também ficou abaixo do desvio padrão do ruído (5), indicando que as previsões de Fourier ficaram próximas do sinal verdadeiro sem ruído.

## Análise de eficiência (Exercício 4.3)

### Gargalos dos loops `for`

As operações matriciais do OLS foram implementadas manualmente com loops em `processamento.R`.

- `calcular_XtX()` utiliza três loops aninhados para calcular os produtos internos entre as colunas de X. O custo cresce aproximadamente com \(np^2\), sendo \(n\) o número de linhas e \(p\) o número de colunas.
- `calcular_Xty()` utiliza dois loops para calcular o produto de cada coluna de X com o vetor y. O custo é aproximadamente \(np\).
- `multiplicar_matriz_vetor()` percorre as linhas e colunas da matriz para calcular \(X\beta\), também com custo aproximado de \(np\).

Essas operações executam muitas multiplicações e somas diretamente nos loops do R. O custo aumenta com o número de observações e com a quantidade de parâmetros do modelo.

### Melhoria com operações vetorizadas

O arquivo `ex4_eficiencia.R` implementa três versões vetorizadas:

| Operação | Implementação com loops | Implementação vetorizada |
|---|---|---|
| X'X | `calcular_XtX(X)` | `crossprod(X)` |
| X'y | `calcular_Xty(X, y)` | `as.numeric(crossprod(X, y))` |
| Xβ | `multiplicar_matriz_vetor(X, beta)` | `as.numeric(X %*% beta)` |

A função `crossprod()` calcula diretamente os produtos matriciais sem precisar construir a transposta de X. O operador `%*%` realiza o produto entre matriz e vetor usando rotinas compiladas do R.

A função `ajustar_ols_vetorizada()` utiliza as duas versões de `crossprod()` para construir as equações normais e continua resolvendo o sistema com `solve(XtX, Xty)`.

Cada operação preserva a mesma soma de produtos da versão com loops. A mudança está na forma de executar os cálculos, sem alterar o procedimento matemático.

### Validação da implementação vetorizada

A primeira comparação foi feita com a série de 120 observações, utilizando os mesmos parâmetros da simulação.

| Verificação | Resultado |
|---|---|
| Coeficientes iguais nas duas versões | `TRUE` |
| Coeficientes dos loops iguais aos de `lm()` | `TRUE` |
| Coeficientes vetorizados iguais aos de `lm()` | `TRUE` |

As três verificações utilizaram `all.equal()`, confirmando a igualdade numérica dos coeficientes dentro da tolerância do R.

### Comparação dos tempos

Para medir o desempenho, foi construída uma matriz com 200000 linhas e sete colunas. Cada operação foi executada dez vezes, e o tempo total foi medido com `system.time()`.

| Operação | Loops `for` | Vetorizada | Ganho |
|---|---:|---:|---:|
| X'X | 8.67 s | 0.11 s | 78.8x |
| X'y | 1.06 s | 0.05 s | 21.2x |
| Xβ | 1.64 s | 0.05 s | 32.8x |

A maior diferença apareceu no cálculo de X'X, que passou de 8.67 segundos para 0.11 segundo, ficando aproximadamente 79 vezes mais rápido.

O cálculo de X'y apresentou ganho de 21.2 vezes, e a multiplicação matriz-vetor ficou 32.8 vezes mais rápida.

As três comparações de resultados retornaram `TRUE`, mostrando que as versões com loops e vetorizadas produziram resultados numericamente iguais dentro da tolerância utilizada.

Com 120 observações, o ajuste manual já é viável. A comparação com 200000 linhas mostra que as operações vetorizadas reduzem bastante o tempo de execução em séries maiores. Os tempos dependem da máquina utilizada.

## Por que usar ggplot2 (Exercício 4.2)

O ggplot2 foi utilizado para manter a mesma organização visual nos gráficos da série, tendência, sazonalidade, resíduos e previsão. As funções de `visualizacao.R` compartilham o tema definido por `tema_graficos()` e utilizam `ajustar_eixos()` para controlar o espaço adicional nas escalas.

A função `plot_decomposicao()` utiliza `facet_wrap()` para apresentar os quatro componentes da decomposição aditiva em painéis com o mesmo eixo de tempo e escalas verticais independentes. Os outros gráficos permitem comparar diretamente os valores observados, estimados e verdadeiros, com legendas para identificar cada linha.

Os resíduos são apresentados em uma série temporal, em um gráfico da FAC e em um histograma com a densidade normal de variância 25. A previsão é representada junto à série observada e ao sinal verdadeiro futuro.

A função `salvar_grafico()` centraliza a exportação com `ggsave()`, utilizando os parâmetros definidos em `parametros.json`. Os nove gráficos são salvos em PNG com resolução de 300 dpi, separados nas pastas `decomposicao/`, `residuos/` e `previsao/`.
