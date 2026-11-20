# Lista 4 – Análise de Séries Temporais (28/08/2026)

## Objetivo

O script `original/Aula_28_08_2026_Decomp_ST.R` simula uma série com tendência polinomial e sazonalidade, estima o modelo por OLS com as equações normais escritas à mão, faz o diagnóstico dos resíduos e prevê 12 períodos. Aqui ele foi reorganizado em um projeto modular, com os parâmetros em um arquivo JSON, os gráficos em ggplot2, dois testes de hipóteses (teste t dos coeficientes e Box-Pierce nos resíduos) e uma comparação de tempo entre os loops e as operações matriciais do R. As funções e a lógica do script original foram mantidas, e o que era variável global virou argumento.

O modelo é X_t = β0 + β1 t + β2 t² + Σ_k [α_k cos(2πkt/s) + β_k sen(2πkt/s)] + ε_t, com ε_t ~ N(0, σ²). O ajuste usa a série inteira, sem divisão em treino e teste, como no script do professor, e a previsão de 12 passos é conferida contra o sinal verdadeiro (tendência mais sazonalidade, sem o ruído).

## Estrutura de pastas

```
Lista 4/
|-- original/
|   |-- Aula_28_08_2026_Decomp_ST.R
|   \-- Lista_Exe_28_08_2026.pdf
|-- scripts/
|   |-- 01_config.R
|   |-- 02_dados.R
|   |-- 03_processamento.R
|   |-- 04_visualizacao.R
|   \-- 05_eficiencia.R
|-- graficos/
|   |-- decomposicao/
|   |-- residuos/
|   \-- previsao/
|-- implementacao_procedural.R
|-- main.R
|-- eficiencia.R
|-- parametros.json
\-- README.md
```

## Como executar

No RStudio, com a pasta `Lista 4` como diretório de trabalho:

```r
source("main.R")        # pipeline do PDF: simulação, OLS, diagnóstico, previsão e gráficos
source("eficiencia.R")  # exercício 4.3: loops contra operações vetorizadas
```

Os pacotes são `ggplot2` e `jsonlite`. Os parâmetros (semente 42, `n_anos`, `s`, `sigma`, coeficientes, `K`, `q`, `max_lag_box`, tamanho e repetições da eficiência) ficam em `parametros.json`. Os gráficos saem em `graficos/`, em PNG de 300 dpi. O `implementacao_procedural.R` roda sozinho, sem os módulos, e usa os gráficos do R base.

## Interfaces dos módulos (Exercício 2.3)

Cada função tem, acima dela no código, um comentário com o que faz, a entrada, a saída e, quando existe, a pré-condição. As tabelas abaixo resumem o mesmo conteúdo.

### 01_config.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `ler_parametros` | `arquivo` (caminho de um .json) | lista com os parâmetros | campos de `parametros.json` |
| `fixar_semente` | `parametros` (campo `seed`) | nenhuma, só fixa o gerador | `seed` inteiro |
| `criar_tempo` | `parametros` (usa `n_anos` e `s`) | vetor `t = 1, ..., n` | `n_anos` e `s` inteiros positivos |
| `criar_beta_sazonal` | `alpha`, `beta_h` (vetores de tamanho K), `K` | vetor de tamanho 2K: (α1, β1, ..., αK, βK) | `length(alpha) == length(beta_h) == K` |

### 02_dados.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `criar_matriz_tendencia` | `t`, `grau` | matriz n x (grau + 1) com t^0, ..., t^grau | `grau >= 0` |
| `criar_matriz_fourier` | `t`, `periodo`, `K` | matriz n x 2K (cos e sen de cada harmônico) | `K < periodo/2` |
| `criar_matriz_dummies` | `t` (inteiros), `periodo` | matriz n x (periodo - 1), mês 1 é a referência | `periodo >= 2` |
| `simular_ruido_branco` | `n`, `sigma` | vetor `eps` de tamanho n | `sigma > 0` |
| `simular_serie` | `beta_tendencia`, `X_tendencia`, `X_sazonal`, `beta_sazonal`, `sigma` | vetor `Xt` de tamanho n | colunas das matrizes iguais ao tamanho dos betas |

### 03_processamento.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `calcular_media_movel` | `serie`, `q` | vetor de tamanho n - 2q | `2q + 1 <= n` |
| `calcular_media_amostral` | `serie` | escalar | sem NA |
| `multiplicar_matriz_vetor` | `X` (n x p), `beta` (p) | vetor de tamanho n | `ncol(X) == length(beta)` |
| `calcular_XtX` | `X` (n x p) | matriz p x p | `X` numérica |
| `calcular_Xty` | `X` (n x p), `y` (n) | vetor de tamanho p | `nrow(X) == length(y)` |
| `ajustar_ols` | `X`, `y` | `beta_hat` (p) | X'X invertível |
| `ajustar_modelo` | `t`, `y`, `grau_tendencia`, `periodo`, `K`, `metodo_sazonal` | lista: `beta_hat`, `y_hat`, `tendencia`, `sazonalidade`, `residuos`, `X_full` e os argumentos do ajuste | `n` maior que o número de colunas; método "fourier" ou "dummies" |
| `diagnostico` | `y`, `residuos` | lista: `media`, `variancia`, `rmse`, `r2` | `n >= 2`, `y` não constante |
| `prever` | `modelo`, `t_futuro` | vetor de previsões | `t_futuro` depois do fim da amostra |
| `teste_t_coeficientes` | `modelo` | data.frame: `beta_hat`, `erro_padrao`, `estatistica_t`, `valor_p` | X'X invertível, `n > p` |
| `teste_box_pierce` | `residuos`, `max_lag` | lista: `Q`, `valor_p` | `1 <= max_lag < n` |

### 04_visualizacao.R

Todas devolvem um objeto ggplot e só recebem vetores prontos, sem chamar nenhuma função de estimação.

| Função | Entrada | Pré-condição |
|---|---|---|
| `plot_serie_ajustada` | `t`, `serie`, `ajustada` | mesmo tamanho |
| `plot_tendencia` | `t`, `tendencia_estimada`, `tendencia_real` (opcional) | mesmo tamanho |
| `plot_sazonalidade` | `t`, `sazonalidade_estimada`, `sazonalidade_real` (opcional) | mesmo tamanho |
| `plot_residuos` | `t`, `residuos` | mesmo tamanho |
| `plot_serie_media_movel` | `t`, `serie`, `media_movel`, `tendencia`, `q` | `length(media_movel) == n - 2q` |
| `plot_decomposicao` | `t`, `serie`, `tendencia`, `sazonalidade`, `residuos` | série = tendência + sazonalidade + resíduos |
| `plot_previsao` | `t`, `serie`, `t_futuro`, `previsoes`, `sinal_futuro` (opcional) | `length(t_futuro) == length(previsoes)` |
| `plot_acf` | `serie`, `titulo` | sem NA |
| `plot_histograma` | `serie`, `titulo`, `xlab`, `sigma2` | `sigma2 > 0` |
| `salvar_grafico` | `grafico`, `nome_arquivo`, `parametros` | `parametros$salvar_graficos` definido |

O arquivo ainda tem `tema_graficos`, `ajustar_eixos` e `imprimir_titulo_console`.

### 05_eficiencia.R

| Função | Entrada | Saída | Pré-condição |
|---|---|---|---|
| `calcular_XtX_vetorizada` | `X` | matriz p x p (`crossprod`) | igual a `calcular_XtX` |
| `calcular_Xty_vetorizada` | `X`, `y` | vetor de tamanho p (`crossprod(X, y)`) | igual a `calcular_Xty` |
| `multiplicar_matriz_vetor_vetorizada` | `X`, `beta` | vetor de tamanho n (`X %*% beta`) | igual a `multiplicar_matriz_vetor` |
| `ajustar_ols_vetorizada` | `X`, `y` | `beta_hat` | X'X invertível |
| `comparar_eficiencia` | `parametros` | nenhuma, imprime no console | `parametros` lido por `ler_parametros` |

## Modelagem procedural (Exercício 3.2)

### Arquitetura modular

Cada seta vai de um arquivo para o que ele usa.

```
main.R --> 01_config.R, 02_dados.R, 03_processamento.R, 04_visualizacao.R
eficiencia.R --> 01_config.R, 02_dados.R, 03_processamento.R, 05_eficiencia.R

01_config.R --> (nenhum módulo)
02_dados.R --> (nenhum módulo)
03_processamento.R --> 02_dados.R   (ajustar_modelo e prever montam as matrizes)
04_visualizacao.R --> (nenhum módulo)
05_eficiencia.R --> 01_config.R, 02_dados.R, 03_processamento.R
```

A visualização só recebe vetores, e a estimação não desenha nada. Por isso os gráficos podem mudar sem mexer no OLS.

### Fluxo de dados e matrizes

```
parametros.json --> parametros --> t (n)
t --> X_tendencia (n x 3) e X_sazonal (n x 2K)
X_tendencia, X_sazonal, beta, sigma --> Xt (n)
Xt --> média móvel (n - 2q)
t, Xt --> X_full (n x p) --> XtX (p x p) e Xty (p) --> beta_hat (p)
X_full, beta_hat --> y_hat, tendência, sazonalidade (n)
Xt, y_hat --> resíduos (n) --> diagnóstico (média, variância, RMSE, R²)
modelo --> testes: t (p linhas) e Box-Pierce (Q, valor-p)
beta_hat, t_futuro --> X_futura (12 x p) --> previsões (12)
previsões, sinal futuro --> RMSE da previsão
vetores --> plot_* --> graficos/
```

### Sequência de chamadas no main.R

```
ler_parametros --> fixar_semente --> criar_tempo --> criar_beta_sazonal
--> criar_matriz_tendencia --> criar_matriz_fourier --> simular_serie
--> calcular_media_movel
--> ajustar_modelo (criar_matriz_*, ajustar_ols, multiplicar_matriz_vetor), com Fourier e com dummies
--> diagnostico --> teste_t_coeficientes --> teste_box_pierce
--> prever (Fourier e dummies) --> diagnostico (RMSE da previsão)
--> plot_decomposicao --> plot_serie_ajustada --> plot_tendencia --> plot_sazonalidade
--> plot_serie_media_movel --> plot_residuos --> plot_acf --> plot_histograma --> plot_previsao
```

### Por que é procedural (Exercício 3.3)

Nada no projeto guarda estado: cada função recebe vetores e matrizes por argumento e devolve outro vetor, outra matriz ou uma lista. Não há classes nem objetos com métodos. A ordem do pipeline é a do texto do `main.R`, de cima para baixo, e o controle de fluxo é feito com `for` e `if`. O trabalho todo é transformar estruturas tabulares: `t` vira matriz de regressores, as matrizes viram `X'X` e `X'y`, e estes viram `beta_hat`, que por sua vez vira ajustados, resíduos e previsões. A lista devolvida por `ajustar_modelo` é só um pacote de vetores que viaja até o diagnóstico e a previsão.

## Fourier e dummies (Exercício 1.3)

Fourier: s_t é a soma, para k de 1 a K, de α_k cos(2πkt/s) + β_k sen(2πkt/s). Cada harmônico k é uma onda de período s/k e ocupa duas colunas, então a matriz tem 2K colunas (4 com K = 2). A curva é suave e tem poucos parâmetros. Exige K < s/2, porque com K = s/2 a coluna do seno do último harmônico é zero em t inteiro e X'X fica singular. Montar a matriz custa n K chamadas de cos e de sen.

Dummies: s_t = γ_m, com m = ((t-1) mod s) + 1 e γ_1 = 0. Usa s − 1 colunas (11 aqui). O mês 1 é a referência: com as s colunas, a soma delas repetiria a coluna de 1 do intercepto e X'X não teria inversa. Cada mês tem um nível livre, sem forma suave, e montar a matriz custa um resto de divisão e uma atribuição por linha.

Na estimação, X'X faz n p² multiplicações e o `solve` custa da ordem de p³. Com tendência de grau 2, p = 7 (Fourier) ou p = 14 (dummies), então X'X custa cerca de 4 vezes mais e o `solve` cerca de 8 vezes mais com dummies. A série simulada tem sazonalidade de Fourier com K = 2, e a comparação na mesma série foi esta:

| Método | Parâmetros | Variância dos resíduos | RMSE | R² | RMSE da previsão |
|---|---|---|---|---|---|
| Fourier | 7 | 26,2615 | 5,1032 | 0,5979 | 0,8775 |
| Dummies | 14 | 24,5124 | 4,9303 | 0,6247 | 1,5812 |

As dummies ajustam melhor dentro da amostra porque têm o dobro de parâmetros, e esse ajuste extra vem do ruído. Fora da amostra elas erram quase o dobro (1,5812 contra 0,8775).

## Gráficos com ggplot2 (Exercícios 4.1 e 4.2)

A decomposição aditiva tem quatro componentes em escalas muito diferentes (a série vai de −10 a 35, a sazonalidade de −3 a 3). Com `facet_wrap(~ componente, ncol = 1, scales = "free_y")` os quatro ficam no mesmo gráfico, com o mesmo eixo de tempo e cada um na sua escala em y, o que no R base exigia `par(mfrow)` e eixos ajustados à mão. A legenda e as cores saem do mapeamento dentro do `aes`, e todos os gráficos usam o mesmo tema, então ficam comparáveis. Os objetos ggplot também podem ser guardados e ajustados depois, por exemplo para acrescentar a tendência verdadeira por cima da estimada. O `salvar_grafico` grava cada um em PNG de 10 x 6 polegadas a 300 dpi (3000 x 1800 pixels), com os valores vindos do `parametros.json`.

## Análise de eficiência (Exercício 4.3)

Gargalo: `calcular_XtX` tem três loops aninhados (p, p e n), ou seja n p² passos interpretados pelo R, e `calcular_Xty` e `multiplicar_matriz_vetor` têm n p passos, cada um com indexação `X[k,i]` e atribuição. O custo cresce linearmente com n e com o quadrado do número de colunas. Alternativa em `05_eficiencia.R`: `crossprod(X)`, `crossprod(X, y)` e `X %*% beta`, que chamam código compilado (BLAS) sobre a matriz inteira. O `solve` é o mesmo nas duas versões, e cada elemento continua sendo a mesma soma de produtos.

O `eficiencia.R` faz duas coisas. Primeiro confere a exatidão com o n da série (120): o OLS com loops, o OLS vetorizado e o `lm()` deram coeficientes iguais. Depois mede X'X, X'y e X·beta com n = 200000 e 7 colunas, sem o `solve`, em 10 repetições (tempo total em segundos, `system.time`). O `solve` fica de fora porque com t² chegando a 4e10, X'X fica mal condicionada em n grande.

| Operação | Loops | Vetorizada | Razão |
|---|---|---|---|
| X'X | 10,61 | 0,14 | 75,8 |
| X'y | 1,46 | 0,05 | 29,2 |
| X·beta | 2,14 | 0,06 | 35,7 |

Esses são os tempos de uma execução. O tempo muda de uma execução para outra e de uma máquina para outra, e a razão é o que conta: a versão vetorizada foi de 29 a 76 vezes mais rápida nas três operações. Os resultados coincidiram nas duas versões em todas elas (`all.equal` verdadeiro). Com n = 120 a diferença de tempo não se percebe, e o ganho só aparece em série longa.

## Validação dos resultados

Semente 42, n = 120, σ = 5, tendência (2; 0,05; 0,001) e sazonalidade com α = (2; 1) e β = (−1; 0,5).

- **OLS manual contra o R:** os coeficientes e os valores ajustados do `lm(Xt ~ modelo$X_full - 1)` coincidem com os do OLS manual (`all.equal` verdadeiro, maior diferença 0 no arredondamento de 4 casas). O `implementacao_procedural.R` imprime os mesmos números do `main.R`.
- **Coeficientes (verdadeiro e estimado):** 2 e 2,8646; 0,05 e 0,0236; 0,001 e 0,0012; 2 e 1,3523; −1 e −1,6327; 1 e 0,7060; 0,5 e 0,0716. Os da tendência ficam perto dos verdadeiros e a tendência estimada acompanha a real no gráfico. Os sazonais ficam mais longe porque σ = 5 é grande perto da amplitude da sazonalidade (coeficientes de 0,5 a 2), então o ruído esconde boa parte do sinal em 120 observações.
- **Resíduos:** média da ordem de 1e-15, como esperado em um OLS com intercepto. A variância é 26,2615, perto de σ² = 25. RMSE de 5,1032 e R² de 0,5979.
- **Previsão:** o RMSE contra o sinal sem ruído é 0,8775, bem abaixo do desvio do ruído (5). Ele é baixo porque a comparação é com tendência mais sazonalidade, sem o ε dos 12 passos, que ninguém consegue prever.
- **Fourier e dummies:** tabela da seção anterior.

### Testes de hipóteses (modelo de Fourier)

O teste t de cada coeficiente usa s² = Σe²/(n − p), erro padrão √(s² [(X'X)⁻¹]_jj), t = β̂_j/erro padrão e valor-p = 2(1 − P(T ≤ |t|)), com n − p = 113 graus de liberdade. Com 5% de nível, rejeitam H0: β_j = 0 o coeficiente de t² (t = 2,6396; valor-p 0,0095), o do cos de k = 1 (t = 1,9913; valor-p 0,0489) e o do sen de k = 1 (t = −2,3978; valor-p 0,0181). O intercepto fica no limite (valor-p 0,0533), e o termo linear (0,6733), o cos de k = 2 (0,3007) e o sen de k = 2 (0,9162) não são significativos, o que combina com os coeficientes pequenos do segundo harmônico.

O teste de Box-Pierce usa Q = n Σ ρ̂(h)², para h de 1 a 12, com ρ̂(h) = γ̂(h)/γ̂(0) calculado à mão (autocovariância com divisor n). Deu Q = 8,3199 e valor-p = 0,7597 com 12 graus de liberdade, então não há evidência de autocorrelação nos resíduos, que é o que se espera de um ruído branco simulado. Os dois testes foram escritos com loops, sem pacote.
