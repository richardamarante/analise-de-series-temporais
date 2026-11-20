# Modelagem do sistema (Exercício 3)

O programa segue o paradigma procedural. `main.R` coordena a simulação, os dois ajustes, as previsões e os gráficos; as funções dos módulos recebem os dados e devolvem os resultados.

## 3.2 (a) Arquitetura modular

```text
Lista 4/
├── parametros.json
├── main.R                 Coordena os módulos
├── scripts/
│   ├── config.R           Leitura e configuração
│   ├── dados.R            Matrizes e simulação
│   ├── processamento.R    OLS, diagnóstico e previsão
│   └── visualizacao.R     Gráficos e exportação
├── ex1_implementacao_procedural.R
├── ex4_eficiencia.R       Usa config.R, dados.R e processamento.R
└── graficos/
    ├── decomposicao/
    ├── previsao/
    ├── residuos/
    └── eficiencia/
```

`config.R` utiliza `jsonlite`; `visualizacao.R`, `ggplot2`. O `main.R` carrega os quatro módulos com `source()`. `processamento.R` recebe matrizes prontas e não depende de `dados.R`; `ex4_eficiencia.R` carrega só os três módulos necessários.

## 3.2 (b) Fluxo de dados e matrizes

1. `ler_parametros()` lê `parametros.json`; a configuração fixa `seed = 42`, cria o tempo e os coeficientes. `dados.R` gera $X_t$ com 120 observações, `X_tendencia` (120 × 3), `X_fourier` (120 × 4) e `X_dummies` (120 × 11).
2. `main.R` passa `y`, `X_tendencia` e `X_sazonal` para `ajustar_modelo()`. A matriz $X=[X_{tendencia}\mid X_{sazonal}]$ tem 120 × 7 no Fourier e 120 × 14 nas dummies.
3. `ajustar_ols()` resolve $\hat\beta=(X'X)^{-1}X'y$ com `solve(XtX, Xty)`. `calcular_XtX()` e `calcular_Xty()` obtêm os produtos com loops; `multiplicar_matriz_vetor()` calcula os valores ajustados.
4. `ajustar_modelo()` devolve coeficientes, tendência, sazonalidade, ajustados, resíduos e matriz completa. Essas saídas alimentam os diagnósticos.
5. Na divisão temporal, $t=1:72$ é treino, $t=73:96$ é validação e $t=97:120$ é teste; os modelos são ajustados no treino, avaliados por `avaliar_previsao()` na validação para escolher o menor RMSE e reajustados com treino e validação antes do teste.
6. `prever(modelo, X_tendencia_futuro, X_sazonal_futuro)` calcula $t=121:132$; as previsões são comparadas por RMSE com o sinal verdadeiro sem ruído. Os resultados seguem para console e gráficos.

## 3.2 (c) Sequência procedural de chamadas

1. `main.R` carrega pacotes e módulos, lê os parâmetros e fixa a semente.
2. Cria tempo, matrizes, ruído, série e componentes verdadeiros; calcula média amostral e média móvel centrada.
3. Chama `ajustar_modelo()` para Fourier e dummies; compara os coeficientes com `lm()`.
4. Calcula os diagnósticos e a FAC dos resíduos.
5. Divide a série em treino, validação e teste, ajusta os dois métodos no treino, escolhe Fourier pelo RMSE da validação e reajusta ambos com treino e validação para avaliar o teste uma única vez, calculando RMSE, MAE e R².
6. Constrói matrizes futuras, chama `prever()` e calcula o RMSE fora da amostra.
7. Chama as funções de `visualizacao.R`, apresenta os resultados e salva 12 PNGs em `graficos/`.
8. Separadamente, `ex4_eficiencia.R` compara loops, `crossprod()` e `%*%`, valida com `all.equal()`, mede tempos com `system.time()` e salva o gráfico de eficiência.

## 3.3 Por que é procedural

A execução segue uma ordem definida de operações sobre vetores e matrizes. As funções recebem argumentos, executam loops `for`, operações matriciais e condicionais e retornam resultados usados nas etapas seguintes. `main.R` organiza as chamadas sem classes nem programação orientada a objetos. O Exercício 1 reúne o procedimento em um script; os Exercícios 2 e 3 dividem as funções em módulos, mantendo a mesma lógica.
