# Modelagem do sistema (Exercício 3)

O sistema é procedural e tem sua execução coordenada por `main.R`, que carrega os módulos responsáveis pela configuração, simulação, processamento e visualização. A separação permite organizar as etapas sem alterar o fluxo sequencial do programa.

## 3.2 (a) Arquitetura dos módulos

```text
main.R
├── scripts/config.R ← parametros.json
├── scripts/dados.R
│   ├── Ruído branco
│   ├── Passeio aleatório
│   └── AR(1)
├── scripts/processamento.R
│   ├── Média e autocovariâncias
│   └── FACs empírica e teórica
└── scripts/visualizacao.R
    └── Gráficos e exibição

ex1_implementacao_procedural.R (execução independente)
ex4_eficiencia.R (usa configuração, dados e processamento)
```

O `main.R` carrega os quatro módulos com `source()`, sem que eles precisem carregar outros arquivos. Os parâmetros de entrada ficam em `parametros.json`, incluindo os valores das simulações e as opções da análise de eficiência.

## 3.2 (b) Dependências e fluxo de dados

A execução começa com a leitura dos parâmetros e a definição da semente, passando para a simulação do ruído branco, do passeio aleatório e do AR(1), que gera os vetores `eps`, `X` e `X_ar1` com `n = 200`, `sigma2 = 1` e `phi = 0.7`. Em seguida, o processamento calcula as médias, autocovariâncias e FACs para as defasagens de 0 a `max_lag = 20`, que são comparadas no `main.R` com `acf()` e, no caso do AR(1), com a FAC teórica. Os resultados são impressos no console, e os gráficos são exibidos e salvos nas respectivas pastas dentro de `graficos/`.

## 3.2 (c) Sequência de chamadas do AR(1)

Depois de carregar os módulos e configurar os parâmetros, o `main.R` chama `simular_ar1()`, que gera os choques com `simular_ruido_branco()` e define `X[1] = eps[1]`. Os valores seguintes são calculados pela recorrência `X[t] = phi * X[t - 1] + eps[t]`, para `t = 2, ..., n`, antes de passar a série para o cálculo da média e da FAC empírica.

A FAC empírica é obtida pelas autocovariâncias divididas por γ(0), enquanto a teórica usa ρ(h) = φ^h. O `main.R` compara os resultados com a teoria e com `acf(X_ar1, lag.max = max_lag, plot = FALSE)`, depois chama as funções de visualização para exibir e salvar quatro gráficos do AR(1), incluindo a série temporal e as comparações entre as FACs.

## 3.3 Por que é procedural

O programa segue etapas sequenciais controladas por `main.R`, com funções que recebem argumentos e retornam valores ou vetores, sem utilizar classes ou herança. As séries têm tamanho `n`, enquanto os vetores de autocovariâncias e FACs têm `max_lag + 1` posições, de modo que a defasagem `h` ocupa a posição `h + 1`. Os loops `for` são usados nas recorrências e no cálculo das autocovariâncias, e as condições `if` tratam casos específicos, mantendo a passagem de dados explícita entre os módulos.
