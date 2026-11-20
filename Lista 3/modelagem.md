# Modelagem do sistema (Exercício 3)

O programa segue o paradigma procedural, com as funções divididas em módulos conforme a etapa do processamento. O `main.R` carrega esses módulos e controla a execução.

## 3.2 (a) Arquitetura modular

```text
main.R
|-- scripts/config.R
|   |-- parametros.json
|   |-- Parâmetros, semente, tempo e matriz X
|-- scripts/dados.R
|   |-- Tendência, ruído e série
|-- scripts/processamento.R
|   |-- Média móvel, média, autocovariâncias e FAC
|-- scripts/visualizacao.R
|   |-- Gráficos e salvamento
```

O `main.R` carrega os quatro módulos com `source()`, sem que eles precisem carregar uns aos outros. O `ex4_eficiencia.R` utiliza apenas os módulos de configuração, dados e processamento.

## 3.2 (b) Fluxo de dados

Os parâmetros são lidos de `parametros.json` e utilizados para definir a semente, o vetor de tempo e a matriz X, formada pelas colunas 1, t e t². Com a matriz e os coeficientes, o programa calcula a tendência verdadeira e acrescenta o ruído branco para obter a série Xt.

A série é utilizada no cálculo da média móvel centrada, da média amostral, das autocovariâncias com divisor n e da FAC empírica. Depois da comparação com `acf()`, os resultados seguem para o módulo de visualização, que constrói os gráficos e os salva conforme as configurações.

## 3.2 (c) Sequência procedural de chamadas

O `main.R` começa limpando o ambiente, carregando ggplot2, jsonlite e patchwork e fazendo `source()` dos quatro módulos. Em seguida, lê os parâmetros, fixa a semente e prepara o tempo e a matriz de regressão para simular a série e calcular a média móvel.

Com os dados simulados, chama as funções de média amostral, autocovariância e FAC, comparando esta última com `acf()` por meio da maior diferença absoluta e de `all.equal()`. Por fim, chama as funções de visualização para exibir e salvar os cinco gráficos.

## 3.3 Por que é procedural

A implementação é procedural porque o `main.R` determina a ordem das operações e utiliza os resultados retornados pelas funções, que recebem os dados por argumentos. O programa trabalha com variáveis, vetores, matrizes e uma lista de parâmetros, utilizando loops `for` e condições `if`, sem definir classes próprias ou métodos orientados a objetos.

A divisão em módulos separa configuração, simulação, processamento e visualização, mantendo cada etapa organizada sem alterar o fluxo sequencial do programa.
