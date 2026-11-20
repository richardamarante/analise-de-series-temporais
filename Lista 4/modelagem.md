# Modelagem do sistema (Exercício 3)

O programa segue o paradigma procedural, com `main.R` responsável por organizar a simulação, os ajustes, os diagnósticos, as previsões e os gráficos. As funções foram divididas em módulos que recebem os dados necessários e devolvem os resultados para as etapas seguintes.

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

O `main.R` carrega os quatro módulos com `source()`, usando jsonlite na configuração e ggplot2 na visualização. O processamento recebe as matrizes prontas, sem depender diretamente de `dados.R`, enquanto o script de eficiência carrega apenas os três módulos necessários para suas comparações.

## 3.2 (b) Fluxo de dados e matrizes

A configuração lê `parametros.json`, fixa a semente 42 e define os coeficientes e os vetores de tempo. A simulação gera 120 observações e as matrizes `X_tendencia`, `X_fourier` e `X_dummies`, com dimensões 120 × 3, 120 × 4 e 120 × 11. Para cada método, `ajustar_modelo()` recebe a série e as matrizes de tendência e sazonalidade, formando uma matriz completa de 120 × 7 para Fourier ou 120 × 14 para dummies.

O ajuste resolve β̂ = (X'X)⁻¹X'y com `solve(XtX, Xty)`, depois calcula os valores ajustados, a tendência, a sazonalidade e os resíduos, que seguem para os diagnósticos. Na avaliação temporal, o treino usa t = 1 a 72, a validação usa t = 73 a 96 para escolher o menor RMSE e o teste usa t = 97 a 120 após o reajuste com treino e validação. Por fim, as matrizes futuras permitem prever t = 121 a 132, comparando os resultados com o sinal verdadeiro sem ruído antes de enviá-los ao console e aos gráficos.

## 3.2 (c) Sequência procedural de chamadas

O `main.R` carrega os módulos, lê os parâmetros e fixa a semente antes de gerar as matrizes, o ruído e a série simulada. Em seguida, calcula a média e a média móvel centrada, ajusta Fourier e dummies com `ajustar_modelo()`, compara os coeficientes com `lm()` e avalia os resíduos pela FAC e pelos demais diagnósticos.

Na etapa de avaliação, ajusta os dois métodos no treino, escolhe Fourier pelo RMSE da validação e reajusta ambos com treino e validação para calcular RMSE, MAE e R² no teste uma única vez. Depois, constrói as matrizes futuras, chama `prever()` para os 12 períodos seguintes e gera os 12 gráficos. Separadamente, `ex4_eficiencia.R` compara loops com `crossprod()` e `%*%`, verifica os resultados com `all.equal()`, mede os tempos com `system.time()` e salva o gráfico de eficiência.

## 3.3 Por que é procedural

A execução segue uma sequência definida de operações sobre vetores e matrizes, com funções que recebem argumentos, utilizam loops `for`, condicionais e operações matriciais e retornam resultados para as próximas etapas. O Exercício 1 reúne o procedimento em um único script, enquanto os Exercícios 2 e 3 distribuem as funções em módulos sem alterar a lógica de execução. O programa não utiliza classes nem programação orientada a objetos.
