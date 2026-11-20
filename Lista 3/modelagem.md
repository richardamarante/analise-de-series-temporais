# Modelagem do sistema (Exercício 3)

O programa segue o paradigma procedural. O `main.R` carrega os módulos e executa as etapas em sequência.

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

O `main.R` faz `source()` dos quatro módulos, que não carregam uns aos outros. O `ex4_eficiencia.R` carrega apenas `config.R`, `dados.R` e `processamento.R`.

## 3.2 (b) Fluxo de dados

1. `ler_parametros()` lê `parametros.json`; `fixar_semente()`, `criar_tempo()` e `criar_matriz_regressao()` preparam os dados. A matriz X contém 1, t e t².
2. `simular_serie_com_tendencia()` usa `calcular_tendencia(beta, X)` e `simular_ruido_branco()` para gerar a série Xt e a tendência verdadeira.
3. `calcular_media_movel()` produz a média móvel centrada. `calcular_media_amostral()`, `calcular_autocovariancia_empirica()` e `calcular_fac_empirica()` calculam as estatísticas de Xt, com autocovariância dividida por n.
4. O `main.R` compara a FAC com `acf()`. `visualizacao.R` recebe os resultados e `salvar_grafico()` grava os PNGs conforme os parâmetros.

## 3.2 (c) Sequência procedural de chamadas

1. O `main.R` limpa o ambiente, carrega ggplot2, jsonlite e patchwork e faz `source()` dos quatro módulos.
2. Executa `ler_parametros()`, `fixar_semente()`, `criar_tempo()` e `criar_matriz_regressao()`.
3. Chama `simular_serie_com_tendencia()`, que utiliza `calcular_tendencia()` e `simular_ruido_branco()`, e depois `calcular_media_movel()`.
4. Chama `calcular_media_amostral()`, `calcular_autocovariancia_empirica()` e `calcular_fac_empirica()`, que utiliza `calcular_autocovariancias()`. Compara os resultados com `acf()`, `max(abs(fac_Xt - fac_R))` e `all.equal()`.
5. Gera gráficos com `plot_serie_temporal()`, `plot_serie_media_movel()`, `plot_fac_empirica()`, `plot_comparacao_acf_r()` e `plot_grafico_integrado()`, exibindo e salvando com `salvar_grafico()`.

## 3.3 Por que é procedural

1. O `main.R` controla a ordem das etapas e utiliza os resultados retornados pelas funções.
2. Os dados são guardados em variáveis, vetores, matrizes e uma lista de parâmetros. As funções recebem argumentos e retornam resultados.
3. A implementação usa loops `for` e condições `if`, sem classes próprias ou métodos orientados a objetos. `fixar_semente()` configura o gerador aleatório.
4. Cada módulo reúne funções de uma etapa, separando configuração, simulação, processamento e visualização.
