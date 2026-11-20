# Modelagem do sistema (Exercício 3)

O sistema é procedural. O `main.R` coordena a execução, e os módulos em `scripts/` separam configuração, simulação, processamento e visualização.

## 3.2 (a) Arquitetura dos módulos

```text
main.R
├── scripts/config.R
│   ├── ler_parametros() ← parametros.json
│   └── fixar_semente()
├── scripts/dados.R
│   ├── simular_ruido_branco()
│   ├── acumular_passeio_aleatorio()
│   ├── simular_passeio_aleatorio()
│   └── simular_ar1()
├── scripts/processamento.R
│   ├── calcular_media_empirica()
│   ├── calcular_autocovariancia_empirica()
│   ├── calcular_autocovariancias()
│   ├── calcular_fac_empirica()
│   └── calcular_fac_teorica_ar1()
└── scripts/visualizacao.R
    └── gráficos, print() e salvar_grafico()

ex1_implementacao_procedural.R (execução independente)
ex4_eficiencia.R (carrega config.R, dados.R e processamento.R)
```

O `main.R` carrega os quatro módulos com `source()`. Os módulos não carregam outros arquivos. O `parametros.json` contém `seed`, `n`, `max_lag`, `sigma2`, `phi`, `n_eficiencia`, `repeticoes_eficiencia`, `n_grafico_eficiencia` e opções de salvamento.

## 3.2 (b) Dependências e fluxo de dados

1. `scripts/config.R` lê `parametros.json` com `ler_parametros()` e fixa a semente com `fixar_semente()`.
2. `scripts/dados.R` gera `eps`, `X` e `X_ar1`, com `n = 200`, `sigma2 = 1` e `phi = 0.7`.
3. `scripts/processamento.R` calcula as médias, autocovariâncias e FACs para as defasagens de 0 a `max_lag = 20`.
4. `main.R` compara as FACs manuais com `acf()` e a FAC empírica do AR(1) com a teórica, imprimindo os resultados.
5. `scripts/visualizacao.R` exibe e salva os gráficos em `graficos/ruido_branco/`, `graficos/passeio_aleatorio/` e `graficos/ar1/`.

## 3.2 (c) Sequência de chamadas do AR(1)

1. `main.R` carrega os módulos, chama `ler_parametros("parametros.json")` e `fixar_semente(parametros)`.
2. `simular_ar1(n, phi, sigma2)` chama `simular_ruido_branco()`, define `X[1] = eps[1]` e calcula `X[t] = phi * X[t - 1] + eps[t]` para `t = 2, ..., n`.
3. `calcular_media_empirica(X_ar1)` retorna a média; `calcular_fac_empirica(X_ar1, max_lag)` usa `calcular_autocovariancias()` e divide cada autocovariância por γ(0).
4. `calcular_fac_teorica_ar1(phi, max_lag)` calcula ρ(h) = φ^h. `main.R` compara as FACs e chama `acf(X_ar1, lag.max = max_lag, plot = FALSE)`.
5. As funções de visualização geram quatro gráficos: série temporal, FAC empírica, comparação com a FAC teórica e comparação com `acf()`; `print()` e `salvar_grafico()` exibem e salvam os PNGs em `graficos/ar1/`.

## 3.3 Por que é procedural

O programa segue etapas sequenciais controladas por `main.R`. As funções recebem argumentos e retornam valores ou vetores, sem classes ou herança. As séries têm tamanho `n`, e os vetores de autocovariâncias e FACs têm `max_lag + 1` posições, com a defasagem `h` na posição `h + 1`. Os loops `for` calculam as recorrências e autocovariâncias, e as condições `if` controlam casos específicos. Os módulos são separados por responsabilidade, mantendo a passagem de dados explícita.
