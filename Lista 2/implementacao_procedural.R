############################################################
# AULA PRÁTICA – PROCESSOS ESTOCÁSTICOS
# Exercício 1: implementação procedural, sem funções
#
# (a) entradas, com valor inicial e fonte
#   n = 200: tamanho de cada série, fixado no início do script (seção 1)
#   max_lag = 20: maior defasagem h da autocovariância e da FAC, fixada
#   na seção 1 (deve ser menor que n)
#   sigma2 = 1: variância do ruído branco, fixada na seção 1
#   phi = 0.7: coeficiente do AR(1), fixado na seção 1 (|phi| < 1)
#   semente = 123: set.seed(123), na primeira linha depois do rm(), antes
#   de qualquer sorteio
#   nenhuma entrada vem de arquivo ou do usuário: tudo é valor fixo no script
# ---
# (b) saídas
#   séries simuladas: eps (ruído branco), X (passeio), X_ar1 (AR(1))
#   médias empíricas: media_eps, media_X, media_ar1
#   autocovariâncias: gamma_eps, gamma_X, gamma_ar1, de 0 a max_lag
#   FACs: fac_eps, fac_X, fac_ar1 (empíricas) e fac_ar1_teorica
#   terminal: as três médias, uma a uma e no resumo final
#   gráficos: série e FAC de cada processo, FAC empírica x teórica do
#   AR(1) e acf() do R para o ruído branco e o passeio
# ---
# (c) estruturas de dados
#   séries: vetores numéricos de tamanho n, indexados por t = 1, ..., n
#   autocovariâncias e FACs: vetores de tamanho max_lag + 1; a posição
#   h+1 guarda a defasagem h, porque o R indexa a partir de 1
#   lags: o vetor 0:max_lag, usado nos eixos dos gráficos
# ---
# (d) funções
#   simular_ruido_branco: loop de eps no bloco do ruído branco
#   simular_passeio_aleatorio: loop de X no bloco do passeio
#   simular_ar1: loop de X_ar1 no bloco do AR(1)
#   calcular_media_empirica: soma e divisão por n em cada bloco
#   calcular_autocovariancia_empirica e calcular_autocovariancias:
#   o loop duplo (h por fora, t por dentro) de cada bloco
#   calcular_fac_empirica: divisão por gamma(0) logo depois
#   calcular_fac_teorica_ar1: loop de phi^h no bloco do AR(1)
#   plot_serie_temporal, plot_fac_empirica, plot_fac_comparacao:
#   plot() e barplot() escritos direto em cada bloco de gráficos
# ---
# (e) fluxo de execução
#   parâmetros e semente -> ruído branco (série, média, FAC, gráficos)
#   -> passeio aleatório (série, média, FAC, gráficos)
#   -> acf() do R para os dois -> AR(1) (série, média, FAC empírica,
#   FAC teórica, gráficos) -> resumo das médias no terminal
############################################################

rm(list = ls())
set.seed(123)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n <- 200

max_lag <- 20

sigma2 <- 1

phi <- 0.7

############################################################
# 5. EXEMPLOS DE USO
############################################################

# ruído branco: eps_t ~ N(0, sigma2)
# o loop percorre t = 1, ..., n e sorteia um valor independente por passo
eps <- numeric(n)

for(t in 1:n){
  eps[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

# média empírica: o loop acumula os n valores em soma
soma <- 0

for(t in 1:n){
  soma <- soma + eps[t]
}

media_eps <- soma / n

cat("Média empírica (ruído branco):", round(media_eps, 4), "\n")

# autocovariâncias: o loop de fora percorre os lags h = 0, ..., max_lag;
# o de dentro soma (X_t - média)(X_{t+h} - média) para t = 1, ..., n-h,
# e t para em n-h para que t+h não passe do fim da série. o lag 0
# cai no mesmo loop e dá a variância, porque d1 e d2 são iguais
gamma_eps <- numeric(max_lag + 1)

for(h in 0:max_lag){

  soma <- 0

  for(t in 1:(n-h)){
    d1 <- eps[t] - media_eps
    d2 <- eps[t+h] - media_eps
    soma <- soma + d1 * d2
  }

  gamma_eps[h+1] <- soma / n
}

# FAC empírica: rho(h) = gamma(h) / gamma(0), um lag por passo
fac_eps <- numeric(max_lag + 1)

for(h in 0:max_lag){
  fac_eps[h+1] <- gamma_eps[h+1] / gamma_eps[1]
}

par(mfrow = c(2,1))

plot(1:n,
     eps,
     type = "l",
     lwd = 2,
     col = "gray30",
     main = "Ruído Branco",
     xlab = "Tempo (t)",
     ylab = "eps_t")

abline(h = 0,
       col = "red",
       lty = 2)

barplot(fac_eps,
        names.arg = 0:max_lag,
        col = "gray70",
        main = "FAC Empírica – Ruído Branco",
        xlab = "Lag (h)",
        ylab = "rho_hat")

par(mfrow = c(1,1))

# passeio aleatório: X_t = X_{t-1} + eps_t
# o passeio usa um ruído novo, sorteado depois do ruído branco
eps_pa <- numeric(n)

for(t in 1:n){
  eps_pa[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

X <- numeric(n)

# condição inicial: não existe X_0, então X_1 = eps_1
X[1] <- eps_pa[1]

# loop de recorrência temporal: em cada passo t o valor anterior X[t-1]
# é lido, o choque novo eps_pa[t] é somado e o resultado vira X[t]. o
# passo seguinte usa esse X[t] como "anterior", então o loop precisa ir
# em ordem crescente de t. no fim, X[t] = eps_1 + ... + eps_t, e por
# isso a variância cresce com t e a série não volta para a média
for(t in 2:n){
  X[t] <- X[t-1] + eps_pa[t]
}

soma <- 0

for(t in 1:n){
  soma <- soma + X[t]
}

media_X <- soma / n

cat("Média empírica (passeio aleatório):", round(media_X, 4), "\n")

# autocovariâncias do passeio, com o mesmo loop duplo (h por fora, t por dentro)
gamma_X <- numeric(max_lag + 1)

for(h in 0:max_lag){

  soma <- 0

  for(t in 1:(n-h)){
    d1 <- X[t] - media_X
    d2 <- X[t+h] - media_X
    soma <- soma + d1 * d2
  }

  gamma_X[h+1] <- soma / n
}

fac_X <- numeric(max_lag + 1)

for(h in 0:max_lag){
  fac_X[h+1] <- gamma_X[h+1] / gamma_X[1]
}

par(mfrow = c(2,1))

plot(1:n,
     X,
     type = "l",
     lwd = 2,
     col = "blue",
     main = "Passeio Aleatório",
     xlab = "Tempo (t)",
     ylab = "X_t")

abline(h = 0,
       col = "red",
       lty = 2)

barplot(fac_X,
        names.arg = 0:max_lag,
        col = "lightblue",
        main = "FAC Empírica – Passeio Aleatório",
        xlab = "Lag (h)",
        ylab = "rho_hat")

par(mfrow = c(1,1))

# comparação com a função built-in do R
par(mfrow = c(2,1))

acf(eps,
    main = "ACF (R) – Ruído Branco")

acf(X,
    main = "ACF (R) – Passeio Aleatório")

par(mfrow = c(1,1))

# processo AR(1): X_t = phi * X_{t-1} + eps_t, com ruído novo
eps_ar1 <- numeric(n)

for(t in 1:n){
  eps_ar1[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

X_ar1 <- numeric(n)

# condição inicial
X_ar1[1] <- eps_ar1[1]

# loop de recorrência temporal: igual ao do passeio, mas o valor anterior
# entra multiplicado por phi antes de somar o choque. com |phi| < 1 a
# influência do passado encolhe a cada passo (phi, phi^2, phi^3, ...) e
# a série volta para perto de zero, ou seja, é estacionária. com phi = 1
# seria o passeio aleatório
for(t in 2:n){
  X_ar1[t] <- phi * X_ar1[t-1] + eps_ar1[t]
}

soma <- 0

for(t in 1:n){
  soma <- soma + X_ar1[t]
}

media_ar1 <- soma / n

cat("Média empírica (AR(1)):", round(media_ar1, 4), "\n")

gamma_ar1 <- numeric(max_lag + 1)

for(h in 0:max_lag){

  soma <- 0

  for(t in 1:(n-h)){
    d1 <- X_ar1[t] - media_ar1
    d2 <- X_ar1[t+h] - media_ar1
    soma <- soma + d1 * d2
  }

  gamma_ar1[h+1] <- soma / n
}

fac_ar1 <- numeric(max_lag + 1)

for(h in 0:max_lag){
  fac_ar1[h+1] <- gamma_ar1[h+1] / gamma_ar1[1]
}

# FAC teórica do AR(1): rho(h) = phi^h
fac_ar1_teorica <- numeric(max_lag + 1)

for(h in 0:max_lag){
  fac_ar1_teorica[h+1] <- phi^h
}

par(mfrow = c(2,1))

plot(1:n,
     X_ar1,
     type = "l",
     lwd = 2,
     col = "forestgreen",
     main = paste("Processo AR(1) Estacionário (phi =", phi, ")"),
     xlab = "Tempo (t)",
     ylab = "X_t")

abline(h = 0,
       col = "red",
       lty = 2)

barplot(fac_ar1,
        names.arg = 0:max_lag,
        ylim = c(0, 1),
        col = "lightgreen",
        main = "FAC Empírica vs Teórica – AR(1)",
        xlab = "Lag (h)",
        ylab = "rho(h)")

lines(0:max_lag,
      fac_ar1_teorica,
      type = "b",
      pch = 16,
      col = "darkgreen",
      lwd = 2)

legend("topright",
       legend = c("FAC Empírica",
                  "FAC Teórica"),
       col = c("lightgreen",
               "darkgreen"),
       lwd = c(2, 2),
       pch = c(NA, 16),
       bty = "n")

par(mfrow = c(1,1))

############################################################
# 6. RESUMO DOS RESULTADOS
############################################################

cat("\n")
cat("========================================\n")
cat("RESUMO DAS MÉDIAS EMPÍRICAS\n")
cat("========================================\n")

cat("Ruído branco:", round(media_eps, 4), "\n")

cat("Passeio aleatório:", round(media_X, 4), "\n")

cat("AR(1):", round(media_ar1, 4), "\n")
