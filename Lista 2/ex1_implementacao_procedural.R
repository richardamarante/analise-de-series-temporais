############################################################
# ex1_implementacao_procedural.R
# Simula ruído branco, passeio aleatório e AR(1) com implementação procedural, sem funções.
# Dependências: R base.

# (a) ENTRADAS
# n = 200: tamanho das séries.
# max_lag = 20: maior defasagem da autocovariância e da FAC, com max_lag < n.
# sigma2 = 1: variância do ruído branco.
# phi = 0.7: coeficiente do AR(1), com |phi| < 1.
# semente = 123: set.seed(123) após rm() e antes dos sorteios.
# Todos os valores são definidos no próprio script, sem entrada externa.

# (b) SAÍDAS
# Séries: eps (ruído branco), X (passeio aleatório), X_ar1 (AR(1)).
# Médias: media_eps, media_X, media_ar1.
# Autocovariâncias: gamma_eps, gamma_X, gamma_ar1, dos lags 0 a max_lag.
# FACs: fac_eps, fac_X, fac_ar1 e fac_ar1_teorica.
# Terminal: médias individuais e resumo final.
# Gráficos: séries, FACs, comparação do AR(1) e acf() do R para ruído branco e passeio.

# (c) ESTRUTURAS DE DADOS
# Séries: vetores numéricos de tamanho n, com t = 1, ..., n.
# Autocovariâncias e FACs: vetores de tamanho max_lag + 1, com o lag h na posição h+1.
# Lags: vetor 0:max_lag, usado nos gráficos.

# (d) FUNÇÕES REPRESENTADAS PELOS BLOCOS PROCEDURAIS
# simular_ruido_branco: sorteio de eps.
# simular_passeio_aleatorio: sorteio de eps_pa e loop de X com acumular_passeio_aleatorio.
# simular_ar1: loop de X_ar1.
# calcular_media_empirica: soma dos valores e divisão por n.
# calcular_autocovariancia_empirica e calcular_autocovariancias: loops de h e t.
# calcular_fac_empirica: divisão das autocovariâncias por gamma(0).
# calcular_fac_teorica_ar1: cálculo de phi^h.
# plot_serie_temporal, plot_fac_empirica e plot_fac_comparacao: chamadas de plot() e barplot().

# (e) FLUXO DE EXECUÇÃO
# 1. Define parâmetros e semente.
# 2. Simula ruído branco, calcula média e FAC e gera gráficos.
# 3. Simula passeio aleatório, calcula média e FAC e gera gráficos.
# 4. Compara as FACs do ruído branco e do passeio com acf() do R.
# 5. Simula AR(1), calcula média, FAC empírica e teórica e gera gráficos.
# 6. Imprime o resumo das médias.
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

# RUÍDO BRANCO: eps_t ~ N(0, sigma2).
# Sorteia n valores independentes, um por vez.
eps <- numeric(n)

for(t in 1:n){
  eps[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

# MÉDIA EMPÍRICA: soma os n valores e divide por n.
soma <- 0

for(t in 1:n){
  soma <- soma + eps[t]
}

media_eps <- soma / n

cat("Média empírica (ruído branco):", round(media_eps, 4), "\n")

# AUTOCOVARIÂNCIA EMPÍRICA
# O loop externo percorre os lags h = 0, ..., max_lag.
# O interno soma (X_t - média)(X_{t+h} - média) até t = n-h.
# O limite n-h impede que t+h ultrapasse a série. Para h = 0, a soma dá a variância.
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

# FAC EMPÍRICA: rho(h) = gamma(h) / gamma(0), calculada para cada lag.
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

# PASSEIO ALEATÓRIO: X_t = X_{t-1} + eps_t.
# Sorteia novos choques, independentes dos usados no ruído branco.
eps_pa <- numeric(n)

for(t in 1:n){
  eps_pa[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

X <- numeric(n)

# Condição inicial: sem X_0, o primeiro valor é X_1 = eps_1.
X[1] <- eps_pa[1]

# LOOP DE RECORRÊNCIA TEMPORAL
# Em cada instante t, soma o choque eps_pa[t] ao valor anterior X[t-1].
# O loop segue a ordem crescente porque X[t] depende de X[t-1].
# Assim, X_t = eps_1 + ... + eps_t, e a variância cresce com o tempo.
for(t in 2:n){
  X[t] <- X[t-1] + eps_pa[t]
}

soma <- 0

for(t in 1:n){
  soma <- soma + X[t]
}

media_X <- soma / n

cat("Média empírica (passeio aleatório):", round(media_X, 4), "\n")

# Calcula as autocovariâncias do passeio com h no loop externo e t no interno.
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

# Divide a janela em dois gráficos para comparar as FACs com acf() do R.
par(mfrow = c(2,1))

acf(eps,
    main = "ACF (R) – Ruído Branco")

acf(X,
    main = "ACF (R) – Passeio Aleatório")

par(mfrow = c(1,1))

# PROCESSO AR(1): X_t = phi * X_{t-1} + eps_t, com novos choques.
eps_ar1 <- numeric(n)

for(t in 1:n){
  eps_ar1[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

X_ar1 <- numeric(n)

# Condição inicial: X_1 = eps_1.
X_ar1[1] <- eps_ar1[1]

# LOOP DE RECORRÊNCIA TEMPORAL
# Em cada instante t, multiplica X[t-1] por phi e soma eps_ar1[t].
# Com |phi| < 1, a influência do passado diminui como phi, phi^2, phi^3, ...
# A série tende a oscilar em torno de zero. Com phi = 1, seria um passeio aleatório.
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

# FAC TEÓRICA DO AR(1): rho(h) = phi^h.
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
