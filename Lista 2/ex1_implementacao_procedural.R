############################################################
# ex1_implementacao_procedural.R
# Simula ruído branco, passeio aleatório e AR(1) de forma procedural, sem definir funções.
# Dependências: R base.

# (a) ENTRADAS
# São n = 200, max_lag = 20, sigma2 = 1 e phi = 0.7, definidos no início do script junto com
# a semente 123, sem nenhuma entrada externa.

# (b) SAÍDAS
# Produz as três séries, suas médias e FACs, as comparações com acf() e os gráficos,
# além de imprimir as médias individuais e um resumo dos resultados no console.

# (c) ESTRUTURAS DE DADOS
# As séries são vetores de tamanho n e as autocovariâncias e FACs têm max_lag + 1 posições,
# com o lag h armazenado na posição h+1 porque os índices em R começam em 1.

# (d) FUNÇÕES REPRESENTADAS PELOS BLOCOS PROCEDURAIS
# Os blocos simulam os três processos, incluindo X_t = phi * X_{t-1} + eps_t, e calculam
# gamma(h) = (1/n) * soma (X_t - media)(X_{t+h} - media) e rho(h) = gamma(h)/gamma(0).
# Também calculam phi^h para o AR(1) e geram os gráficos com plot() e barplot().

# (e) FLUXO DE EXECUÇÃO
# Define os parâmetros, simula os processos e calcula as estatísticas, depois compara as FACs
# com acf() e com a expressão teórica do AR(1), gera os gráficos e imprime o resumo.
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

# Sorteia os choques um por vez para manter a implementação procedural do ruído branco.
eps <- numeric(n)

for(t in 1:n){
  eps[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

soma <- 0

for(t in 1:n){
  soma <- soma + eps[t]
}

media_eps <- soma / n

cat("Média empírica (ruído branco):", round(media_eps, 4), "\n")

# Para cada lag h, o loop interno soma os produtos dos desvios até t = n-h, evitando que
# o índice t+h ultrapasse a série. Em h = 0, a soma dividida por n fornece a variância.
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

# Sorteia novos choques independentes para que o passeio não reutilize o ruído branco anterior.
eps_pa <- numeric(n)

for(t in 1:n){
  eps_pa[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

X <- numeric(n)

# Como não há X_0, o passeio começa em X_1 = eps_1.
X[1] <- eps_pa[1]

# O loop segue a ordem crescente porque cada X_t depende de X_{t-1}, acumulando os choques
# desde o início, o que faz a variância do passeio crescer com o tempo.
for(t in 2:n){
  X[t] <- X[t-1] + eps_pa[t]
}

soma <- 0

for(t in 1:n){
  soma <- soma + X[t]
}

media_X <- soma / n

cat("Média empírica (passeio aleatório):", round(media_X, 4), "\n")

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

par(mfrow = c(2,1))

acf(eps,
    main = "ACF (R) – Ruído Branco")

acf(X,
    main = "ACF (R) – Passeio Aleatório")

par(mfrow = c(1,1))

eps_ar1 <- numeric(n)

for(t in 1:n){
  eps_ar1[t] <- rnorm(1, mean = 0, sd = sqrt(sigma2))
}

X_ar1 <- numeric(n)

X_ar1[1] <- eps_ar1[1]

# O valor anterior recebe peso phi e um novo choque é somado. Como |phi| < 1, a influência
# do passado diminui com as potências de phi, enquanto phi = 1 produziria um passeio aleatório.
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
