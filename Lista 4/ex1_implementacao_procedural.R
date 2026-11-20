############################################################
# ex1_implementacao_procedural.R
# Simula e decompõe uma série com tendência polinomial e sazonalidade, ajustando por OLS
# em uma implementação procedural com vetores, matrizes e loops, usando apenas R base.
# Execução: Rscript ex1_implementacao_procedural.R

# (a) Entradas: n = 120 (10 anos, s = 12), sigma = 5 e tendência de grau 2, com K = 2
# harmônicos, q = 5, semente 42, método Fourier ou dummies e previsão de 12 períodos.

# (b) Saídas: série e componentes, coeficientes OLS, resíduos, média móvel, testes,
# métricas, previsões e cinco gráficos.

# (c) Estruturas: vetores para os dados e resultados, matrizes n x p para os regressores,
# p x p para X'X e um vetor de tamanho p para X'y.

# (d) Funções: não define funções próprias, pois usa loops e operações do R base.

# (e) Fluxo: simula Xt = tendência + sazonalidade + eps, calcula a média móvel e ajusta
# beta_hat = (X'X)^(-1) X'y, antes do diagnóstico, dos testes, da previsão e dos gráficos.

# Em X'X, i e j percorrem as colunas e k soma os produtos nas n linhas, com custo O(n p²);
# em X'y, i percorre as colunas e k soma X_full[k,i] * Xt[k], com custo O(n p).
# solve(XtX, Xty) resolve o sistema sem formar a inversa, desde que X_full tenha posto completo.

# Fourier usa pares de cosseno e seno para cada harmônico, com 2K colunas e K < s/2
# para evitar seno nulo, enquanto dummies usam s - 1 colunas e deixam o mês 1 no intercepto.
# Com grau 2 e K = 2, são p = 7 contra p = 14, então as dummies custam cerca de 4 vezes
# mais em X'X e 8 vezes em solve(); usando K = s/2 sem o último seno, Fourier passa
# a representar o mesmo espaço das dummies e produz o mesmo ajuste.
############################################################

rm(list = ls())

set.seed(42)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n_anos <- 10

s <- 12

n <- n_anos * s

sigma <- 5

q <- 5

K <- 2

grau_tendencia <- 2

beta_tendencia <- c(2, 0.05, 0.001)

alpha <- c(2, 1)

beta_h <- c(-1, 0.5)

metodo_sazonal <- "fourier"

periodo <- 12

t <- 1:n

############################################################
# 2. MATRIZ DE TENDÊNCIA
############################################################

X_tendencia <- matrix(0, nrow = n, ncol = grau_tendencia + 1)

for(i in 1:n){

  for(j in 0:grau_tendencia){
    X_tendencia[i,j+1] <- t[i]^j
  }
}

############################################################
# 3. MATRIZ SAZONAL
############################################################

X_fourier <- matrix(0, nrow = n, ncol = 2 * K)

for(i in 1:n){

  for(k in 1:K){

    coluna_cos <- 2*k - 1
    coluna_sin <- 2*k

    X_fourier[i,coluna_cos] <- cos(2 * pi * k * t[i] / s)

    X_fourier[i,coluna_sin] <- sin(2 * pi * k * t[i] / s)
  }
}

X_dummies <- matrix(0, nrow = n, ncol = s - 1)

for(i in 1:n){

  mes <- ((t[i] - 1) %% s) + 1

  if(mes != 1){

    coluna <- mes - 1

    X_dummies[i,coluna] <- 1
  }
}

X_sazonal <- X_fourier

# Intercala os coeficientes na ordem cos1, sen1, cos2, sen2, que é a ordem das colunas de Fourier.
beta_sazonal <- numeric(2 * K)

for(k in 1:K){

  coluna_cos <- 2*k - 1
  coluna_sin <- 2*k

  beta_sazonal[coluna_cos] <- alpha[k]

  beta_sazonal[coluna_sin] <- beta_h[k]
}

############################################################
# 4. RUÍDO BRANCO E SIMULAÇÃO DA SÉRIE
############################################################

eps <- numeric(n)

for(i in 1:n){

  # Gera o ruído branco com eps_t ~ N(0, sigma²).
  eps[i] <- rnorm(1, mean = 0, sd = sigma)
}

Xt <- numeric(n)

for(i in 1:n){

  valor_tendencia <- 0

  for(j in 1:ncol(X_tendencia)){

    valor_tendencia <- valor_tendencia + X_tendencia[i,j] * beta_tendencia[j]
  }

  valor_sazonal <- 0

  for(j in 1:ncol(X_sazonal)){

    valor_sazonal <- valor_sazonal + X_sazonal[i,j] * beta_sazonal[j]
  }

  Xt[i] <- valor_tendencia + valor_sazonal + eps[i]
}

tendencia_real <- numeric(n)
sazonalidade_real <- numeric(n)

for(i in 1:n){

  valor <- 0

  for(j in 1:ncol(X_tendencia)){
    valor <- valor + X_tendencia[i,j] * beta_tendencia[j]
  }

  tendencia_real[i] <- valor

  valor <- 0

  for(j in 1:ncol(X_sazonal)){
    valor <- valor + X_sazonal[i,j] * beta_sazonal[j]
  }

  sazonalidade_real[i] <- valor
}

############################################################
# A média móvel centrada perde q observações em cada ponta, restando n - 2q médias.
############################################################

numero_medias <- n - 2*q

tendencia_media_movel <- numeric(numero_medias)

for(i in (q+1):(n-q)){

  soma <- 0

  for(j in (i-q):(i+q)){

    soma <- soma + Xt[j]
  }

  tendencia_media_movel[i-q] <- soma / (2*q + 1)
}

############################################################
# 6. ESTIMAÇÃO OLS
############################################################

if(metodo_sazonal == "fourier"){

  X_sazonal_ajuste <- X_fourier

} else if(metodo_sazonal == "dummies"){

  X_sazonal_ajuste <- X_dummies

} else {

  stop("Método sazonal inválido")
}

X_full <- cbind(X_tendencia, X_sazonal_ajuste)

numero_linhas <- nrow(X_full)

numero_colunas <- ncol(X_full)

# Os loops i e j percorrem as posições de X'X, enquanto k soma os produtos
# X_full[k,i] * X_full[k,j] nas n linhas, calculando cada produto interno sem transpor X_full.
XtX <- matrix(0, nrow = numero_colunas, ncol = numero_colunas)

for(i in 1:numero_colunas){

  for(j in 1:numero_colunas){

    soma <- 0

    for(k in 1:numero_linhas){

      soma <- soma + X_full[k,i] * X_full[k,j]

    }

    XtX[i,j] <- soma
  }
}

# Para calcular X'y, i percorre as colunas de X_full e k soma X_full[k,i] * Xt[k]
# nas n linhas, totalizando n p multiplicações.
Xty <- numeric(numero_colunas)

for(i in 1:numero_colunas){

  soma <- 0

  for(k in 1:numero_linhas){

    soma <- soma + X_full[k,i] * Xt[k]
  }

  Xty[i] <- soma
}

# Resolve (X'X) beta = X'y sem calcular a inversa explicitamente, o que exige
# que as colunas de X_full sejam linearmente independentes.
beta_hat <- solve(XtX, Xty)

beta_hat <- as.numeric(beta_hat)

############################################################
# 7. VALORES AJUSTADOS E COMPONENTES ESTIMADOS
############################################################

y_hat <- numeric(n)

for(i in 1:n){

  valor <- 0

  for(j in 1:numero_colunas){

    valor <- valor + X_full[i,j] * beta_hat[j]
  }

  y_hat[i] <- valor
}

numero_colunas_tendencia <- ncol(X_tendencia)

numero_colunas_sazonal <- ncol(X_sazonal_ajuste)

beta_tendencia_hat <- numeric(numero_colunas_tendencia)

for(i in 1:numero_colunas_tendencia){

  beta_tendencia_hat[i] <- beta_hat[i]
}

beta_sazonal_hat <- numeric(numero_colunas_sazonal)

for(i in 1:numero_colunas_sazonal){

  beta_sazonal_hat[i] <- beta_hat[numero_colunas_tendencia + i]
}

tendencia_hat <- numeric(n)
sazonalidade_hat <- numeric(n)

for(i in 1:n){

  valor <- 0

  for(j in 1:numero_colunas_tendencia){
    valor <- valor + X_tendencia[i,j] * beta_tendencia_hat[j]
  }

  tendencia_hat[i] <- valor

  valor <- 0

  for(j in 1:numero_colunas_sazonal){
    valor <- valor + X_sazonal_ajuste[i,j] * beta_sazonal_hat[j]
  }

  sazonalidade_hat[i] <- valor
}

residuos <- numeric(n)

for(i in 1:n){

  residuos[i] <- Xt[i] - y_hat[i]
}

############################################################
# 8. DIAGNÓSTICO
############################################################

ss_res <- 0

for(i in 1:n){
  ss_res <- ss_res + residuos[i]^2
}

soma <- 0

for(i in 1:n){
  soma <- soma + Xt[i]
}

media_y <- soma / n

ss_tot <- 0

for(i in 1:n){

  d <- Xt[i] - media_y

  ss_tot <- ss_tot + d^2
}

soma <- 0

for(i in 1:n){
  soma <- soma + residuos[i]
}

media_residuos <- soma / n

soma_quadrados <- 0

for(i in 1:n){

  d <- residuos[i] - media_residuos

  soma_quadrados <- soma_quadrados + d^2
}

variancia_residuos <- soma_quadrados / (n - 1)

rmse <- sqrt(ss_res / n)

r2 <- 1 - ss_res / ss_tot

cat("Média dos resíduos:", round(media_residuos, 4), "\n")

cat("Variância dos resíduos:", round(variancia_residuos, 4), "\n")

cat("RMSE:", round(rmse, 4), "\n")

cat("R²:", round(r2, 4), "\n")

cat("Coeficientes estimados:\n")

print(round(beta_hat, 4))

# Testa H0: beta_j = 0 com t_j = beta_hat_j / ep_j, em que
# ep_j² = s2 [(X'X)^-1]_jj.
p <- numero_colunas

s2 <- ss_res / (n - p)

XtX_inversa <- solve(XtX)

for(j in 1:p){

  erro_padrao <- sqrt(XtX_inversa[j,j] * s2)

  estatistica_t <- beta_hat[j] / erro_padrao

  valor_p <- 2 * (1 - pt(abs(estatistica_t), n - p))

  cat("Coeficiente", j, "- estatística t:", round(estatistica_t, 4),
      "- valor-p:", round(valor_p, 4), "\n")
}

# Aplica Box-Pierce com 12 defasagens, usando Q = n * soma de rho_hat(h)².
max_lag_box <- 12

gama0 <- 0

for(i in 1:n){
  gama0 <- gama0 + (residuos[i] - media_residuos)^2
}

gama0 <- gama0 / n

Q <- 0

for(h in 1:max_lag_box){

  gama_h <- 0

  for(i in 1:(n - h)){
    gama_h <- gama_h + (residuos[i] - media_residuos) * (residuos[i + h] - media_residuos)
  }

  Q <- Q + (gama_h / n / gama0)^2
}

Q <- n * Q

cat("Box-Pierce - Q:", round(Q, 4), "\n")

cat("Box-Pierce - valor-p:", round(1 - pchisq(Q, max_lag_box), 4), "\n")

############################################################
# 9. PREVISÃO
############################################################

t_futuro <- (n+1):(n+12)

n_futuro <- length(t_futuro)

X_tendencia_fut <- matrix(0, nrow = n_futuro, ncol = grau_tendencia + 1)

for(i in 1:n_futuro){

  for(j in 0:grau_tendencia){
    X_tendencia_fut[i,j+1] <- t_futuro[i]^j
  }
}

# Mantém o mesmo método sazonal do ajuste para construir as matrizes futuras.
if(metodo_sazonal == "fourier"){

  X_sazonal_fut <- matrix(0, nrow = n_futuro, ncol = 2 * K)

  for(i in 1:n_futuro){

    for(k in 1:K){

      X_sazonal_fut[i,2*k - 1] <- cos(2 * pi * k * t_futuro[i] / s)

      X_sazonal_fut[i,2*k] <- sin(2 * pi * k * t_futuro[i] / s)
    }
  }

} else {

  X_sazonal_fut <- matrix(0, nrow = n_futuro, ncol = s - 1)

  for(i in 1:n_futuro){

    mes <- ((t_futuro[i] - 1) %% s) + 1

    if(mes != 1){
      X_sazonal_fut[i,mes - 1] <- 1
    }
  }
}

X_fut <- cbind(X_tendencia_fut, X_sazonal_fut)

previsoes <- numeric(n_futuro)

for(i in 1:n_futuro){

  valor <- 0

  for(j in 1:ncol(X_fut)){
    valor <- valor + X_fut[i,j] * beta_hat[j]
  }

  previsoes[i] <- valor
}

cat("Previsões para os próximos 12 períodos:\n")

print(round(previsoes, 4))

# Quando o método é Fourier, compara as previsões com o sinal verdadeiro sem ruído.
if(metodo_sazonal == "fourier"){

  soma_erro2 <- 0

  for(i in 1:n_futuro){

    sinal <- 0

    for(j in 1:ncol(X_tendencia_fut)){
      sinal <- sinal + X_tendencia_fut[i,j] * beta_tendencia[j]
    }

    for(j in 1:ncol(X_sazonal_fut)){
      sinal <- sinal + X_sazonal_fut[i,j] * beta_sazonal[j]
    }

    soma_erro2 <- soma_erro2 + (previsoes[i] - sinal)^2
  }

  cat("RMSE da previsão (Fourier):", round(sqrt(soma_erro2 / n_futuro), 4), "\n")
}

############################################################
# 10. GRÁFICOS
############################################################

par(mfrow = c(4,1), mar = c(4,4,2,2))

plot(t, Xt, type = "l", col = "blue", lwd = 2,
     xlab = "Tempo", ylab = "X_t", main = "Série Observada vs Ajustada")
lines(t, y_hat, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observada", "Série ajustada"),
       col = c("blue", "red"), lwd = c(2, 2), lty = c(1, 2), bty = "n")
grid()

plot(t, tendencia_hat, type = "l", col = "red", lwd = 2,
     xlab = "Tempo", ylab = "Tendência", main = "Tendência")
lines(t, tendencia_real, col = "blue", lwd = 2)
legend("topleft", legend = c("Tendência estimada", "Tendência real"),
       col = c("red", "blue"), lwd = 2, bty = "n")
grid()

plot(t, sazonalidade_hat, type = "l", col = "green", lwd = 2,
     xlab = "Tempo", ylab = "Sazonalidade", main = "Sazonalidade")
lines(t, sazonalidade_real, col = "blue", lwd = 2)
legend("topleft", legend = c("Sazonalidade estimada", "Sazonalidade real"),
       col = c("green", "blue"), lwd = 2, bty = "n")
grid()

plot(t, residuos, type = "l", col = "black", lwd = 2,
     xlab = "Tempo", ylab = "Resíduo", main = "Resíduos")
abline(h = 0, col = "red", lty = 2)
grid()

par(mfrow = c(1,1))

plot(t, Xt, type = "l", col = "blue", lwd = 1.5,
     xlab = "Tempo", ylab = "X_t", main = "Série Temporal, Média Móvel e Tendência")

tempo_media <- (q+1):(n-q)

lines(tempo_media, tendencia_media_movel, col = "red", lwd = 2)
lines(t, tendencia_hat, col = "green", lwd = 2)

legend("topleft",
       legend = c("Série Xt", paste0("Média móvel q=", q), "Tendência estimada por OLS"),
       col = c("blue", "red", "green"), lwd = c(1.5, 2, 2), bty = "n")

grid()
