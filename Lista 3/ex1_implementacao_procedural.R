############################################################
# ex1_implementacao_procedural.R
# Simula a série com tendência polinomial, calcula média móvel e FAC e gera gráficos,
# em uma implementação procedural sem funções definidas.
# Dependências: nenhuma.

# (a) Entradas: n = 90, sigma = 15, q = 3 (janela de 7 pontos), max_lag = 20 e semente = 123.
# A tendência usa beta = c(2, 0.01, 0.01), com t = 1:n e matriz X de colunas 1, t e t².

# (b) Saídas: série simulada, média móvel, média amostral, autocovariâncias e FAC, com
# comparação à acf() do R no console e gráficos da série, média móvel e FAC.

# (c) Estruturas: vetores guardam a série e os cálculos, enquanto X é uma matriz n x 3
# cuja linha i contém (1, t_i, t_i²).

# (d) Funções: os blocos simulam o ruído branco e a série, calculam média móvel, média,
# autocovariâncias e FAC e geram os gráficos, sem definir funções.

# (e) Fluxo: define os parâmetros, constrói X, simula Xt = X %*% beta + eps e calcula
# gamma(h) = (1/n) soma (X_t - media)(X_{t+h} - media) e rho(h) = gamma(h)/gamma(0).
# Depois mostra os resultados, gera os gráficos e compara a FAC com acf().
############################################################

rm(list = ls())

set.seed(123)

############################################################
# 1. PARÂMETROS GERAIS
############################################################

n <- 90

sigma <- 15

q <- 3

max_lag <- 20

t <- 1:n

# A tendência é dada por 2 + 0.01*t + 0.01*t².
beta <- c(2, 0.01, 0.01)

X <- matrix(
  0,
  nrow = n,
  ncol = 3
)

# Para cada instante i, preenche as três colunas de X com 1, t_i e t_i².
for(i in 1:n){

  X[i,1] <- 1

  X[i,2] <- t[i]

  X[i,3] <- t[i]^2
}

############################################################
# 2. SIMULAÇÃO
############################################################

eps <- numeric(n)

for(i in 1:n){
  eps[i] <- rnorm(1, mean = 0, sd = sigma)
}

Xt <- numeric(n)

numero_colunas <- ncol(X)

for(i in 1:n){

  valor <- 0

  # Para cada instante i, soma X[i,j] * beta[j] ao percorrer as colunas de X,
  # obtendo beta[1] + beta[2]*t_i + beta[3]*t_i².
  for(j in 1:numero_colunas){
    valor <- valor + X[i,j] * beta[j]
  }

  Xt[i] <- valor + eps[i]
}

############################################################
# 3. MÉDIA MÓVEL
############################################################

# A média móvel centrada usa 2q + 1 valores, de i-q até i+q, e como a janela precisa
# caber na série, só existem n - 2q médias.
numero_medias <- n - 2*q

m_q <- numeric(numero_medias)

for(i in (q+1):(n-q)){

  soma <- 0

  for(j in (i-q):(i+q)){
    soma <- soma + Xt[j]
  }

  # Guarda a média na posição i-q porque o primeiro centro da janela, i = q+1,
  # corresponde à primeira posição de m_q.
  m_q[i-q] <- soma / (2*q + 1)
}

############################################################
# 4. ESTATÍSTICAS
############################################################

soma <- 0

for(i in 1:n){
  soma <- soma + Xt[i]
}

media_Xt <- soma / n

cat("Média amostral de Xt:", media_Xt, "\n")

# Calcula gamma(h) = (1/n) soma (X_i - media)(X_{i+h} - media), para h de 0 até max_lag,
# guardando cada autocovariância na posição h+1 do vetor.
autocovariancias <- numeric(max_lag + 1)

for(h in 0:max_lag){

  soma <- 0

  # Para cada h, percorre os n-h pares (X_i, X_{i+h}) e soma os produtos dos desvios,
  # que são quadrados dos desvios quando h = 0.
  for(i in 1:(n-h)){
    d1 <- Xt[i] - media_Xt
    d2 <- Xt[i+h] - media_Xt
    soma <- soma + d1 * d2
  }

  autocovariancias[h+1] <- soma / n
}

gamma_1 <- autocovariancias[2]

cat("Autocovariância empírica de defasagem 1:", gamma_1, "\n")

# A FAC é dada por rho(h) = gamma(h)/gamma(0), com rho(0) = 1.
gamma0 <- autocovariancias[1]

fac_Xt <- numeric(max_lag + 1)

for(h in 0:max_lag){
  fac_Xt[h+1] <- autocovariancias[h+1] / gamma0
}

cat("FAE (autocorrelação empírica) para lags 0 a", max_lag, ":\n")

print(fac_Xt)

############################################################
# 7. GRÁFICOS
############################################################

par(mfrow = c(2,1), mar = c(4,4,2,2))

plot(
  t,
  Xt,
  type = "l",
  col = "blue",
  lwd = 1.5,
  xlab = "Tempo (t)",
  ylab = "X_t",
  main = "Série Temporal com Média Móvel"
)

lines(
  (q+1):(n-q),
  m_q,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c(
    "Série Xt",
    paste0("Média móvel q=", q)
  ),
  col = c("blue", "red"),
  lwd = c(1.5, 2),
  bty = "n"
)

barplot(
  fac_Xt,
  names.arg = 0:max_lag,
  col = "skyblue",
  ylim = c(
    min(0, min(fac_Xt)),
    1
  ),
  main = "Função de Autocorrelação Empírica (FAE)",
  xlab = "Lag (h)",
  ylab = expression(hat(rho)[h])
)

abline(
  h = 0,
  col = "gray",
  lty = 2
)

par(mfrow = c(1,1))

############################################################
# 8. COMPARAÇÃO COM A FUNÇÃO BUILT-IN DO R
############################################################

acf(Xt, main = "ACF (R) – Série Temporal")

fac_R <- as.numeric(acf(Xt, lag.max = max_lag, plot = FALSE)$acf)

cat("Maior diferença absoluta entre a FAC manual e acf():",
    max(abs(fac_Xt - fac_R)), "\n")
