############################################################
# ex1_implementacao_procedural.R
# Simula a série com tendência polinomial, calcula média móvel e FAC e gera gráficos.
# Implementação procedural, sem funções definidas.
# Dependências: nenhuma.

# (a) ENTRADAS E PARÂMETROS
# n = 90: Tamanho da série
# sigma = 15: Desvio padrão do ruído branco
# q = 3: Raio da média móvel, com janela de 2q + 1 = 7 pontos
# max_lag = 20: Maior defasagem da autocovariância e da FAC
# semente = 123: Valor usado em set.seed(123)
# beta = c(2, 0.01, 0.01): Coeficientes do polinômio
# t = 1:n: Vetor de tempo
# X: Matriz n x 3 com colunas 1, t e t²

# (b) SAÍDAS
# Xt: Série simulada com tendência, vetor de tamanho n
# m_q: Médias móveis centradas, vetor de tamanho n - 2q
# media_Xt: Média amostral de Xt
# gamma_1: Autocovariância empírica de lag 1
# fac_Xt: FAC empírica, vetor de tamanho max_lag + 1
# Console: Média, gamma_1, FAC e diferença para a acf() do R
# Gráficos: Série com média móvel, FAC empírica e acf() do R

# (c) ESTRUTURAS DE DADOS
# Vetores numéricos: t, beta, eps, Xt, m_q, autocovariancias, fac_Xt
# Matriz numérica: X, com n linhas e 3 colunas, linha i = (1, t_i, t_i²)

# (d) FUNÇÕES REPRESENTADAS POR BLOCOS
# simular_ruido_branco: Simulação do ruído branco
# simular_serie_com_tendencia: Simulação da série com tendência
# calcular_media_movel: Cálculo da média móvel
# calcular_media_amostral: Cálculo da média amostral
# calcular_autocovariancia_empirica: Cálculo da autocovariância
# calcular_autocovariancias: Cálculo das autocovariâncias
# calcular_fac_empirica: Cálculo da FAC
# plot_serie_media_movel: Gráfico da série com média móvel
# plot_fac_empirica: Gráfico da FAC

# (e) FLUXO DE EXECUÇÃO
# 1. Define os parâmetros, o tempo, os coeficientes e a matriz X.
# 2. Simula o ruído branco e a série com tendência.
# 3. Calcula a média móvel, a média amostral, as autocovariâncias e a FAC.
# 4. Mostra os resultados e gera os gráficos.
# 5. Compara a FAC calculada com a acf() do R.
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

# Vetor de tempo de 1 até n.
t <- 1:n

# Coeficientes da tendência: 2 + 0.01*t + 0.01*t².
beta <- c(2, 0.01, 0.01)

# Matriz de regressão X, com n linhas e 3 colunas.
X <- matrix(
  0,
  nrow = n,
  ncol = 3
)

# Para cada instante i, a linha i de X recebe os termos (1, t_i, t_i²).
for(i in 1:n){

  # Coluna 1: termo constante, igual a 1.
  X[i,1] <- 1

  # Coluna 2: termo linear, igual a t_i.
  X[i,2] <- t[i]

  # Coluna 3: termo quadrático, igual a t_i².
  X[i,3] <- t[i]^2
}

############################################################
# 2. SIMULAÇÃO
############################################################

# Reserva o vetor eps para n valores independentes de N(0, sigma²).
eps <- numeric(n)

for(i in 1:n){
  eps[i] <- rnorm(1, mean = 0, sd = sigma)
}

# Reserva o vetor Xt para a série X %*% beta + eps, calculada linha por linha.
Xt <- numeric(n)

numero_colunas <- ncol(X)

# O loop de fora percorre os n instantes da série.
for(i in 1:n){

  # Começa em zero para somar os termos da tendência no instante i.
  valor <- 0

  # O loop de dentro percorre as colunas de X e soma X[i,j] * beta[j].
  # Ao final, valor = beta[1] + beta[2]*t_i + beta[3]*t_i².
  for(j in 1:numero_colunas){
    valor <- valor + X[i,j] * beta[j]
  }

  # Soma o ruído eps[i] à tendência e guarda o resultado em Xt[i].
  Xt[i] <- valor + eps[i]
}

############################################################
# 3. MÉDIA MÓVEL
############################################################

# Média móvel centrada: m_t = (1/(2q + 1)) * soma de X_j, de j = t-q até t+q.
# Como a janela precisa caber na série, existem n - 2q médias.
numero_medias <- n - 2*q

m_q <- numeric(numero_medias)

# O loop percorre os centros das janelas, de q+1 até n-q.
for(i in (q+1):(n-q)){

  # Começa em zero para somar os 2q + 1 valores da janela.
  soma <- 0

  # O loop de dentro percorre os pontos de i-q até i+q.
  for(j in (i-q):(i+q)){
    soma <- soma + Xt[j]
  }

  # Divide a soma por 2q + 1 e guarda na posição i-q do vetor m_q.
  # Assim, o primeiro centro, i = q+1, ocupa a posição 1.
  m_q[i-q] <- soma / (2*q + 1)
}

############################################################
# 4. ESTATÍSTICAS
############################################################

# Começa em zero para somar os valores da série e calcular a média amostral.
soma <- 0

for(i in 1:n){
  soma <- soma + Xt[i]
}

media_Xt <- soma / n

cat("Média amostral de Xt:", media_Xt, "\n")

# Autocovariância: gamma(h) = (1/n) * soma de (X_t - média)(X_{t+h} - média).
# O vetor guarda os valores de h = 0 até max_lag.
autocovariancias <- numeric(max_lag + 1)

# O loop percorre as defasagens h, guardando gamma(h) na posição h+1.
for(h in 0:max_lag){

  soma <- 0

  # O loop soma os produtos dos desvios para os pares (X_i, X_{i+h}), de i = 1 até n-h.
  # Para h = 0, soma os quadrados dos desvios.
  for(i in 1:(n-h)){
    d1 <- Xt[i] - media_Xt
    d2 <- Xt[i+h] - media_Xt
    soma <- soma + d1 * d2
  }

  # Divide a soma por n para obter gamma(h).
  autocovariancias[h+1] <- soma / n
}

gamma_1 <- autocovariancias[2]

cat("Autocovariância empírica de defasagem 1:", gamma_1, "\n")

# FAC empírica: rho(h) = gamma(h) / gamma(0), com rho(0) = 1.
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

# Organiza os dois gráficos na mesma janela, um acima do outro.
par(mfrow = c(2,1), mar = c(4,4,2,2))

# Mostra a série temporal.
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

# Adiciona a média móvel, de t = q+1 até t = n-q.
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

# Mostra a FAC empírica em barras.
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
