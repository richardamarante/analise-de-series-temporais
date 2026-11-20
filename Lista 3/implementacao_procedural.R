############################################################
# AULA PRÁTICA – SÉRIE TEMPORAL
# TENDÊNCIA, MÉDIA MÓVEL E AUTOCORRELAÇÃO
# IMPLEMENTAÇÃO PROCEDURAL (SEM FUNÇÕES DEFINIDAS)
#
# (a) entradas e parâmetros globais
#   n = 90: tamanho da série, fixado no início do script (seção 1)
#   sigma = 15: desvio padrão do ruído branco, fixado na seção 1
#   q = 3: raio da média móvel (janela de 2q + 1 = 7 pontos), fixado na seção 1
#   max_lag = 20: maior defasagem da autocovariância e da FAC, fixado na seção 1
#   semente = 123: valor de set.seed, chamado no começo do script
#   beta = c(2, 0.01, 0.01): coeficientes de 1, t e t², fixados na seção 1
#   t = 1:n: vetor de tempo, calculado a partir de n
#   X: matriz de regressão n x 3 com colunas 1, t e t², montada por loop a partir de t
# ---
# (b) saídas
#   Xt: série simulada com tendência (vetor de tamanho n)
#   m_q: médias móveis centradas (vetor de tamanho n - 2q)
#   media_Xt: média amostral de Xt
#   gamma_1: autocovariância empírica de defasagem 1
#   fac_Xt: FAC empírica (vetor de tamanho max_lag + 1)
#   console: média, gamma_1, FAC e diferença para a acf() do R
#   gráficos: série com média móvel, FAC (R base) e acf() do R
# ---
# (c) estruturas de dados
#   vetores numéricos: t, beta, eps, Xt, m_q, autocovariancias, fac_Xt
#   matriz numérica: X, n linhas x 3 colunas, linha i = (1, t_i, t_i²)
# ---
# (d) funções
#   simular_ruido_branco -> bloco ruído branco
#   simular_serie_com_tendencia -> bloco série com tendência
#   calcular_media_movel -> seção 3
#   calcular_media_amostral -> bloco média amostral
#   calcular_autocovariancia_empirica e calcular_autocovariancias -> bloco autocovariâncias
#   calcular_fac_empirica -> bloco FAC empírica
#   plot_serie_media_movel e plot_fac_empirica -> seção 7
# ---
# (e) fluxo de execução
#   parâmetros -> t, beta, X -> ruído -> Xt -> média móvel
#   -> média amostral -> autocovariâncias -> FAC -> gráficos -> acf() do R
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

# vetor de tempo, 1, 2, ..., n
t <- 1:n

# coeficientes do polinômio: 2 + 0.01 t + 0.01 t²
beta <- c(2, 0.01, 0.01)

# matriz de regressão, uma linha por instante e uma coluna por termo
X <- matrix(
  0,
  nrow = n,
  ncol = 3
)

# i percorre as linhas de X, ou seja, os instantes de tempo
# a linha i guarda (1, t_i, t_i²), os três termos do polinômio em t_i
for(i in 1:n){

  # coluna 1: termo constante, sempre 1
  X[i,1] <- 1

  # coluna 2: termo linear, o próprio tempo
  X[i,2] <- t[i]

  # coluna 3: termo quadrático, o tempo ao quadrado
  X[i,3] <- t[i]^2
}

############################################################
# 2. SIMULAÇÃO
############################################################

# ruído branco: n sorteios independentes de N(0, sigma²), um por instante
eps <- numeric(n)

for(i in 1:n){
  eps[i] <- rnorm(1, mean = 0, sd = sigma)
}

# série com tendência: Xt = X beta + eps, calculada linha por linha
Xt <- numeric(n)

numero_colunas <- ncol(X)

# loop de fora: um instante i por vez, de 1 até n
for(i in 1:n){

  # valor acumula o produto da linha i de X por beta
  # começa em zero a cada instante novo
  valor <- 0

  # loop de dentro: percorre as colunas j de X
  # a cada passo soma X[i,j] * beta[j], um termo do polinômio
  # j = 1: 1 * beta[1]; j = 2: t_i * beta[2]; j = 3: t_i² * beta[3]
  # ao fim das 3 voltas, valor = beta[1] + beta[2] t_i + beta[3] t_i²
  for(j in 1:numero_colunas){
    valor <- valor + X[i,j] * beta[j]
  }

  # soma o ruído do instante i e guarda a observação
  Xt[i] <- valor + eps[i]
}

############################################################
# 3. MÉDIA MÓVEL
############################################################

# média móvel centrada de raio q: m_t = (X[t-q] + ... + X[t+q]) / (2q + 1)
# a janela precisa caber na série, então só existem n - 2q médias
numero_medias <- n - 2*q

m_q <- numeric(numero_medias)

# loop de fora: i é o centro da janela, de q+1 até n-q
# nos primeiros e nos últimos q pontos a janela sairia da série
for(i in (q+1):(n-q)){

  # soma dos 2q + 1 valores da janela [i-q, i+q]
  soma <- 0

  # loop de dentro: j percorre a janela, do ponto i-q ao ponto i+q
  for(j in (i-q):(i+q)){
    soma <- soma + Xt[j]
  }

  # divide por 2q + 1 e guarda na posição i-q, então a primeira média
  # (centro i = q+1) fica na posição 1 e a última na posição n - 2q
  m_q[i-q] <- soma / (2*q + 1)
}

############################################################
# 4. ESTATÍSTICAS
############################################################

# média amostral: soma de todos os X_t dividida por n
soma <- 0

for(i in 1:n){
  soma <- soma + Xt[i]
}

media_Xt <- soma / n

cat("Média amostral de Xt:", media_Xt, "\n")

# autocovariâncias: gamma(h) = (1/n) soma de (X_t - média)(X_{t+h} - média)
autocovariancias <- numeric(max_lag + 1)

# loop de fora: h é a defasagem, e gamma(h) fica na posição h+1
for(h in 0:max_lag){

  soma <- 0

  # loop de dentro: i vai de 1 até n-h, os pares (X_i, X_{i+h}) que existem
  # quando h = 0 o par é (X_i, X_i) e a soma vira a soma dos quadrados dos desvios
  for(i in 1:(n-h)){
    d1 <- Xt[i] - media_Xt
    d2 <- Xt[i+h] - media_Xt
    soma <- soma + d1 * d2
  }

  # divide por n, não por n-h
  autocovariancias[h+1] <- soma / n
}

gamma_1 <- autocovariancias[2]

cat("Autocovariância empírica de defasagem 1:", gamma_1, "\n")

# FAC empírica: rho(h) = gamma(h) / gamma(0), então rho(0) = 1
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

# dois gráficos na mesma janela, um embaixo do outro
par(mfrow = c(2,1), mar = c(4,4,2,2))

# série com a média móvel por cima
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

# a média móvel começa em t = q+1 e termina em t = n-q
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

# FAC empírica em barras
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
