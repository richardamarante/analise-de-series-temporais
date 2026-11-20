############################################################
# ex1_implementacao_procedural.R
# exercício 1 da lista de 28/08/2026: decomposição clássica de uma série
# com tendência polinomial e sazonalidade, estimada por OLS, em um único
# script procedural (variáveis, vetores, matrizes e loops, sem funções).
# roda sozinho: Rscript ex1_implementacao_procedural.R
#
# (a) entradas e parâmetros globais (valor inicial e origem)
#   n_anos = 10: anos simulados, fixado no início do script
#   s = 12: período sazonal (meses por ano), fixado no início
#   n = n_anos * s = 120: tamanho da série, calculado a partir de n_anos e s
#   sigma = 5: desvio padrão do ruído branco, fixado no início
#   q = 5: raio da média móvel centrada, fixado no início
#   K = 2: número de harmônicos de Fourier, fixado no início
#   grau_tendencia = 2: grau do polinômio da tendência, fixado no início
#   beta_tendencia = c(2, 0.05, 0.001): coeficientes verdadeiros de 1, t e t^2
#   alpha = c(2, 1) e beta_h = c(-1, 0.5): coeficientes verdadeiros dos
#     cossenos e dos senos; beta_sazonal os intercala (cos1, sen1, cos2, sen2)
#   t = 1:n: vetor de tempo, calculado a partir de n
#   metodo_sazonal = "fourier": método do ajuste, "fourier" ou "dummies"
#   periodo = 12: período usado nas matrizes sazonais, igual a s
#   semente = 42: fixada com set.seed
#   t_futuro = (n+1):(n+12): instantes da previsão, calculados a partir de n
# ---
# (b) saídas
#   matrizes de modelo: X_tendencia, X_fourier, X_dummies, X_sazonal, X_full
#   série simulada: eps e Xt; componentes verdadeiros: tendencia_real e
#     sazonalidade_real
#   estimados: beta_hat, y_hat, tendencia_hat, sazonalidade_hat, residuos
#   diagnóstico: media_residuos, variancia_residuos, rmse, r2
#   testes: estatística t e valor-p de cada coeficiente, Q de Box-Pierce e valor-p
#   média móvel: tendencia_media_movel
#   previsões: previsoes (12 períodos) e o rmse contra o sinal verdadeiro
#   gráficos: observada e ajustada, tendência, sazonalidade, resíduos, média
#     móvel contra tendência estimada
# ---
# (c) estruturas de dados
#   vetores: t, eps, Xt, beta_*, y_hat, residuos, previsoes, medias
#   matrizes: X_tendencia (n x 3), X_sazonal (n x 2K), X_full (n x p),
#     XtX (p x p), com p = número de colunas de X_full
#   vetor Xty (tamanho p): produto interno de cada coluna de X com Xt
# ---
# (d) funções
#   criar_matriz_tendencia, criar_matriz_fourier, criar_matriz_dummies: seções 2 e 3
#   simular_ruido_branco, simular_serie: seção 4
#   calcular_media_movel: seção 5
#   multiplicar_matriz_vetor, calcular_XtX, calcular_Xty, ajustar_ols: seções 6 e 7
#   ajustar_modelo: seções 6 e 7
#   diagnostico: seção 8 (os testes t e de Box-Pierce vêm logo depois)
#   prever: seção 9
#   plot_*: seção 10 (gráficos do R base)
# ---
# (e) fluxo de execução
#   parâmetros -> matrizes de tendência e sazonal -> ruído e série
#   -> média móvel -> X_full -> X'X e X'y -> solve -> ajustados e componentes
#   -> resíduos -> diagnóstico e testes -> matrizes futuras -> previsões
#   -> validação das previsões contra o sinal verdadeiro -> gráficos
# ---
# loops de matriz e solve() (seção 6)
#   a transposta não é construída. X'[i,k] é X[k,i], então ler a coluna i de X
#   faz o papel da linha i de X'. o elemento (i,j) de X'X é o produto interno
#   da coluna i com a coluna j: três loops, i e j escolhem a posição no
#   resultado (p x p) e k soma X[k,i] * X[k,j] sobre as n linhas. custo n p^2.
#   o vetor X'y usa dois loops: para cada coluna i, soma X[k,i] * y[k] sobre
#   as n linhas. custo n p.
#   as equações normais são (X'X) beta = X'y, um sistema linear de p equações.
#   solve(XtX, Xty) o resolve por decomposição LU, sem formar a inversa de
#   X'X, o que é mais rápido e mais estável. precisa de X'X invertível, ou
#   seja, de colunas de X linearmente independentes.
# ---
# fourier x dummies
#   modelo: X_t = tendência(t) + sazonalidade(t) + eps_t, com período s.
#   fourier: sazonalidade(t) = soma_{k=1}^{K} [alpha_k cos(2 pi k t / s)
#     + beta_k sen(2 pi k t / s)]. cada harmônico k é uma onda de período s/k
#     e ocupa 2 colunas (cos e sen), no total 2K (4 aqui). curva suave, com
#     poucos parâmetros, e vale para qualquer s. precisa de K < s/2: com
#     K = s/2 o seno do último harmônico é zero em t inteiro e X'X fica
#     singular. montar a matriz custa n K chamadas de cos e de sen.
#   dummies: sazonalidade(t) = soma_{m=2}^{s} gamma_m D_m(t), com D_m(t) = 1
#     quando o mês ((t-1) %% s) + 1 vale m e 0 caso contrário. o mês 1 é a
#     referência e fica no intercepto: com as s colunas a soma delas seria a
#     coluna de 1 e X'X seria singular. usa s - 1 colunas (11 aqui). cada mês
#     tem um nível livre, sem forma suave, e montar a matriz custa um resto de
#     divisão e uma atribuição por linha.
#   custo da estimação: X'X custa n p^2 multiplicações e o solve custa da
#     ordem de p^3. com tendência de grau 2, p = 3 + 4 = 7 (fourier) ou
#     p = 3 + 11 = 14 (dummies): X'X custa cerca de 4 vezes mais e o solve
#     cerca de 8 vezes mais com dummies. com K = s/2 sem a coluna nula, os
#     dois métodos geram o mesmo espaço de colunas e o mesmo ajuste.
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

# X_sazonal é a matriz usada na simulação (Fourier)
X_sazonal <- X_fourier

# vetor de coeficientes sazonais
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

  # eps_t ~ N(0, sigma²)
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

# tendência e sazonalidade reais
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
# 5. MÉDIA MÓVEL CENTRADA DE RAIO q
############################################################

numero_medias <- n - 2*q

tendencia_media_movel <- numeric(numero_medias)

for(i in (q+1):(n-q)){

  soma <- 0

  # janela de 2q + 1 observações ao redor de i
  for(j in (i-q):(i+q)){

    soma <- soma + Xt[j]
  }

  tendencia_media_movel[i-q] <- soma / (2*q + 1)
}

############################################################
# 6. ESTIMAÇÃO OLS
############################################################

# matriz sazonal do ajuste, escolhida pelo método
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

# X'X com três loops aninhados (a transposta não é construída)
# - i e j escolhem a posição (i,j) do resultado, que é p x p;
# - k percorre as n linhas e soma X[k,i] * X[k,j];
# - ler a coluna i de X é o mesmo que ler a linha i de X', então
#   XtX[i,j] é o produto interno da coluna i com a coluna j;
# - custo: p * p * n multiplicações; a matriz sai simétrica
XtX <- matrix(0, nrow = numero_colunas, ncol = numero_colunas)

for(i in 1:numero_colunas){

  for(j in 1:numero_colunas){

    # produto interno entre a coluna i e a coluna j
    soma <- 0

    for(k in 1:numero_linhas){

      # acumula X[k,i] * X[k,j], uma linha por vez
      soma <- soma + X_full[k,i] * X_full[k,j]

    }

    # guarda o produto interno na posição (i,j)
    XtX[i,j] <- soma
  }
}

# X'y com dois loops
# - i escolhe a coluna de X; k percorre as n linhas e soma X[k,i] * y[k];
# - Xty[i] é o produto interno da coluna i de X com y;
# - custo: p * n multiplicações
Xty <- numeric(numero_colunas)

for(i in 1:numero_colunas){

  soma <- 0

  for(k in 1:numero_linhas){

    soma <- soma + X_full[k,i] * Xt[k]
  }

  Xty[i] <- soma
}

# equações normais: (X'X) beta = X'y, um sistema linear de p equações
# - solve(A, b) resolve A beta = b por decomposição LU, sem formar a inversa
#   de A, o que é mais rápido e mais estável que solve(A) %*% b;
# - precisa de X'X invertível (colunas de X linearmente independentes);
# - as.numeric() tira a estrutura de matriz do resultado
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

# coeficientes da tendência e da sazonalidade
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

# média de y
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

# média dos resíduos
soma <- 0

for(i in 1:n){
  soma <- soma + residuos[i]
}

media_residuos <- soma / n

# variância dos resíduos
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

# teste t dos coeficientes: t_j = beta_hat_j / ep_j, com ep_j^2 = s2 [(X'X)^-1]_jj
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

# teste de Box-Pierce: Q = n * soma de rho_hat(h)^2, h = 1, ..., max_lag_box
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

# matriz de tendência futura
X_tendencia_fut <- matrix(0, nrow = n_futuro, ncol = grau_tendencia + 1)

for(i in 1:n_futuro){

  for(j in 0:grau_tendencia){
    X_tendencia_fut[i,j+1] <- t_futuro[i]^j
  }
}

# matriz sazonal futura
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

# validação: previsão contra tendência + sazonalidade verdadeiras (sem ruído)
# (só no método fourier, que é o que gerou a série)
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

# série observada + ajustada
plot(t, Xt, type = "l", col = "blue", lwd = 2,
     xlab = "Tempo", ylab = "X_t", main = "Série Observada vs Ajustada")
lines(t, y_hat, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("Série observada", "Série ajustada"),
       col = c("blue", "red"), lwd = c(2, 2), lty = c(1, 2), bty = "n")
grid()

# tendência
plot(t, tendencia_hat, type = "l", col = "red", lwd = 2,
     xlab = "Tempo", ylab = "Tendência", main = "Tendência")
lines(t, tendencia_real, col = "blue", lwd = 2)
legend("topleft", legend = c("Tendência estimada", "Tendência real"),
       col = c("red", "blue"), lwd = 2, bty = "n")
grid()

# sazonalidade
plot(t, sazonalidade_hat, type = "l", col = "green", lwd = 2,
     xlab = "Tempo", ylab = "Sazonalidade", main = "Sazonalidade")
lines(t, sazonalidade_real, col = "blue", lwd = 2)
legend("topleft", legend = c("Sazonalidade estimada", "Sazonalidade real"),
       col = c("green", "blue"), lwd = 2, bty = "n")
grid()

# resíduos
plot(t, residuos, type = "l", col = "black", lwd = 2,
     xlab = "Tempo", ylab = "Resíduo", main = "Resíduos")
abline(h = 0, col = "red", lty = 2)
grid()

par(mfrow = c(1,1))

# comparação entre média móvel e tendência estimada
plot(t, Xt, type = "l", col = "blue", lwd = 1.5,
     xlab = "Tempo", ylab = "X_t", main = "Série Temporal, Média Móvel e Tendência")

tempo_media <- (q+1):(n-q)

lines(tempo_media, tendencia_media_movel, col = "red", lwd = 2)
lines(t, tendencia_hat, col = "green", lwd = 2)

legend("topleft",
       legend = c("Série Xt", paste0("Média móvel q=", q), "Tendência estimada por OLS"),
       col = c("blue", "red", "green"), lwd = c(1.5, 2, 2), bty = "n")

grid()
