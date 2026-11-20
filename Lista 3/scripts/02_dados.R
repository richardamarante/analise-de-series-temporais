############################################################
# 02_dados.R
# Simulação do ruído branco e da série com tendência polinomial.
# Dependências: nenhuma.
############################################################

# ruído branco: eps_t ~ N(0, sigma²), independentes
# entrada: n (inteiro positivo), sigma (desvio padrão, positivo)
# saída: vetor numérico de tamanho n
# pré-condição: sigma > 0
simular_ruido_branco <- function(n, sigma){

  eps <- numeric(n)

  for(t in 1:n){
    eps[t] <- rnorm(1, mean = 0, sd = sigma)
  }

  return(eps)
}

# série com tendência polinomial: X_t = β₀ + β₁t + β₂t² + eps_t, ou Xt = X beta + eps
# entrada: beta (vetor com ncol(X) coeficientes), X (matriz n x p), sigma (desvio padrão)
# saída: vetor numérico de tamanho n
# pré-condição: length(beta) == ncol(X) e sigma > 0
simular_serie_com_tendencia <- function(beta, X, sigma){

  n <- nrow(X)

  eps <- simular_ruido_branco(n, sigma)

  Xt <- numeric(n)

  numero_colunas <- ncol(X)

  # loop de fora: um instante t por vez (linha de X)
  for(t in 1:n){

    valor <- 0

    # loop de dentro: produto da linha t de X por beta, termo a termo
    for(j in 1:numero_colunas){
      valor <- valor + X[t,j] * beta[j]
    }

    Xt[t] <- valor + eps[t]
  }

  return(Xt)
}
