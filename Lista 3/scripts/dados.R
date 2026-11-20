############################################################
# dados.R
# Simula o ruído branco e a série com tendência polinomial, calculada a partir de X e beta.
# Dependências: nenhuma.
############################################################

# Simula n valores independentes de ruído branco, com eps_t ~ N(0, sigma²).
# Entrada: n inteiro positivo e sigma como desvio padrão.
# Saída: vetor numérico de tamanho n.
# Pré-condição: sigma > 0.
simular_ruido_branco <- function(n, sigma){

  eps <- numeric(n)

  for(t in 1:n){
    eps[t] <- rnorm(1, mean = 0, sd = sigma)
  }

  return(eps)
}

# Calcula a tendência mu_t = β₀ + β₁*t + β₂*t², equivalente a X %*% beta.
# Entrada: beta com ncol(X) coeficientes e X como matriz n x p.
# Saída: vetor numérico de tamanho n.
# Pré-condição: length(beta) == ncol(X).
calcular_tendencia <- function(beta, X){

  n <- nrow(X)

  tendencia <- numeric(n)

  numero_colunas <- ncol(X)

  for(t in 1:n){

    valor <- 0

    # Para cada instante t, soma X[t,j] * beta[j] ao percorrer as colunas de X,
    # obtendo o valor da tendência naquele instante.
    for(j in 1:numero_colunas){
      valor <- valor + X[t,j] * beta[j]
    }

    tendencia[t] <- valor
  }

  return(tendencia)
}

# Simula Xt = X %*% beta + eps, com eps_t ~ N(0, sigma²).
# Entrada: beta com ncol(X) coeficientes, X como matriz n x p e sigma como desvio padrão.
# Saída: vetor numérico de tamanho n.
# Pré-condição: length(beta) == ncol(X) e sigma > 0.
simular_serie_com_tendencia <- function(beta, X, sigma){

  n <- nrow(X)

  eps <- simular_ruido_branco(n, sigma)

  tendencia <- calcular_tendencia(beta, X)

  Xt <- numeric(n)

  for(t in 1:n){
    Xt[t] <- tendencia[t] + eps[t]
  }

  return(Xt)
}
