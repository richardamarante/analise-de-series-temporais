############################################################
# dados.R
# Simula o ruído branco, calcula a tendência polinomial e simula a série com tendência.
# Dependências: nenhuma.
############################################################

# Simula n valores independentes de ruído branco, eps_t ~ N(0, sigma²).
# Entrada: n (inteiro positivo), sigma (desvio padrão).
# Saída: vetor numérico de tamanho n.
# Pré-condição: sigma > 0.
simular_ruido_branco <- function(n, sigma){

  eps <- numeric(n)

  for(t in 1:n){
    eps[t] <- rnorm(1, mean = 0, sd = sigma)
  }

  return(eps)
}

# Calcula a tendência sem ruído: mu_t = β₀ + β₁*t + β₂*t², ou X %*% beta.
# Entrada: beta (vetor com ncol(X) coeficientes), X (matriz n x p).
# Saída: vetor numérico de tamanho n.
# Pré-condição: length(beta) == ncol(X).
calcular_tendencia <- function(beta, X){

  n <- nrow(X)

  tendencia <- numeric(n)

  numero_colunas <- ncol(X)

  # O loop de fora percorre os instantes t, que são as linhas de X.
  for(t in 1:n){

    valor <- 0

    # O loop de dentro soma X[t,j] * beta[j] para cada coluna j.
    # Ao final, a soma é o valor da tendência no instante t.
    for(j in 1:numero_colunas){
      valor <- valor + X[t,j] * beta[j]
    }

    tendencia[t] <- valor
  }

  return(tendencia)
}

# Simula a série Xt = X %*% beta + eps, com eps_t ~ N(0, sigma²).
# Entrada: beta (vetor com ncol(X) coeficientes), X (matriz n x p), sigma (desvio padrão).
# Saída: vetor numérico de tamanho n.
# Pré-condição: length(beta) == ncol(X) e sigma > 0.
simular_serie_com_tendencia <- function(beta, X, sigma){

  n <- nrow(X)

  eps <- simular_ruido_branco(n, sigma)

  tendencia <- calcular_tendencia(beta, X)

  Xt <- numeric(n)

  # Para cada instante t, soma o ruído à tendência.
  for(t in 1:n){
    Xt[t] <- tendencia[t] + eps[t]
  }

  return(Xt)
}
