############################################################
# dados.R
# Simula ruído branco, passeio aleatório e AR(1), recebendo os parâmetros como argumentos.
# Dependências: nenhuma.
############################################################

# Simula ruído branco com n valores independentes de eps_t ~ N(0, sigma2).
# Entrada: n é um inteiro >= 1 e sigma2 é um número > 0.
# Saída: vetor numérico de tamanho n.
simular_ruido_branco <- function(n, sigma2){
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# Acumula os choques do passeio aleatório, com X_1 = eps_1 e X_t = X_{t-1} + eps_t.
# Entrada: eps é um vetor numérico de tamanho n >= 1.
# Saída: vetor de tamanho n, com cada X_t igual à soma dos choques de eps_1 até eps_t.
acumular_passeio_aleatorio <- function(eps){

  n <- length(eps)

  X <- numeric(n)

  X[1] <- eps[1]

  # Como X[1] já foi calculado, o loop começa em t = 2 e só é executado quando n >= 2,
  # seguindo a ordem crescente porque cada valor depende do anterior.
  if(n >= 2){

    for(t in 2:n){

      X[t] <- X[t-1] + eps[t]
    }
  }

  return(X)
}

# Simula o passeio aleatório X_t = X_{t-1} + eps_t, com choques de variância sigma2.
# Entrada: n é um inteiro >= 1 e sigma2 é um número > 0.
# Saída: vetor numérico de tamanho n.
# Pré-condição: fixar_semente() chamada no main.R para reproduzir o sorteio.
simular_passeio_aleatorio <- function(n, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  # Separa o acúmulo do sorteio para que ex4_eficiencia.R possa cronometrar apenas o loop.
  X <- acumular_passeio_aleatorio(eps)

  return(X)
}

# Simula o processo AR(1), com X_t = phi * X_{t-1} + eps_t.
# Entrada: n é um inteiro >= 1, phi satisfaz |phi| < 1 e sigma2 é um número > 0.
# Saída: vetor numérico de tamanho n.
# Pré-condição: fixar_semente() chamada no main.R para reproduzir o sorteio.
simular_ar1 <- function(n, phi, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  X <- numeric(n)

  X[1] <- eps[1]

  # O loop começa em t = 2 porque X[1] já foi definido e usa phi * X[t-1] + eps[t] em cada passo.
  # Como |phi| < 1, a influência do passado diminui, enquanto phi = 1 daria um passeio aleatório.
  if(n >= 2){

    for(t in 2:n){

      X[t] <- phi * X[t-1] + eps[t]
    }
  }

  return(X)
}
