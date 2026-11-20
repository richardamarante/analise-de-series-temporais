############################################################
# dados.R
# Simula ruído branco, passeio aleatório e AR(1).
# Dependências: nenhuma. Os parâmetros são recebidos como argumentos.
############################################################

# Simula ruído branco: eps_t ~ N(0, sigma2), com valores independentes.
# Entrada: n (inteiro >= 1), sigma2 (número > 0).
# Saída: Vetor numérico de tamanho n.
simular_ruido_branco <- function(n, sigma2){
  # rnorm() sorteia os n valores de uma vez, na mesma ordem do sorteio individual.
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# Acumula os choques do passeio: X_1 = eps_1 e X_t = X_{t-1} + eps_t.
# Entrada: eps (vetor numérico de tamanho n >= 1).
# Saída: Vetor de tamanho n com X_t = eps_1 + ... + eps_t.
acumular_passeio_aleatorio <- function(eps){

  n <- length(eps)

  X <- numeric(n)

  # Condição inicial: sem X_0, X[1] recebe apenas eps[1].
  X[1] <- eps[1]

  # O loop começa em t = 2 porque X[1] já foi calculado.
  # Em cada passo, soma eps[t] ao valor anterior X[t-1] e grava em X[t].
  # A ordem crescente é necessária porque cada valor depende do anterior.
  if(n >= 2){

    for(t in 2:n){

      X[t] <- X[t-1] + eps[t]
    }
  }

  return(X)
}

# Simula o passeio aleatório: X_t = X_{t-1} + eps_t.
# Entrada: n (inteiro >= 1), sigma2 (número > 0).
# Saída: Vetor numérico de tamanho n.
# Pré-condição: fixar_semente() chamada no main.R.
simular_passeio_aleatorio <- function(n, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  # Separa o acúmulo do sorteio para ex4_eficiencia.R cronometrar apenas o loop.
  X <- acumular_passeio_aleatorio(eps)

  return(X)
}

# Simula o AR(1): X_t = phi * X_{t-1} + eps_t.
# Entrada: n (inteiro >= 1), phi (número com |phi| < 1), sigma2 (número > 0).
# Saída: Vetor numérico de tamanho n.
# Pré-condição: fixar_semente() chamada no main.R.
simular_ar1 <- function(n, phi, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  X <- numeric(n)

  # Condição inicial: X_1 = eps_1.
  X[1] <- eps[1]

  # O loop começa em t = 2 e usa X[t-1], já calculado, para obter X[t].
  # Diferente do passeio, o valor anterior é multiplicado por phi antes de somar eps[t].
  # Com |phi| < 1, a influência do passado diminui e a série oscila em torno de zero.
  # Com phi = 1, o processo seria um passeio aleatório.
  if(n >= 2){

    for(t in 2:n){

      X[t] <- phi * X[t-1] + eps[t]
    }
  }

  return(X)
}
