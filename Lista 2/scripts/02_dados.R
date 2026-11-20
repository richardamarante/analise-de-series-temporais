############################################################
# 02_dados.R
# Simulação do ruído branco, do passeio aleatório e do AR(1).
# Dependências: nenhuma.
############################################################

# ruído branco: eps_t ~ N(0, sigma2), independentes
# entrada: n (inteiro >= 1), sigma2 (número > 0)
# saída: vetor numérico de tamanho n
simular_ruido_branco <- function(n, sigma2){
  # rnorm(n) sorteia os n valores de uma vez, na mesma ordem dos sorteios um a um
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# passeio aleatório: X_t = X_{t-1} + eps_t
# entrada: n (inteiro >= 1), sigma2 (número > 0)
# saída: vetor numérico de tamanho n
# pré-condição: fixar_semente() já chamada (feito no main.R)
simular_passeio_aleatorio <- function(n, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  X <- numeric(n)

  # não existe X_0 para somar ao primeiro choque, então X[1] recebe só eps[1]
  X[1] <- eps[1]

  # o loop começa em t = 2 porque X[1] já foi preenchido. em cada passo
  # lê o valor anterior X[t-1], soma o choque novo eps[t] e grava em X[t].
  # tem que ir em ordem crescente de t, porque X[t] depende de X[t-1],
  # que já precisa estar pronto. no fim, X_t = eps_1 + ... + eps_t
  if(n >= 2){

    for(t in 2:n){

      X[t] <- X[t-1] + eps[t]
    }
  }

  return(X)
}

# processo AR(1): X_t = phi * X_{t-1} + eps_t
# entrada: n (inteiro >= 1), phi (número com |phi| < 1), sigma2 (número > 0)
# saída: vetor numérico de tamanho n
# pré-condição: fixar_semente() já chamada (feito no main.R)
simular_ar1 <- function(n, phi, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  X <- numeric(n)

  # sem X_0, então X[1] recebe só eps[1]
  X[1] <- eps[1]

  # mesmo loop do passeio, começando em t = 2 pelo mesmo motivo, mas em cada
  # passo o valor anterior X[t-1] entra multiplicado por phi antes de somar
  # eps[t]. com |phi| < 1 a influência do passado encolhe (phi, phi^2, ...) e a série
  # volta para a média 0; com phi = 1 seria o passeio aleatório
  if(n >= 2){

    for(t in 2:n){

      X[t] <- phi * X[t-1] + eps[t]
    }
  }

  return(X)
}
