############################################################
# processamento.R
# Calcula média, autocovariâncias e FAC empírica, além da FAC teórica do AR(1).
# Dependências: nenhuma. Não gera gráficos.
############################################################

# Calcula a média empírica somando os valores da série e dividindo por n.
# Entrada: serie é um vetor numérico de tamanho n, sem NA.
# Saída: número com a média empírica.
calcular_media_empirica <- function(serie){

  n <- length(serie)

  soma <- 0

  for(t in 1:n){

    soma <- soma + serie[t]
  }

  media <- soma / n

  return(media)
}

# Calcula gamma(h) = (1/n) * soma_{t=1}^{n-h} (X_t - media)(X_{t+h} - media).
# Entrada: serie é um vetor numérico de tamanho n, sem NA, e h é um inteiro com 0 <= h < n.
# Saída: número com a autocovariância gamma(h).
calcular_autocovariancia_empirica <- function(serie, h){

  n <- length(serie)

  media <- calcular_media_empirica(serie)

  soma <- 0

  if(h == 0){

    # Em h = 0, os dois desvios são iguais e a soma dos quadrados, dividida por n, dá a variância.
    for(t in 1:n){

      d <- serie[t] - media

      soma <- soma + d * d
    }

  } else {

    # Percorre até t = n-h para que o segundo índice, t+h, não ultrapasse o tamanho da série.
    for(t in 1:(n-h)){

      d1 <- serie[t] - media

      d2 <- serie[t+h] - media

      soma <- soma + d1 * d2
    }
  }

  autocovariancia <- soma / n

  return(autocovariancia)
}

# Calcula as autocovariâncias para todos os lags de 0 até max_lag.
# Entrada: serie é um vetor numérico de tamanho n e max_lag é um inteiro com 0 <= max_lag < n.
# Saída: vetor de tamanho max_lag + 1, com gamma(h) na posição h+1.
calcular_autocovariancias <- function(serie, max_lag){

  autocovariancias <- numeric(max_lag + 1)

  # Guarda gamma(h) na posição h+1 porque os índices dos vetores em R começam em 1.
  for(h in 0:max_lag){

    autocovariancias[h+1] <-
      calcular_autocovariancia_empirica(serie, h)
  }

  return(autocovariancias)
}

# Calcula a FAC empírica dividindo as autocovariâncias por gamma(0), com rho(0) = 1.
# Entrada: serie é um vetor numérico não constante de tamanho n e max_lag é um inteiro
# com 0 <= max_lag < n.
# Saída: vetor de tamanho max_lag + 1, com rho(h) na posição h+1.
calcular_fac_empirica <- function(serie, max_lag){

  autocovariancias <-
    calcular_autocovariancias(serie, max_lag)

  gamma0 <- autocovariancias[1]

  fac <- numeric(max_lag + 1)

  fac[1] <- 1

  if(max_lag >= 1){

    for(h in 1:max_lag){

      fac[h+1] <-
        autocovariancias[h+1] / gamma0
    }
  }

  return(fac)
}

# Calcula a FAC teórica do AR(1), usando rho(h) = phi^h para cada lag.
# Entrada: phi é um número com |phi| < 1 e max_lag é um inteiro >= 0.
# Saída: vetor de tamanho max_lag + 1, com phi^0, ..., phi^max_lag.
calcular_fac_teorica_ar1 <- function(phi, max_lag){

  fac_teorica <- numeric(max_lag + 1)

  for(h in 0:max_lag){

    fac_teorica[h+1] <- phi^h
  }

  return(fac_teorica)
}
