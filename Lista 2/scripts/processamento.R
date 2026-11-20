############################################################
# processamento.R
# Calcula média, autocovariâncias e FAC empírica e teórica do AR(1).
# Dependências: nenhuma. Não gera gráficos.
############################################################

# Calcula a média empírica: soma dos valores dividida por n.
# Entrada: serie (vetor numérico de tamanho n, sem NA).
# Saída: Número com a média.
calcular_media_empirica <- function(serie){

  n <- length(serie)

  soma <- 0

  # Acumula os n valores da série em soma.
  for(t in 1:n){

    soma <- soma + serie[t]
  }

  media <- soma / n

  return(media)
}

# Calcula gamma(h) = (1/n) * soma_{t=1}^{n-h} (X_t - média)(X_{t+h} - média).
# Entrada: serie (vetor numérico de tamanho n, sem NA), h (inteiro, 0 <= h < n).
# Saída: Número com gamma(h).
calcular_autocovariancia_empirica <- function(serie, h){

  n <- length(serie)

  media <- calcular_media_empirica(serie)

  soma <- 0

  if(h == 0){

    # Para h = 0, soma os desvios ao quadrado para calcular a variância.
    for(t in 1:n){

      d <- serie[t] - media

      soma <- soma + d * d
    }

  } else {

    # Multiplica os desvios de X_t e X_{t+h} até t = n-h, sem ultrapassar o fim da série.
    for(t in 1:(n-h)){

      d1 <- serie[t] - media

      d2 <- serie[t+h] - media

      soma <- soma + d1 * d2
    }
  }

  autocovariancia <- soma / n

  return(autocovariancia)
}

# Calcula as autocovariâncias dos lags 0 até max_lag.
# Entrada: serie (vetor numérico de tamanho n), max_lag (inteiro, 0 <= max_lag < n).
# Saída: Vetor de tamanho max_lag + 1, com gamma(h) na posição h+1.
calcular_autocovariancias <- function(serie, max_lag){

  autocovariancias <- numeric(max_lag + 1)

  # Guarda gamma(h) na posição h+1 porque o R começa a indexar em 1.
  for(h in 0:max_lag){

    autocovariancias[h+1] <-
      calcular_autocovariancia_empirica(serie, h)
  }

  return(autocovariancias)
}

# Calcula a FAC empírica: rho(h) = gamma(h) / gamma(0).
# Entrada: serie (vetor numérico de tamanho n, não constante), max_lag (inteiro, 0 <= max_lag < n).
# Saída: Vetor de tamanho max_lag + 1, com rho(h) na posição h+1 e rho(0) = 1.
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

# Calcula a FAC teórica do AR(1): rho(h) = phi^h.
# Entrada: phi (número com |phi| < 1), max_lag (inteiro >= 0).
# Saída: Vetor de tamanho max_lag + 1 com phi^0, ..., phi^max_lag.
calcular_fac_teorica_ar1 <- function(phi, max_lag){

  fac_teorica <- numeric(max_lag + 1)

  for(h in 0:max_lag){

    fac_teorica[h+1] <- phi^h
  }

  return(fac_teorica)
}
