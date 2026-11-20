############################################################
# 03_processamento.R
# Média empírica, autocovariâncias e FAC empírica e teórica
# do AR(1) (sem gráficos).
# Dependências: nenhuma.
############################################################

# média empírica: soma de todos os valores dividida por n
# entrada: serie (vetor numérico de tamanho n, sem NA)
# saída: número
calcular_media_empirica <- function(serie){

  n <- length(serie)

  soma <- 0

  # acumula os n valores em soma
  for(t in 1:n){

    soma <- soma + serie[t]
  }

  media <- soma / n

  return(media)
}

# autocovariância empírica: gamma(h) = (1/n) * soma de (X_t - média)(X_{t+h} - média), t = 1, ..., n-h
# entrada: serie (vetor numérico de tamanho n, sem NA), h (inteiro, 0 <= h < n)
# saída: número, gamma(h)
calcular_autocovariancia_empirica <- function(serie, h){

  n <- length(serie)

  media <- calcular_media_empirica(serie)

  soma <- 0

  if(h == 0){

    # lag 0 é a variância: desvio ao quadrado de cada ponto
    for(t in 1:n){

      d <- serie[t] - media

      soma <- soma + d * d
    }

  } else {

    # multiplica o desvio de X_t pelo de X_{t+h}; t para em n-h
    # para que t+h não passe do fim da série
    for(t in 1:(n-h)){

      d1 <- serie[t] - media

      d2 <- serie[t+h] - media

      soma <- soma + d1 * d2
    }
  }

  autocovariancia <- soma / n

  return(autocovariancia)
}

# autocovariâncias de 0 até max_lag
# entrada: serie (vetor numérico de tamanho n), max_lag (inteiro, 0 <= max_lag < n)
# saída: vetor de tamanho max_lag + 1; a posição h+1 guarda gamma(h)
calcular_autocovariancias <- function(serie, max_lag){

  autocovariancias <- numeric(max_lag + 1)

  # o R indexa a partir de 1, então o lag h fica na posição h+1
  for(h in 0:max_lag){

    autocovariancias[h+1] <-
      calcular_autocovariancia_empirica(serie, h)
  }

  return(autocovariancias)
}

# FAC empírica: rho(h) = gamma(h) / gamma(0)
# entrada: serie (vetor numérico de tamanho n, não constante), max_lag (inteiro, 0 <= max_lag < n)
# saída: vetor de tamanho max_lag + 1; a posição h+1 guarda rho(h), com rho(0) = 1
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

# FAC teórica do AR(1): rho(h) = phi^h
# entrada: phi (número com |phi| < 1), max_lag (inteiro >= 0)
# saída: vetor de tamanho max_lag + 1 com phi^0, ..., phi^max_lag
calcular_fac_teorica_ar1 <- function(phi, max_lag){

  fac_teorica <- numeric(max_lag + 1)

  for(h in 0:max_lag){

    fac_teorica[h+1] <- phi^h
  }

  return(fac_teorica)
}
