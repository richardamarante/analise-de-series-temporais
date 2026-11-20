############################################################
# processamento.R
# Calcula média móvel centrada, média amostral, autocovariâncias e FAC empírica.
# Dependências: nenhuma.
############################################################

# Calcula a média móvel centrada pela média dos 2q + 1 valores de cada janela.
# Entrada: serie numérica de tamanho n e q inteiro, com 2q + 1 <= n.
# Saída: vetor de tamanho n - 2q, começando no centro t = q+1.
# Pré-condição: serie sem NA.
calcular_media_movel <- function(serie, q){

  n <- length(serie)

  numero_medias <- n - 2*q

  medias <- numeric(numero_medias)

  if(numero_medias > 0){

    for(t in (q+1):(n-q)){

      soma <- 0

      for(j in (t-q):(t+q)){
        soma <- soma + serie[j]
      }
      medias[t-q] <- soma / (2*q + 1)
    }
  }

  return(medias)
}

# Calcula a média amostral somando os valores da série e dividindo por n.
# Entrada: serie numérica de tamanho n >= 1.
# Saída: um número.
# Pré-condição: serie sem NA.
calcular_media_amostral <- function(serie){

  n <- length(serie)

  soma <- 0

  for(t in 1:n){
    soma <- soma + serie[t]
  }

  media <- soma / n

  return(media)
}

# Calcula gamma(h) = (1/n) soma (X_t - media)(X_{t+h} - media), de t = 1 até n-h.
# Entrada: serie numérica de tamanho n e h inteiro, com 0 <= h < n.
# Saída: um número.
# Pré-condição: serie sem NA.
calcular_autocovariancia_empirica <- function(serie, h){

  n <- length(serie)

  media <- calcular_media_amostral(serie)

  soma <- 0

  if(h == 0){

    for(t in 1:n){
      d <- serie[t] - media
      soma <- soma + d * d
    }

  } else {

    if(h < n){

      for(t in 1:(n-h)){
        d1 <- serie[t] - media
        d2 <- serie[t+h] - media
        soma <- soma + d1 * d2
      }
    }
  }

  autocovariancia <- soma / n

  return(autocovariancia)
}

# Calcula as autocovariâncias de h = 0 até max_lag.
# Entrada: serie numérica de tamanho n e max_lag inteiro, com 0 <= max_lag < n.
# Saída: vetor de tamanho max_lag + 1, com gamma(h) na posição h+1.
# Pré-condição: serie sem NA.
calcular_autocovariancias <- function(serie, max_lag){

  autocovariancias <- numeric(max_lag + 1)

  for(h in 0:max_lag){
    autocovariancias[h+1] <- calcular_autocovariancia_empirica(serie, h)
  }

  return(autocovariancias)
}

# Calcula a FAC empírica por rho(h) = gamma(h)/gamma(0), com rho(0) = 1.
# Entrada: serie numérica de tamanho n e max_lag inteiro, com 0 <= max_lag < n.
# Saída: vetor de tamanho max_lag + 1.
# Pré-condição: serie sem NA e não constante.
calcular_fac_empirica <- function(serie, max_lag){

  autocovariancias <- calcular_autocovariancias(serie, max_lag)

  gamma0 <- autocovariancias[1]

  fac <- numeric(max_lag + 1)

  if(gamma0 != 0){

    for(h in 0:max_lag){
      fac[h+1] <- autocovariancias[h+1] / gamma0
    }
  }
  return(fac)
}
