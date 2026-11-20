############################################################
# 03_processamento.R
# Média móvel, média amostral, autocovariâncias e FAC empírica
# (sem gráficos).
# Dependências: nenhuma.
############################################################

# média móvel centrada de raio q: m_t = (X[t-q] + ... + X[t+q]) / (2q + 1)
# entrada: serie (vetor numérico de tamanho n), q (inteiro com 2q + 1 <= n)
# saída: vetor com as n - 2q médias; a primeira corresponde a t = q + 1
# pré-condição: serie sem NA
calcular_media_movel <- function(serie, q){

  n <- length(serie)

  numero_medias <- n - 2*q

  medias <- numeric(numero_medias)

  if(numero_medias > 0){

    # t é o centro da janela, de q+1 até n-q
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

# média amostral: soma de X_t dividida por n
# entrada: serie (vetor numérico de tamanho n >= 1)
# saída: número
# pré-condição: serie sem NA
calcular_media_amostral <- function(serie){

  n <- length(serie)

  soma <- 0

  for(t in 1:n){
    soma <- soma + serie[t]
  }

  media <- soma / n

  return(media)
}

# autocovariância empírica: gamma(h) = (1/n) soma de (X_t - média)(X_{t+h} - média), t = 1, ..., n-h
# entrada: serie (vetor numérico de tamanho n), h (inteiro com 0 <= h < n)
# saída: número
# pré-condição: serie sem NA
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

# autocovariâncias de h = 0 até max_lag
# entrada: serie (vetor numérico de tamanho n), max_lag (inteiro com 0 <= max_lag < n)
# saída: vetor com max_lag + 1 valores, a posição h+1 guarda gamma(h)
# pré-condição: serie sem NA
calcular_autocovariancias <- function(serie, max_lag){

  autocovariancias <- numeric(max_lag + 1)

  for(h in 0:max_lag){
    autocovariancias[h+1] <- calcular_autocovariancia_empirica(serie, h)
  }

  return(autocovariancias)
}

# função de autocorrelação empírica: rho(h) = gamma(h) / gamma(0)
# entrada: serie (vetor numérico de tamanho n), max_lag (inteiro com 0 <= max_lag < n)
# saída: vetor com max_lag + 1 valores, rho(0) = 1
# pré-condição: serie sem NA e não constante
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
