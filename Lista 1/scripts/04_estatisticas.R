############################################################
# 04_estatisticas.R
# Média, variância e covariância entre as trajetórias em cada
# instante, e média e variância de uma série (sem gráficos).
# Dependências: nenhuma.
############################################################

# média de cada instante entre as m trajetórias
# entrada: trajetorias (matriz n x m, uma coluna por trajetória)
# saída: vetor de tamanho n com a média em cada instante t
calcular_media_trajetoria <- function(trajetorias){

  n <- nrow(trajetorias)
  m <- ncol(trajetorias)

  media <- numeric(n)

  # percorre os instantes e soma as m trajetórias
  for(t in 1:n){

    soma <- 0

    for(i in 1:m){
      soma <- soma + trajetorias[t,i]
    }

    media[t] <- soma / m
  }

  return(media)
}

# variância amostral de cada instante entre as m trajetórias,
# s_t^2 = soma((x_it - media_t)^2) / (m - 1)
# entrada: trajetorias (matriz n x m, uma coluna por trajetória)
# saída: vetor de tamanho n com a variância em cada instante t
# pré-condição: m >= 2
calcular_variancia_trajetoria <- function(trajetorias){

  n <- nrow(trajetorias)
  m <- ncol(trajetorias)

  if(m < 2){
    stop("São necessárias pelo menos duas trajetórias.")
  }

  variancia <- numeric(n)

  for(t in 1:n){
    soma <- 0

    for(i in 1:m){
      soma <- soma + trajetorias[t,i]
    }

    media <- soma / m

    soma <- 0

    # soma dos desvios ao quadrado em torno da média do instante
    for(i in 1:m){
      d <- trajetorias[t,i] - media
      soma <- soma + d*d
    }

    variancia[t] <- soma / (m-1)
  }

  return(variancia)
}

# covariância amostral entre os instantes t e t-1 das m trajetórias,
# soma((x_it - media_t)(x_i,t-1 - media_t-1)) / (m - 1)
# entrada: trajetorias (matriz n x m, uma coluna por trajetória)
# saída: vetor de tamanho n; a posição 1 é NA, pois não há instante anterior
# pré-condição: m >= 2
calcular_covariancia_trajetoria <- function(trajetorias){

  n <- nrow(trajetorias)
  m <- ncol(trajetorias)

  if(m < 2){
    stop("São necessárias pelo menos duas trajetórias.")
  }

  covariancia <- numeric(n)
  covariancia[1] <- NA

  for(t in 2:n){

    soma_t <- 0
    soma_anterior <- 0

    for(i in 1:m){
      soma_t <- soma_t + trajetorias[t,i]
      soma_anterior <- soma_anterior + trajetorias[t-1,i]
    }

    media_t <- soma_t / m
    media_anterior <- soma_anterior / m

    soma <- 0

    # produto dos desvios nos dois instantes
    for(i in 1:m){
      d1 <- trajetorias[t,i] - media_t
      d2 <- trajetorias[t-1,i] - media_anterior

      soma <- soma + d1*d2
    }

    covariancia[t] <- soma / (m-1)
  }

  return(covariancia)
}

# média de uma série, ao longo do tempo
# entrada: serie (vetor numérico)
# saída: um número, a média da série
calcular_media_serie <- function(serie){
  return(mean(serie))
}

# variância amostral de uma série, ao longo do tempo
# entrada: serie (vetor numérico, tamanho >= 2)
# saída: um número, a variância da série
calcular_variancia_serie <- function(serie){
  return(var(serie))
}
