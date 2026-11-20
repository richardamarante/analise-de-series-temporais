############################################################
# Arquivo: estatisticas.R
# Calcula média, variância e covariância entre trajetórias e estatísticas de uma série.
# Dependências: nenhuma.
############################################################

# Calcula a média das m trajetórias em cada instante.
# Entrada: trajetorias (matriz n x m, uma coluna por trajetória).
# Saída: vetor de tamanho n com a média em cada instante.
calcular_media_trajetoria <- function(trajetorias){

  n <- nrow(trajetorias)
  m <- ncol(trajetorias)

  media <- numeric(n)

  # Percorre os instantes e soma os valores das m trajetórias.
  for(t in 1:n){

    soma <- 0

    for(i in 1:m){
      soma <- soma + trajetorias[t,i]
    }

    media[t] <- soma / m
  }

  return(media)
}

# Calcula a variância amostral entre trajetórias: s_t^2 = soma((x_it - media_t)^2)/(m - 1).
# Entrada: trajetorias (matriz n x m, uma coluna por trajetória).
# Saída: vetor de tamanho n com a variância em cada instante.
# Pré-condição: m >= 2.
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

    # Soma os desvios ao quadrado em relação à média de cada instante.
    for(i in 1:m){
      d <- trajetorias[t,i] - media
      soma <- soma + d*d
    }

    variancia[t] <- soma / (m-1)
  }

  return(variancia)
}

# Covariância amostral entre t e t-1: soma((x_it - media_t)(x_i,t-1 - media_t-1))/(m-1).
# Entrada: trajetorias (matriz n x m, uma coluna por trajetória).
# Saída: vetor de tamanho n, com NA na primeira posição.
# Pré-condição: m >= 2.
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

    # Soma os produtos dos desvios entre instantes consecutivos.
    for(i in 1:m){
      d1 <- trajetorias[t,i] - media_t
      d2 <- trajetorias[t-1,i] - media_anterior

      soma <- soma + d1*d2
    }

    covariancia[t] <- soma / (m-1)
  }

  return(covariancia)
}

# Calcula a média de uma série ao longo do tempo.
# Entrada: serie (vetor numérico).
# Saída: valor da média.
calcular_media_serie <- function(serie){
  return(mean(serie))
}

# Calcula a variância amostral de uma série ao longo do tempo.
# Entrada: serie (vetor numérico).
# Saída: valor da variância.
# Pré-condição: serie com pelo menos 2 valores.
calcular_variancia_serie <- function(serie){
  return(var(serie))
}
