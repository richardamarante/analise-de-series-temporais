############################################################
# 01_config.R
# Parâmetros (lidos de scripts/parametros.json), semente, vetor
# de tempo e matriz de regressão da tendência polinomial.
# Dependências: pacote jsonlite.
############################################################

# lê o arquivo json com os parâmetros da simulação
# entrada: arquivo (texto com o caminho do json)
# saída: lista com seed, n, sigma, q, max_lag, beta e opções de gráfico
# pré-condição: arquivo existe e jsonlite está carregado
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# vetor de tempo t = 1, 2, ..., n
# entrada: parametros (lista com n, inteiro positivo)
# saída: vetor de inteiros de tamanho n
criar_tempo <- function(parametros){
  n <- parametros$n
  t <- 1:n
  return(t)
}

# matriz de regressão do polinômio de grau 2: linha i = (1, t_i, t_i^2)
# entrada: parametros (lista com n), t (vetor de tempo de tamanho n)
# saída: matriz numérica n x 3
# pré-condição: length(t) == n
criar_matriz_regressao <- function(parametros, t){
  n <- parametros$n

  X <- matrix(
    0,
    nrow = n,
    ncol = 3
  )

  for(i in 1:n){

    X[i,1] <- 1

    X[i,2] <- t[i]

    X[i,3] <- t[i]^2
  }

  return(X)
}

# fixa a semente do gerador de números aleatórios
# entrada: parametros (lista com seed, inteiro)
# saída: nenhuma, só fixa o gerador
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}
