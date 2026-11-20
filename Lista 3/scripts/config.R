############################################################
# config.R
# Lê os parâmetros do JSON, define a semente, o tempo e a matriz de regressão.
# Dependências: pacote jsonlite.
############################################################

# Lê os parâmetros da simulação em um arquivo JSON.
# Entrada: arquivo (caminho do arquivo JSON).
# Saída: lista com seed, n, sigma, q, max_lag, beta e opções de gráfico.
# Pré-condição: arquivo existente e pacote jsonlite carregado.
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# Cria o vetor de tempo t = 1, 2, ..., n.
# Entrada: parametros (lista com n, inteiro positivo).
# Saída: vetor de inteiros de tamanho n.
criar_tempo <- function(parametros){
  n <- parametros$n
  t <- 1:n
  return(t)
}

# Cria a matriz X do polinômio de grau 2, com linha i = (1, t_i, t_i²).
# Entrada: parametros (lista com n), t (vetor de tempo de tamanho n).
# Saída: matriz numérica n x 3.
# Pré-condição: length(t) == n.
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

# Fixa a semente do gerador de números aleatórios.
# Entrada: parametros (lista com seed, inteiro).
# Saída: nenhuma, apenas define a semente.
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}
