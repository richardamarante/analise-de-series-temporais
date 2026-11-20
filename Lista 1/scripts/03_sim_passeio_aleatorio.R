############################################################
# 03_sim_passeio_aleatorio.R
# Simula o passeio aleatório: uma trajetória ou várias.
# Dependências: 02_sim_ruido_branco.R.
############################################################

# simula um passeio aleatório, X_t = X_{t-1} + eps_t, com X_1 = eps_1
# entrada: n (tamanho da série), sigma2 (variância do ruído eps_t)
# saída: vetor de tamanho n com a trajetória X_1, ..., X_n
simular_passeio_aleatorio <- function(n, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  X <- numeric(n)

  # condição inicial
  X[1] <- eps[1]

  # soma acumulada dos ruídos, passo a passo
  if(n >= 2){
    for(t in 2:n){
      X[t] <- X[t-1] + eps[t]
    }
  }

  return(X)
}

# simula m passeios aleatórios independentes
# entrada: n (tamanho de cada trajetória), m (número de trajetórias),
#          sigma2 (variância do ruído)
# saída: matriz n x m, uma coluna por trajetória
simular_multiplos_passeio_aleatorio <- function(n, m, sigma2){

  trajetorias <- matrix(0, nrow = n, ncol = m)

  for(i in 1:m){
    trajetorias[, i] <- simular_passeio_aleatorio(n, sigma2)
  }

  return(trajetorias)
}
