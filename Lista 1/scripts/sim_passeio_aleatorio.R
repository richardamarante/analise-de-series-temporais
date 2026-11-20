############################################################
# Arquivo: sim_passeio_aleatorio.R
# Simula uma ou várias trajetórias de passeio aleatório.
# Dependências: sim_ruido_branco.R.
############################################################

# Simula um passeio aleatório com X_t = X_{t-1} + eps_t e X_1 = eps_1.
# Entrada: n, tamanho da série, e sigma2, variância do ruído.
# Saída: vetor de tamanho n com a trajetória simulada.
simular_passeio_aleatorio <- function(n, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  X <- numeric(n)

  X[1] <- eps[1]

  # A partir de t = 2, acumula os ruídos, sem entrar no laço quando n = 1.
  if(n >= 2){
    for(t in 2:n){
      X[t] <- X[t-1] + eps[t]
    }
  }

  return(X)
}

# Simula m trajetórias independentes de passeio aleatório.
# Entrada: n, tamanho de cada trajetória, m, quantidade, e sigma2, variância do ruído.
# Saída: matriz n x m com uma trajetória por coluna.
simular_multiplos_passeio_aleatorio <- function(n, m, sigma2){

  trajetorias <- matrix(0, nrow = n, ncol = m)

  for(i in 1:m){
    trajetorias[, i] <- simular_passeio_aleatorio(n, sigma2)
  }

  return(trajetorias)
}
