############################################################
# Arquivo: sim_ruido_branco.R
# Simula uma ou várias trajetórias de ruído branco gaussiano.
# Dependências: nenhuma.
############################################################

# Simula uma série de ruído branco gaussiano com eps_t ~ N(0, sigma2).
# Entrada: n, tamanho da série, e sigma2, variância do ruído.
# Saída: vetor de tamanho n com a série simulada.
simular_ruido_branco <- function(n, sigma2){
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# Simula m trajetórias independentes de ruído branco.
# Entrada: n, tamanho de cada série, m, quantidade, e sigma2, variância do ruído.
# Saída: matriz n x m com uma trajetória por coluna.
# :::
simular_multiplas_trajetorias_ruido <- function(n, m, sigma2){

  trajetorias <- matrix(0, nrow = n, ncol = m)

  for(i in 1:m){
    trajetorias[, i] <- simular_ruido_branco(n, sigma2)
  }

  return(trajetorias)
}
