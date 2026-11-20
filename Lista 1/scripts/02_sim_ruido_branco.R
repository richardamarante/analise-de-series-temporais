############################################################
# 02_sim_ruido_branco.R
# Simula o ruído branco gaussiano: uma série ou várias trajetórias.
# Dependências: nenhuma.
############################################################

# simula uma série de ruído branco, eps_t ~ N(0, sigma2)
# entrada: n (tamanho da série), sigma2 (variância do ruído)
# saída: vetor de tamanho n com a série simulada
simular_ruido_branco <- function(n, sigma2){
  return(rnorm(n, mean = 0, sd = sqrt(sigma2)))
}

# simula m trajetórias independentes de ruído branco
# entrada: n (tamanho de cada série), m (número de trajetórias),
#          sigma2 (variância do ruído)
# saída: matriz n x m, uma coluna por trajetória
simular_multiplas_trajetorias_ruido <- function(n, m, sigma2){

  trajetorias <- matrix(0, nrow = n, ncol = m)

  for(i in 1:m){
    trajetorias[, i] <- simular_ruido_branco(n, sigma2)
  }

  return(trajetorias)
}
