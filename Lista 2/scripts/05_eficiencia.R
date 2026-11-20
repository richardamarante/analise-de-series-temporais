############################################################
# 05_eficiencia.R
# Versões vetorizadas da soma acumulada e das autocovariâncias
# e comparação de tempo com os loops (Exercício 4.3).
# Dependências: 02_dados.R e 03_processamento.R.
############################################################

# passeio aleatório pela soma acumulada: X_t = eps_1 + ... + eps_t
# entrada: n (inteiro >= 1), sigma2 (número > 0)
# saída: vetor numérico de tamanho n, igual ao do loop para a mesma semente
simular_passeio_aleatorio_vetorizada <- function(n, sigma2){

  eps <- simular_ruido_branco(n, sigma2)

  # cumsum faz em código compilado a mesma soma que o loop fazia passo a passo
  X <- cumsum(eps)

  return(X)
}

# autocovariâncias de 0 até max_lag sem o loop interno sobre t
# entrada: serie (vetor numérico de tamanho n), max_lag (inteiro, 0 <= max_lag < n)
# saída: vetor de tamanho max_lag + 1 com gamma(0), ..., gamma(max_lag)
calcular_autocovariancias_vetorizada <- function(serie, max_lag){

  n <- length(serie)

  # os desvios são calculados uma única vez (o loop refazia a média a cada lag)
  d <- serie - mean(serie)

  autocovariancias <- numeric(max_lag + 1)

  # d[1:(n-h)] e d[(1+h):n] são a mesma série deslocada h posições:
  # o produto elemento a elemento e o sum() dão a soma de (X_t - média)(X_{t+h} - média).
  # com h = 0 vira a soma dos quadrados, então o if(h == 0) deixa de ser necessário
  for(h in 0:max_lag){
    autocovariancias[h+1] <- sum(d[1:(n-h)] * d[(1+h):n]) / n
  }

  return(autocovariancias)
}

# FAC empírica a partir das autocovariâncias vetorizadas
# entrada: serie (vetor numérico de tamanho n, não constante), max_lag (inteiro, 0 <= max_lag < n)
# saída: vetor de tamanho max_lag + 1 com rho(0), ..., rho(max_lag)
calcular_fac_empirica_vetorizada <- function(serie, max_lag){

  autocovariancias <- calcular_autocovariancias_vetorizada(serie, max_lag)

  return(autocovariancias / autocovariancias[1])
}

# compara tempo e resultado das versões com loop e vetorizadas e imprime no console
# entrada: parametros (lista com seed, sigma2, max_lag, n_eficiencia e repeticoes_eficiencia)
# saída: nenhuma (só imprime); o AR(1) não entra porque cada X_t depende do X_{t-1} já calculado
comparar_eficiencia <- function(parametros){

  n <- parametros$n_eficiencia
  sigma2 <- parametros$sigma2
  max_lag <- parametros$max_lag
  repeticoes <- parametros$repeticoes_eficiencia

  cat("n:", n, "\n")
  cat("max_lag:", max_lag, "\n")
  cat("repetições:", repeticoes, "\n")

  # mesma semente nas duas versões: os choques são iguais e as séries devem coincidir
  fixar_semente(parametros)
  x_loop <- simular_passeio_aleatorio(n, sigma2)
  fixar_semente(parametros)
  x_vet <- simular_passeio_aleatorio_vetorizada(n, sigma2)

  tempo_loop <- system.time(for(r in 1:repeticoes) simular_passeio_aleatorio(n, sigma2))[["elapsed"]]
  tempo_vet <- system.time(for(r in 1:repeticoes) simular_passeio_aleatorio_vetorizada(n, sigma2))[["elapsed"]]

  cat("\nPasseio aleatório (soma acumulada)\n")
  cat("Tempo do loop for (s):", round(tempo_loop, 3), "\n")
  cat("Tempo do cumsum (s):", round(tempo_vet, 3), "\n")
  cat("Razão loop / cumsum:", round(tempo_loop / tempo_vet, 3), "\n")
  cat("Maior diferença entre as séries:", round(max(abs(x_loop - x_vet)), 4), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(x_loop, x_vet)), "\n")

  # as autocovariâncias são calculadas sobre a mesma série nas duas versões
  serie <- x_vet

  gama_loop <- calcular_autocovariancias(serie, max_lag)
  gama_vet <- calcular_autocovariancias_vetorizada(serie, max_lag)

  tempo_loop <- system.time(for(r in 1:repeticoes) calcular_autocovariancias(serie, max_lag))[["elapsed"]]
  tempo_vet <- system.time(for(r in 1:repeticoes) calcular_autocovariancias_vetorizada(serie, max_lag))[["elapsed"]]

  cat("\nAutocovariâncias de 0 a max_lag\n")
  cat("Tempo dos loops for (s):", round(tempo_loop, 3), "\n")
  cat("Tempo da versão vetorizada (s):", round(tempo_vet, 3), "\n")
  cat("Razão loop / vetorizada:", round(tempo_loop / tempo_vet, 3), "\n")
  cat("Maior diferença entre as autocovariâncias:", round(max(abs(gama_loop - gama_vet)), 4), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(gama_loop, gama_vet)), "\n")

  fac_loop <- calcular_fac_empirica(serie, max_lag)
  fac_vet <- calcular_fac_empirica_vetorizada(serie, max_lag)

  cat("\nFAC empírica\n")
  cat("Maior diferença entre as FACs:", round(max(abs(fac_loop - fac_vet)), 4), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(fac_loop, fac_vet)), "\n")
}
