############################################################
# 05_eficiencia.R
# Versões vetorizadas da matriz X, da simulação, da média móvel
# e das autocovariâncias, e comparação com os loops (Exercício 4.3).
# Dependências: 01_config.R, 02_dados.R e 03_processamento.R.
############################################################

# matriz de regressão sem loop
# entrada: parametros (lista com n), t (vetor de tempo de tamanho n)
# saída: matriz n x 3 igual à de criar_matriz_regressao
criar_matriz_regressao_vetorizada <- function(parametros, t){
  X <- cbind(1, t, t^2)

  return(X)
}

# série com tendência com X %*% beta e rnorm(n) de uma vez
# entrada: beta (vetor com ncol(X) coeficientes), X (matriz n x p), sigma (desvio padrão, positivo)
# saída: vetor de tamanho n, igual ao de simular_serie_com_tendencia com a mesma semente
# pré-condição: length(beta) == ncol(X)
simular_serie_com_tendencia_vetorizada <- function(beta, X, sigma){

  n <- nrow(X)

  eps <- rnorm(n, mean = 0, sd = sigma)

  Xt <- as.numeric(X %*% beta) + eps

  return(Xt)
}

# autocovariância de defasagem h sem loop: soma dos produtos dos desvios deslocados h posições
# entrada: serie (vetor numérico de tamanho n), h (inteiro com 0 <= h < n)
# saída: número, igual ao de calcular_autocovariancia_empirica
# pré-condição: serie sem NA
calcular_autocovariancia_empirica_vetorizada <- function(serie, h){

  n <- length(serie)

  media <- mean(serie)

  d <- serie - media

  autocovariancia <- sum(d[1:(n-h)] * d[(1+h):n]) / n

  return(autocovariancia)
}

# autocovariâncias de h = 0 até max_lag, sem loop sobre t
# entrada: serie (vetor numérico de tamanho n), max_lag (inteiro com 0 <= max_lag < n)
# saída: vetor com max_lag + 1 valores
# pré-condição: serie sem NA
calcular_autocovariancias_vetorizada <- function(serie, max_lag){

  autocovariancias <- sapply(0:max_lag,
                             calcular_autocovariancia_empirica_vetorizada,
                             serie = serie)

  return(autocovariancias)
}

# média móvel centrada com soma acumulada: cada janela é a diferença de duas somas acumuladas
# entrada: serie (vetor numérico de tamanho n), q (inteiro com 2q + 1 <= n)
# saída: vetor com n - 2q médias, igual ao de calcular_media_movel
# pré-condição: serie sem NA
calcular_media_movel_vetorizada <- function(serie, q){

  n <- length(serie)

  numero_medias <- n - 2*q

  medias <- numeric(numero_medias)

  if(numero_medias > 0){

    # soma acumulada com zero na frente
    soma_acumulada <- c(0, cumsum(serie))

    medias <- (soma_acumulada[(2*q+2):(n+1)] - soma_acumulada[1:numero_medias]) / (2*q + 1)
  }

  return(medias)
}

# compara tempo (system.time, com repetições) e resultado (all.equal) entre loop e vetorizada
# entrada: parametros (lista com seed, sigma, q, max_lag, beta, n_eficiencia, repeticoes_eficiencia)
# saída: nenhuma, imprime tempos, razões e igualdade no console
# pré-condição: funções dos arquivos 01 a 03 carregadas
comparar_eficiencia <- function(parametros){

  parametros$n <- parametros$n_eficiencia
  repeticoes <- parametros$repeticoes_eficiencia
  sigma <- parametros$sigma
  q <- parametros$q
  max_lag <- parametros$max_lag

  t <- criar_tempo(parametros)
  beta <- parametros$beta

  # matriz X
  tempo_x_loop <- system.time({
    for(r in 1:repeticoes){
      X <- criar_matriz_regressao(parametros, t)
    }
  })[["elapsed"]]

  tempo_x_vet <- system.time({
    for(r in 1:repeticoes){
      X_vet <- criar_matriz_regressao_vetorizada(parametros, t)
    }
  })[["elapsed"]]

  # simulação, com a mesma semente nas duas versões
  tempo_sim_loop <- system.time({
    for(r in 1:repeticoes){
      set.seed(parametros$seed)
      Xt_loop <- simular_serie_com_tendencia(beta, X, sigma)
    }
  })[["elapsed"]]

  tempo_sim_vet <- system.time({
    for(r in 1:repeticoes){
      set.seed(parametros$seed)
      Xt_vet <- simular_serie_com_tendencia_vetorizada(beta, X, sigma)
    }
  })[["elapsed"]]

  # média móvel
  tempo_mm_loop <- system.time({
    for(r in 1:repeticoes){
      mm_loop <- calcular_media_movel(Xt_loop, q)
    }
  })[["elapsed"]]

  tempo_mm_vet <- system.time({
    for(r in 1:repeticoes){
      mm_vet <- calcular_media_movel_vetorizada(Xt_loop, q)
    }
  })[["elapsed"]]

  # autocovariâncias
  tempo_ac_loop <- system.time({
    for(r in 1:repeticoes){
      ac_loop <- calcular_autocovariancias(Xt_loop, max_lag)
    }
  })[["elapsed"]]

  tempo_ac_vet <- system.time({
    for(r in 1:repeticoes){
      ac_vet <- calcular_autocovariancias_vetorizada(Xt_loop, max_lag)
    }
  })[["elapsed"]]

  cat("Tamanho da série:", parametros$n, "\n")
  cat("Repetições:", repeticoes, "\n")

  cat("\nMatriz de regressão\n")
  cat("Tempo com loop (s):", round(tempo_x_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_x_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_x_loop / tempo_x_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(X, X_vet, check.attributes = FALSE)), "\n")

  cat("\nSimulação da série\n")
  cat("Tempo com loop (s):", round(tempo_sim_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_sim_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_sim_loop / tempo_sim_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(Xt_loop, Xt_vet)), "\n")

  cat("\nMédia móvel\n")
  cat("Tempo com loop (s):", round(tempo_mm_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_mm_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_mm_loop / tempo_mm_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(mm_loop, mm_vet)), "\n")

  cat("\nAutocovariâncias\n")
  cat("Tempo com loop (s):", round(tempo_ac_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_ac_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_ac_loop / tempo_ac_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(ac_loop, ac_vet)), "\n")
}
