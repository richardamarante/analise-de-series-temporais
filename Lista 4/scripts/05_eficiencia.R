############################################################
# 05_eficiencia.R
# Versões vetorizadas de X'X, X'y, X·beta e do OLS e comparação
# com os loops (Exercício 4.3).
# Dependências: 01_config.R, 02_dados.R e 03_processamento.R.
############################################################

# X'X com crossprod, sem loops
# entrada: X (matriz n x p)
# saída: matriz p x p, igual à de calcular_XtX
calcular_XtX_vetorizada <- function(X){
  resultado <- crossprod(X)
  return(resultado)
}

# X'y com crossprod, sem loops
# entrada: X (matriz n x p), y (vetor de tamanho n)
# saída: vetor de tamanho p, igual ao de calcular_Xty
calcular_Xty_vetorizada <- function(X, y){
  resultado <- as.numeric(crossprod(X, y))
  return(resultado)
}

# X beta com o operador %*%, sem loops
# entrada: X (matriz n x p), beta (vetor de tamanho p)
# saída: vetor de tamanho n, igual ao de multiplicar_matriz_vetor
multiplicar_matriz_vetor_vetorizada <- function(X, beta){
  resultado <- as.numeric(X %*% beta)
  return(resultado)
}

# OLS com as versões vetorizadas de X'X e X'y: beta_hat = solve(X'X, X'y)
# entrada: X (matriz n x p), y (vetor de tamanho n)
# saída: vetor beta_hat de tamanho p
# pré-condição: X'X invertível
ajustar_ols_vetorizada <- function(X, y){

  XtX <- calcular_XtX_vetorizada(X)

  Xty <- calcular_Xty_vetorizada(X, y)

  beta_hat <- as.numeric(solve(XtX, Xty))

  return(beta_hat)
}

# compara loops e operações vetorizadas: exatidão do OLS com o n da série e tempo de X'X, X'y e X beta com n grande
# entrada: parametros (lista com seed, n_anos, s, K, grau_tendencia, beta_tendencia, alpha, beta_h, sigma,
#          n_eficiencia e repeticoes_eficiencia)
# saída: nenhuma; imprime os resultados no console
# pré-condição: parametros lido por ler_parametros
comparar_eficiencia <- function(parametros){

  fixar_semente(parametros)

  K <- parametros$K
  repeticoes <- parametros$repeticoes_eficiencia

  beta_sazonal <- criar_beta_sazonal(parametros$alpha, parametros$beta_h, K)

  # exatidão: OLS completo com o n da série, contra o lm()
  t <- criar_tempo(parametros)

  X_tendencia <- criar_matriz_tendencia(t, parametros$grau_tendencia)
  X_sazonal <- criar_matriz_fourier(t, parametros$s, K)
  X <- cbind(X_tendencia, X_sazonal)

  y <- simular_serie(parametros$beta_tendencia, X_tendencia, X_sazonal,
                     beta_sazonal, parametros$sigma)

  beta_loops <- ajustar_ols(X, y)
  beta_vetorizada <- ajustar_ols_vetorizada(X, y)
  beta_lm <- as.numeric(coef(lm(y ~ X - 1)))

  cat("Tamanho da série:", length(t), "\n")
  cat("Coeficientes iguais nas duas versões:", isTRUE(all.equal(beta_loops, beta_vetorizada)), "\n")
  cat("Coeficientes iguais ao lm() (loops):", isTRUE(all.equal(beta_loops, beta_lm)), "\n")
  cat("Coeficientes iguais ao lm() (vetorizada):", isTRUE(all.equal(beta_vetorizada, beta_lm)), "\n")

  # tempo: X'X, X'y e X beta com n grande (sem o solve)
  n_grande <- parametros$n_eficiencia

  t_grande <- 1:n_grande

  X_grande <- cbind(criar_matriz_tendencia(t_grande, parametros$grau_tendencia),
                    criar_matriz_fourier(t_grande, parametros$s, K))

  y_grande <- rnorm(n_grande)

  beta_teste <- c(parametros$beta_tendencia, beta_sazonal)

  cat("Tamanho da amostra para o tempo:", n_grande, "\n")
  cat("Colunas de X:", ncol(X_grande), "\n")
  cat("Repetições:", repeticoes, "\n")

  tempo_loop_XtX <- system.time({
    for(r in 1:repeticoes){
      resultado_loop_XtX <- calcular_XtX(X_grande)
    }
  })[["elapsed"]]

  tempo_vet_XtX <- system.time({
    for(r in 1:repeticoes){
      resultado_vet_XtX <- calcular_XtX_vetorizada(X_grande)
    }
  })[["elapsed"]]

  tempo_loop_Xty <- system.time({
    for(r in 1:repeticoes){
      resultado_loop_Xty <- calcular_Xty(X_grande, y_grande)
    }
  })[["elapsed"]]

  tempo_vet_Xty <- system.time({
    for(r in 1:repeticoes){
      resultado_vet_Xty <- calcular_Xty_vetorizada(X_grande, y_grande)
    }
  })[["elapsed"]]

  tempo_loop_mv <- system.time({
    for(r in 1:repeticoes){
      resultado_loop_mv <- multiplicar_matriz_vetor(X_grande, beta_teste)
    }
  })[["elapsed"]]

  tempo_vet_mv <- system.time({
    for(r in 1:repeticoes){
      resultado_vet_mv <- multiplicar_matriz_vetor_vetorizada(X_grande, beta_teste)
    }
  })[["elapsed"]]

  operacoes <- c("X'X", "X'y", "X·beta")
  tempo_loops <- c(tempo_loop_XtX, tempo_loop_Xty, tempo_loop_mv)
  tempo_vetorizado <- c(tempo_vet_XtX, tempo_vet_Xty, tempo_vet_mv)

  iguais <- c(isTRUE(all.equal(resultado_loop_XtX, resultado_vet_XtX)),
              isTRUE(all.equal(resultado_loop_Xty, resultado_vet_Xty)),
              isTRUE(all.equal(resultado_loop_mv, resultado_vet_mv)))

  for(i in 1:length(operacoes)){
    cat("Operação:", operacoes[i], "\n")
    cat("Tempo com loops (s):", round(tempo_loops[i], 3), "\n")
    cat("Tempo vetorizado (s):", round(tempo_vetorizado[i], 3), "\n")
    cat("Razão (loops / vetorizado):", round(tempo_loops[i] / tempo_vetorizado[i], 1), "\n")
    cat("Mesmo resultado nas duas versões:", iguais[i], "\n")
  }
}
