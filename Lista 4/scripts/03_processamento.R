############################################################
# 03_processamento.R
# OLS pelas equações normais, média móvel, diagnóstico dos
# resíduos, testes t e de Box-Pierce e previsão (sem gráficos).
# Dependências: 02_dados.R.
############################################################

# média móvel centrada de raio q: m_t = (X[t-q] + ... + X[t+q]) / (2q + 1)
# entrada: serie (vetor numérico de tamanho n), q (inteiro com 2q + 1 <= n)
# saída: vetor com as n - 2q médias; a primeira corresponde a t = q + 1
calcular_media_movel <- function(serie, q){

  n <- length(serie)

  numero_medias <- n - 2*q

  medias <- numeric(numero_medias)

  if(numero_medias > 0){

    for(t in (q+1):(n-q)){

      soma <- 0

      for(j in (t-q):(t+q)){

        soma <- soma + serie[j]
      }

      medias[t-q] <- soma / (2*q + 1)
    }
  }
  return(medias)
}

# média amostral: soma de X[i] dividida por n
# entrada: serie (vetor numérico de tamanho n >= 1, sem NA)
# saída: um número
calcular_media_amostral <- function(serie){

  n <- length(serie)

  soma <- 0

  for(i in 1:n){
    soma <- soma + serie[i]
  }

  media <- soma / n

  return(media)
}

# produto matriz-vetor: resultado[i] = soma_j X[i,j] beta[j]
# entrada: X (matriz n x p), beta (vetor de tamanho p)
# saída: vetor de tamanho n
multiplicar_matriz_vetor <- function(X, beta){

  n <- nrow(X)

  numero_colunas <- ncol(X)

  resultado <- numeric(n)

  for(i in 1:n){

    valor <- 0

    for(j in 1:numero_colunas){

      valor <- valor + X[i,j] * beta[j]
    }

    resultado[i] <- valor
  }

  return(resultado)
}

# X'X: elemento (i,j) é o produto interno da coluna i com a coluna j de X (3 loops)
# entrada: X (matriz n x p)
# saída: matriz p x p, simétrica
calcular_XtX <- function(X){

  numero_linhas <- nrow(X)

  numero_colunas <- ncol(X)

  resultado <- matrix(0, nrow = numero_colunas, ncol = numero_colunas)

  for(i in 1:numero_colunas){

    for(j in 1:numero_colunas){

      soma <- 0

      for(k in 1:numero_linhas){

        soma <- soma + X[k,i] * X[k,j]

      }

      resultado[i,j] <- soma
    }
  }

  return(resultado)
}

# X'y: elemento i é o produto interno da coluna i de X com y (2 loops)
# entrada: X (matriz n x p), y (vetor de tamanho n)
# saída: vetor de tamanho p
calcular_Xty <- function(X, y){

  numero_linhas <- nrow(X)

  numero_colunas <- ncol(X)

  resultado <- numeric(numero_colunas)

  for(i in 1:numero_colunas){

    soma <- 0

    for(k in 1:numero_linhas){

      soma <- soma + X[k,i] * y[k]
    }

    resultado[i] <- soma
  }

  return(resultado)
}

# mínimos quadrados pelas equações normais: beta_hat = (X'X)^-1 X'y, com solve(X'X, X'y)
# entrada: X (matriz n x p), y (vetor de tamanho n)
# saída: vetor beta_hat de tamanho p
# pré-condição: X'X invertível, ou seja, colunas de X linearmente independentes
ajustar_ols <- function(X, y){

  XtX <- calcular_XtX(X)

  Xty <- calcular_Xty(X, y)

  beta_hat <- solve(XtX, Xty)

  beta_hat <- as.numeric(beta_hat)

  return(beta_hat)
}

# ajusta tendência polinomial mais sazonalidade por OLS e separa os componentes estimados
# entrada: t (vetor de tamanho n), y (vetor de tamanho n), grau_tendencia (inteiro), periodo (inteiro),
#          K (inteiro), metodo_sazonal ("fourier" ou "dummies")
# saída: lista com beta_hat, y_hat, tendencia, sazonalidade, residuos, X_full e os argumentos do ajuste
# pré-condição: n maior que o número de colunas de X_full
ajustar_modelo <- function(t, y, grau_tendencia = 2, periodo = 12, K = 2, metodo_sazonal = "fourier"){

  t <- as.numeric(t)

  y <- as.numeric(y)

  # matriz de tendência
  X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)

  # matriz de sazonalidade
  if(metodo_sazonal == "fourier"){

    X_sazonal <- criar_matriz_fourier(t, periodo, K)

  } else if(metodo_sazonal == "dummies"){

    X_sazonal <- criar_matriz_dummies(t, periodo)

  } else {

    stop("Método sazonal inválido")
  }

  # matriz completa
  X_full <- cbind(X_tendencia, X_sazonal)

  # estimação dos coeficientes
  beta_hat <- ajustar_ols(X_full, y)

  # valores ajustados
  y_hat <- multiplicar_matriz_vetor(X_full, beta_hat)

  # coeficientes da tendência
  numero_colunas_tendencia <- ncol(X_tendencia)

  beta_tendencia_hat <- numeric(numero_colunas_tendencia)

  for(i in 1:numero_colunas_tendencia){

    beta_tendencia_hat[i] <- beta_hat[i]

  }

  # coeficientes da sazonalidade
  numero_colunas_sazonal <- ncol(X_sazonal)

  beta_sazonal_hat <- numeric(numero_colunas_sazonal)

  for(i in 1:numero_colunas_sazonal){

    beta_sazonal_hat[i] <- beta_hat[numero_colunas_tendencia + i]
  }

  # componentes estimados
  tendencia_hat <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)

  sazonalidade_hat <- multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)

  # resíduos
  n <- length(y)

  residuos <- numeric(n)

  for(i in 1:n){

    residuos[i] <- y[i] - y_hat[i]
  }

  return(
    list(
      beta_hat = beta_hat,
      y_hat = y_hat,
      tendencia = tendencia_hat,
      sazonalidade = sazonalidade_hat,
      residuos = residuos,
      X_full = X_full,
      grau_tendencia = grau_tendencia,
      periodo = periodo,
      K = K,
      metodo_sazonal = metodo_sazonal
    )
  )
}

# diagnóstico dos resíduos e = y - y_hat: média, variância (divisor n - 1),
# rmse = sqrt(soma e^2 / n) e r2 = 1 - soma e^2 / soma (y - média de y)^2
# entrada: y (vetor de tamanho n), residuos (vetor de tamanho n)
# saída: lista com media, variancia, rmse e r2
# pré-condição: n >= 2 e y não constante
diagnostico <- function(y, residuos){

  n <- length(y)

  # soma dos quadrados dos resíduos
  ss_res <- 0

  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }

  # média de y
  media_y <- calcular_media_amostral(y)

  # soma total dos quadrados
  ss_tot <- 0

  for(i in 1:n){

    d <- y[i] - media_y

    ss_tot <- ss_tot + d^2
  }

  # média dos resíduos
  media_residuos <- calcular_media_amostral(residuos)

  # variância dos resíduos
  soma_quadrados <- 0

  for(i in 1:n){

    d <- residuos[i] - media_residuos

    soma_quadrados <- soma_quadrados + d^2
  }

  variancia_residuos <- soma_quadrados / (n - 1)

  # RMSE
  rmse <- sqrt(ss_res / n)

  # R²
  r2 <- 1 - ss_res / ss_tot

  return(list(media = media_residuos, variancia = variancia_residuos, rmse = rmse, r2 = r2))
}

# previsão fora da amostra: X_futura beta_hat, com as mesmas colunas do ajuste
# entrada: modelo (lista devolvida por ajustar_modelo), t_futuro (vetor de inteiros depois do fim da amostra)
# saída: vetor de previsões de tamanho length(t_futuro)
prever <- function(modelo, t_futuro){

  t_futuro <- as.numeric(t_futuro)

  grau_tendencia <- modelo$grau_tendencia

  periodo <- modelo$periodo

  K <- modelo$K

  metodo_sazonal <- modelo$metodo_sazonal

  # matriz de tendência futura
  X_tendencia_fut <- criar_matriz_tendencia(t_futuro, grau_tendencia)

  # matriz sazonal futura
  if(metodo_sazonal == "fourier"){

    X_sazonal_fut <- criar_matriz_fourier(t_futuro, periodo, K)

  } else {

    X_sazonal_fut <- criar_matriz_dummies(t_futuro, periodo)
  }

  # matriz futura completa
  X_fut <- cbind(X_tendencia_fut, X_sazonal_fut)

  # previsões
  previsoes <- multiplicar_matriz_vetor(X_fut, modelo$beta_hat)

  return(previsoes)
}

# teste t dos coeficientes: t_j = beta_hat_j / ep_j, com ep_j^2 = s2 [(X'X)^-1]_jj e s2 = soma e^2 / (n - p);
# valor-p = 2 (1 - P(T <= |t_j|)), T com n - p graus de liberdade; H0: beta_j = 0
# entrada: modelo (lista devolvida por ajustar_modelo)
# saída: data.frame com beta_hat, erro_padrao, estatistica_t e valor_p, uma linha por coeficiente
# pré-condição: X'X invertível e n > p
teste_t_coeficientes <- function(modelo){

  X <- modelo$X_full

  residuos <- modelo$residuos

  n <- nrow(X)

  p <- ncol(X)

  # soma dos quadrados dos resíduos
  ss_res <- 0

  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }

  s2 <- ss_res / (n - p)

  XtX_inversa <- solve(calcular_XtX(X))

  erro_padrao <- numeric(p)

  estatistica_t <- numeric(p)

  valor_p <- numeric(p)

  for(j in 1:p){

    erro_padrao[j] <- sqrt(XtX_inversa[j,j] * s2)

    estatistica_t[j] <- modelo$beta_hat[j] / erro_padrao[j]

    valor_p[j] <- 2 * (1 - pt(abs(estatistica_t[j]), n - p))
  }

  return(data.frame(beta_hat = modelo$beta_hat,
                    erro_padrao = erro_padrao,
                    estatistica_t = estatistica_t,
                    valor_p = valor_p))
}

# teste de Box-Pierce: Q = n soma_{h=1}^{max_lag} rho_hat(h)^2, com rho_hat(h) = gama_hat(h) / gama_hat(0);
# valor-p = 1 - P(chi2 com max_lag graus de liberdade <= Q); H0: resíduos sem autocorrelação
# entrada: residuos (vetor numérico de tamanho n), max_lag (inteiro com 1 <= max_lag < n)
# saída: lista com Q e valor_p
teste_box_pierce <- function(residuos, max_lag){

  n <- length(residuos)

  media <- calcular_media_amostral(residuos)

  # autocovariância de lag 0
  gama0 <- 0

  for(i in 1:n){
    gama0 <- gama0 + (residuos[i] - media)^2
  }

  gama0 <- gama0 / n

  Q <- 0

  for(h in 1:max_lag){

    gama_h <- 0

    for(i in 1:(n - h)){
      gama_h <- gama_h + (residuos[i] - media) * (residuos[i + h] - media)
    }

    gama_h <- gama_h / n

    rho_h <- gama_h / gama0

    Q <- Q + rho_h^2
  }

  Q <- n * Q

  valor_p <- 1 - pchisq(Q, max_lag)

  return(list(Q = Q, valor_p = valor_p))
}
