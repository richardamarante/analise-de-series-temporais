############################################################
# processamento.R
# Calcula média móvel, OLS, diagnóstico, testes t e de Box-Pierce, sazonalidade mensal e previsão.
# Dependências: nenhuma. Recebe as matrizes de regressores prontas.
############################################################

# Calcula a média móvel centrada: m_t = soma de X[t-q] até X[t+q], dividida por 2q + 1.
# Entrada: serie (vetor n), q (inteiro).
# Saída: vetor com n - 2q médias, começando em t = q + 1.
# Pré-condição: 2q + 1 <= n.
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

# Calcula a média amostral: soma de serie[i] dividida por n.
# Entrada: serie (vetor numérico).
# Saída: um número.
# Pré-condição: n >= 1, sem NA.
calcular_media_amostral <- function(serie){

  n <- length(serie)

  soma <- 0

  for(i in 1:n){
    soma <- soma + serie[i]
  }

  media <- soma / n

  return(media)
}

# Multiplica matriz por vetor: resultado[i] = soma_j X[i,j] * beta[j].
# Entrada: X (matriz n x p), beta (vetor p).
# Saída: vetor n.
# Pré-condição: length(beta) = ncol(X).
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

# Calcula X'X com três loops, somando o produto interno de cada par de colunas.
# Entrada: X (matriz n x p).
# Saída: matriz p x p, simétrica.
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

# Calcula X'y com dois loops, somando o produto interno de cada coluna com y.
# Entrada: X (matriz n x p), y (vetor n).
# Saída: vetor p.
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

# Ajusta OLS pelas equações normais: beta_hat = solve(X'X, X'y).
# Entrada: X (matriz n x p), y (vetor n).
# Saída: vetor beta_hat de tamanho p.
# Pré-condição: X'X invertível, com colunas de X linearmente independentes.
ajustar_ols <- function(X, y){

  XtX <- calcular_XtX(X)

  Xty <- calcular_Xty(X, y)

  beta_hat <- solve(XtX, Xty)

  beta_hat <- as.numeric(beta_hat)

  return(beta_hat)
}

# Ajusta tendência e sazonalidade por OLS e separa os componentes estimados.
# Entrada: y (vetor n), X_tendencia (matriz n x p1), X_sazonal (matriz n x p2).
# Saída: lista com beta_hat, y_hat, tendencia, sazonalidade, residuos e X_full.
# Pré-condição: matrizes com n linhas, n > p1 + p2 e X'X invertível.
ajustar_modelo <- function(y, X_tendencia, X_sazonal){

  y <- as.numeric(y)

  # Junta as colunas da tendência e da sazonalidade.
  X_full <- cbind(X_tendencia, X_sazonal)

  # Estima os coeficientes por OLS.
  beta_hat <- ajustar_ols(X_full, y)

  # Calcula os valores ajustados.
  y_hat <- multiplicar_matriz_vetor(X_full, beta_hat)

  # Separa os coeficientes da tendência.
  numero_colunas_tendencia <- ncol(X_tendencia)

  beta_tendencia_hat <- numeric(numero_colunas_tendencia)

  for(i in 1:numero_colunas_tendencia){

    beta_tendencia_hat[i] <- beta_hat[i]

  }

  # Separa os coeficientes da sazonalidade.
  numero_colunas_sazonal <- ncol(X_sazonal)

  beta_sazonal_hat <- numeric(numero_colunas_sazonal)

  for(i in 1:numero_colunas_sazonal){

    beta_sazonal_hat[i] <- beta_hat[numero_colunas_tendencia + i]
  }

  # Calcula os componentes estimados.
  tendencia_hat <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)

  sazonalidade_hat <- multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)

  # Resíduos: diferença entre y e y_hat.
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
      X_full = X_full
    )
  )
}

# Calcula média, variância amostral, RMSE e R² dos resíduos.
# Entrada: y e residuos (vetores n).
# Saída: lista com media, variancia, rmse e r2.
# Pré-condição: n >= 2 e y não constante.
diagnostico <- function(y, residuos){

  n <- length(y)

  # Soma dos quadrados dos resíduos.
  ss_res <- 0

  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }

  # Média de y.
  media_y <- calcular_media_amostral(y)

  # Soma total dos quadrados em relação à média de y.
  ss_tot <- 0

  for(i in 1:n){

    d <- y[i] - media_y

    ss_tot <- ss_tot + d^2
  }

  # Média dos resíduos.
  media_residuos <- calcular_media_amostral(residuos)

  # Soma dos quadrados dos desvios dos resíduos em relação à média.
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

# Prevê fora da amostra: previsoes = X_futuro beta_hat.
# Entrada: modelo, X_tendencia_futuro e X_sazonal_futuro (matrizes com h linhas).
# Saída: vetor de previsões de tamanho h.
# Pré-condição: matrizes futuras com as mesmas colunas e ordem do ajuste.
prever <- function(modelo, X_tendencia_futuro, X_sazonal_futuro){

  # Junta as matrizes futuras na ordem do ajuste.
  X_futuro <- cbind(X_tendencia_futuro, X_sazonal_futuro)

  if(ncol(X_futuro) != length(modelo$beta_hat)){

    stop("As matrizes futuras não têm o número de colunas do ajuste")
  }

  # Multiplica a matriz futura pelos coeficientes estimados.
  previsoes <- multiplicar_matriz_vetor(X_futuro, modelo$beta_hat)

  return(previsoes)
}

# Mede o erro de previsão: RMSE, MAE e R² contra a média do treino.
# Entrada: y e y_hat (vetores de mesmo tamanho), media_treino (média de y no conjunto de ajuste).
# Saída: lista com rmse, mae e r2.
# Pré-condição: y e y_hat sem NA.
avaliar_previsao <- function(y, y_hat, media_treino){

  n <- length(y)

  sse <- 0
  soma_absoluta <- 0
  sst <- 0

  for(i in 1:n){

    erro <- y[i] - y_hat[i]

    sse <- sse + erro^2

    soma_absoluta <- soma_absoluta + abs(erro)

    sst <- sst + (y[i] - media_treino)^2
  }

  rmse <- sqrt(sse / n)

  mae <- soma_absoluta / n

  # R² igual a 0 empata com prever a média do treino; negativo é pior que ela.
  r2 <- 1 - sse / sst

  return(list(rmse = rmse, mae = mae, r2 = r2))
}

# Calcula o efeito médio de cada mês e centraliza para média zero.
# Entrada: t (vetor de inteiros), sazonalidade (vetor n), periodo (inteiro >= 2).
# Saída: vetor de tamanho periodo, com um efeito para cada mês.
# Pré-condição: cada mês aparece pelo menos uma vez.
calcular_sazonalidade_mensal <- function(t, sazonalidade, periodo){

  n <- length(t)

  soma <- numeric(periodo)

  contagem <- numeric(periodo)

  # Soma os valores e conta as observações de cada mês, usando ((t - 1) %% periodo) + 1.
  for(i in 1:n){

    mes <- ((t[i] - 1) %% periodo) + 1

    soma[mes] <- soma[mes] + sazonalidade[i]

    contagem[mes] <- contagem[mes] + 1
  }

  media_mes <- numeric(periodo)

  for(m in 1:periodo){

    media_mes[m] <- soma[m] / contagem[m]
  }

  # Centraliza os efeitos mensais, pois o mês 1 das dummies fica no intercepto.
  media_geral <- calcular_media_amostral(media_mes)

  sazonalidade_mensal <- numeric(periodo)

  for(m in 1:periodo){

    sazonalidade_mensal[m] <- media_mes[m] - media_geral
  }

  return(sazonalidade_mensal)
}

# Testa H0: beta_j = 0, com t_j = beta_hat_j / ep_j.
# O erro padrão usa ep_j² = s2 [(X'X)^-1]_jj, com s2 = soma e² / (n - p).
# O valor-p é bilateral, calculado pela distribuição t com n - p graus de liberdade.
# Entrada: modelo (lista de ajustar_modelo).
# Saída: data.frame com beta_hat, erro_padrao, estatistica_t e valor_p.
# Pré-condição: X'X invertível e n > p.
teste_t_coeficientes <- function(modelo){

  X <- modelo$X_full

  residuos <- modelo$residuos

  n <- nrow(X)

  p <- ncol(X)

  # Soma dos quadrados dos resíduos.
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

# Testa H0: resíduos sem autocorrelação, com Q = n * soma de rho_hat(h)².
# A soma vai de h = 1 até max_lag, com rho_hat(h) = gama_hat(h) / gama_hat(0).
# O valor-p usa a distribuição qui-quadrado com max_lag graus de liberdade.
# Entrada: residuos (vetor n), max_lag (inteiro).
# Saída: lista com Q e valor_p.
# Pré-condição: 1 <= max_lag < n.
teste_box_pierce <- function(residuos, max_lag){

  n <- length(residuos)

  media <- calcular_media_amostral(residuos)

  # Autocovariância amostral de lag 0.
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
