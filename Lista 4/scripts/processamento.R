############################################################
# processamento.R
# Calcula média móvel, OLS, diagnóstico, testes, sazonalidade mensal e previsão.
# Dependências: nenhuma, pois recebe as matrizes de regressores prontas.
############################################################

# Calcula a média móvel centrada somando os valores de t - q até t + q
# e dividindo por 2q + 1, sem calcular médias nas pontas incompletas.
# Entrada: serie é um vetor de tamanho n e q é um inteiro.
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

# Calcula a média amostral somando os valores de serie e dividindo por n.
# Entrada: serie é um vetor numérico.
# Saída: um número.
# Pré-condição: n >= 1 e serie não pode conter NA.
calcular_media_amostral <- function(serie){

  n <- length(serie)

  soma <- 0

  for(i in 1:n){
    soma <- soma + serie[i]
  }

  media <- soma / n

  return(media)
}

# Multiplica X por beta, somando X[i,j] * beta[j] para cada linha i.
# Entrada: X é uma matriz n x p e beta é um vetor de tamanho p.
# Saída: vetor de tamanho n.
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

# Calcula X'X com três loops, somando o produto interno de cada par de colunas de X.
# Entrada: X é uma matriz n x p.
# Saída: matriz simétrica p x p.
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

# Calcula X'y com dois loops, somando o produto de cada coluna de X pelos valores de y.
# Entrada: X é uma matriz n x p e y é um vetor de tamanho n.
# Saída: vetor de tamanho p.
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

# Ajusta OLS pelas equações normais, resolvendo beta_hat = solve(X'X, X'y).
# Entrada: X é uma matriz n x p e y é um vetor de tamanho n.
# Saída: vetor beta_hat de tamanho p.
# Pré-condição: X'X deve ser invertível, com colunas de X linearmente independentes.
ajustar_ols <- function(X, y){

  XtX <- calcular_XtX(X)

  Xty <- calcular_Xty(X, y)

  beta_hat <- solve(XtX, Xty)

  beta_hat <- as.numeric(beta_hat)

  return(beta_hat)
}

# Ajusta tendência e sazonalidade por OLS e separa os componentes estimados.
# Entrada: y é um vetor de tamanho n, X_tendencia é uma matriz n x p1
# e X_sazonal é uma matriz n x p2.
# Saída: Lista com beta_hat, y_hat, tendencia, sazonalidade, residuos e X_full.
# Pré-condição: as matrizes devem ter n linhas, com n > p1 + p2 e X'X invertível.
ajustar_modelo <- function(y, X_tendencia, X_sazonal){

  y <- as.numeric(y)

  X_full <- cbind(X_tendencia, X_sazonal)

  beta_hat <- ajustar_ols(X_full, y)

  y_hat <- multiplicar_matriz_vetor(X_full, beta_hat)

  numero_colunas_tendencia <- ncol(X_tendencia)

  beta_tendencia_hat <- numeric(numero_colunas_tendencia)

  for(i in 1:numero_colunas_tendencia){

    beta_tendencia_hat[i] <- beta_hat[i]

  }

  numero_colunas_sazonal <- ncol(X_sazonal)

  beta_sazonal_hat <- numeric(numero_colunas_sazonal)

  for(i in 1:numero_colunas_sazonal){

    beta_sazonal_hat[i] <- beta_hat[numero_colunas_tendencia + i]
  }

  tendencia_hat <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)

  sazonalidade_hat <- multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)

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

# Calcula média e variância amostral dos resíduos, além do RMSE e do R² do ajuste.
# Entrada: y e residuos são vetores de tamanho n.
# Saída: Lista com media, variancia, rmse e r2.
# Pré-condição: n >= 2 e y não pode ser constante.
diagnostico <- function(y, residuos){

  n <- length(y)

  ss_res <- 0

  for(i in 1:n){
    ss_res <- ss_res + residuos[i]^2
  }

  media_y <- calcular_media_amostral(y)

  ss_tot <- 0

  for(i in 1:n){

    d <- y[i] - media_y

    ss_tot <- ss_tot + d^2
  }

  media_residuos <- calcular_media_amostral(residuos)

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

# Faz previsões fora da amostra com previsoes = X_futuro beta_hat.
# Entrada: modelo contém os coeficientes estimados, e X_tendencia_futuro
# e X_sazonal_futuro são as matrizes de regressores para os h instantes futuros.
# Saída: vetor de previsões de tamanho h.
# Pré-condição: as matrizes futuras devem manter as colunas e a ordem do ajuste.
prever <- function(modelo, X_tendencia_futuro, X_sazonal_futuro){

  X_futuro <- cbind(X_tendencia_futuro, X_sazonal_futuro)

  if(ncol(X_futuro) != length(modelo$beta_hat)){

    stop("As matrizes futuras não têm o número de colunas do ajuste")
  }

  previsoes <- multiplicar_matriz_vetor(X_futuro, modelo$beta_hat)

  return(previsoes)
}

# Calcula RMSE, MAE e R² das previsões, usando a média do treino como referência para o R².
# Entrada: y e y_hat são vetores de mesmo tamanho, e media_treino é a média
# dos dados usados no ajuste.
# Saída: Lista com rmse, mae e r2.
# Pré-condição: y e y_hat não podem conter NA.
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

  # R² = 0 corresponde ao erro de prever sempre a média do treino, enquanto um valor
  # negativo indica que as previsões tiveram erro quadrático maior que essa referência.
  r2 <- 1 - sse / sst

  return(list(rmse = rmse, mae = mae, r2 = r2))
}

# Calcula o efeito médio de cada mês e centraliza os valores para terem média zero.
# Entrada: t é um vetor de inteiros, sazonalidade tem tamanho n e periodo é um inteiro >= 2.
# Saída: vetor de tamanho periodo, com um efeito para cada mês.
# Pré-condição: cada mês deve aparecer pelo menos uma vez.
calcular_sazonalidade_mensal <- function(t, sazonalidade, periodo){

  n <- length(t)

  soma <- numeric(periodo)

  contagem <- numeric(periodo)

  # Usa ((t[i] - 1) %% periodo) + 1 para identificar o mês de cada observação,
  # somando os efeitos e contando quantos valores pertencem a cada mês.
  for(i in 1:n){

    mes <- ((t[i] - 1) %% periodo) + 1

    soma[mes] <- soma[mes] + sazonalidade[i]

    contagem[mes] <- contagem[mes] + 1
  }

  media_mes <- numeric(periodo)

  for(m in 1:periodo){

    media_mes[m] <- soma[m] / contagem[m]
  }

  # Centraliza os efeitos mensais porque, nas dummies, o mês 1 fica no intercepto,
  # e a média zero permite comparar os dois métodos na mesma escala.
  media_geral <- calcular_media_amostral(media_mes)

  sazonalidade_mensal <- numeric(periodo)

  for(m in 1:periodo){

    sazonalidade_mensal[m] <- media_mes[m] - media_geral
  }

  return(sazonalidade_mensal)
}

# Testa H0: beta_j = 0 com t_j = beta_hat_j / ep_j, em que
# ep_j² = s2 [(X'X)^-1]_jj e s2 = soma e² / (n - p), usando a distribuição t
# com n - p graus de liberdade para calcular os valores-p bilaterais.
# Entrada: modelo é a lista produzida por ajustar_modelo.
# Saída: data.frame com beta_hat, erro_padrao, estatistica_t e valor_p.
# Pré-condição: X'X deve ser invertível e n > p.
teste_t_coeficientes <- function(modelo){

  X <- modelo$X_full

  residuos <- modelo$residuos

  n <- nrow(X)

  p <- ncol(X)

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

# Aplica Box-Pierce com Q = n * soma de rho_hat(h)², de h = 1 até max_lag,
# usando rho_hat(h) = gama_hat(h) / gama_hat(0) e a distribuição qui-quadrado
# com max_lag graus de liberdade para calcular o valor-p.
# Entrada: residuos é um vetor de tamanho n e max_lag é um inteiro.
# Saída: Lista com Q e valor_p.
# Pré-condição: 1 <= max_lag < n.
teste_box_pierce <- function(residuos, max_lag){

  n <- length(residuos)

  media <- calcular_media_amostral(residuos)

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
