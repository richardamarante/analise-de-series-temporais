############################################################
# processamento.R
# OLS pelas equações normais, média móvel, diagnóstico dos
# resíduos, testes t e de Box-Pierce, sazonalidade por mês e
# previsão (sem gráficos). As matrizes de regressores chegam
# prontas, montadas no main.R com as funções de dados.R.
# Dependências: nenhuma.
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

# Ajusta tendência mais sazonalidade por OLS com as matrizes já montadas e separa os componentes estimados
# Entrada: y (vetor de tamanho n), X_tendencia (matriz n x p1), X_sazonal (matriz n x p2, de Fourier ou de dummies)
# Saída: lista com beta_hat, y_hat, tendencia, sazonalidade, residuos e X_full
# Pré-condição: as duas matrizes têm n linhas e n é maior que p1 + p2
ajustar_modelo <- function(y, X_tendencia, X_sazonal){

  y <- as.numeric(y)

  # Matriz completa: colunas da tendência e depois as da sazonalidade
  X_full <- cbind(X_tendencia, X_sazonal)

  # Estimação dos coeficientes
  beta_hat <- ajustar_ols(X_full, y)

  # Valores ajustados
  y_hat <- multiplicar_matriz_vetor(X_full, beta_hat)

  # Coeficientes da tendência
  numero_colunas_tendencia <- ncol(X_tendencia)

  beta_tendencia_hat <- numeric(numero_colunas_tendencia)

  for(i in 1:numero_colunas_tendencia){

    beta_tendencia_hat[i] <- beta_hat[i]

  }

  # Coeficientes da sazonalidade
  numero_colunas_sazonal <- ncol(X_sazonal)

  beta_sazonal_hat <- numeric(numero_colunas_sazonal)

  for(i in 1:numero_colunas_sazonal){

    beta_sazonal_hat[i] <- beta_hat[numero_colunas_tendencia + i]
  }

  # Componentes estimados
  tendencia_hat <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia_hat)

  sazonalidade_hat <- multiplicar_matriz_vetor(X_sazonal, beta_sazonal_hat)

  # Resíduos
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

# Previsão fora da amostra: X_futura beta_hat, com X_futura = [X_tendencia_futuro | X_sazonal_futuro]
# Entrada: modelo (lista devolvida por ajustar_modelo), X_tendencia_futuro (matriz h x p1), X_sazonal_futuro (matriz h x p2)
# Saída: vetor de previsões de tamanho h
# Pré-condição: as matrizes futuras foram montadas nos instantes depois do fim da amostra, com as mesmas colunas do ajuste
prever <- function(modelo, X_tendencia_futuro, X_sazonal_futuro){

  # Matriz futura completa, com as colunas na ordem do ajuste
  X_futuro <- cbind(X_tendencia_futuro, X_sazonal_futuro)

  if(ncol(X_futuro) != length(modelo$beta_hat)){

    stop("As matrizes futuras não têm o número de colunas do ajuste")
  }

  # Previsões
  previsoes <- multiplicar_matriz_vetor(X_futuro, modelo$beta_hat)

  return(previsoes)
}

# Sazonalidade de cada mês: média do componente nos instantes do mês m, menos a média dos periodo valores
# Entrada: t (vetor de inteiros de tamanho n), sazonalidade (vetor de tamanho n), periodo (inteiro >= 2)
# Saída: vetor de tamanho periodo, com média zero; o elemento m é o efeito do mês m = ((t - 1) %% periodo) + 1
# Pré-condição: cada mês aparece pelo menos uma vez em t
calcular_sazonalidade_mensal <- function(t, sazonalidade, periodo){

  n <- length(t)

  soma <- numeric(periodo)

  contagem <- numeric(periodo)

  # Soma e contagem dos instantes de cada mês, com a mesma regra de mês da matriz de dummies
  for(i in 1:n){

    mes <- ((t[i] - 1) %% periodo) + 1

    soma[mes] <- soma[mes] + sazonalidade[i]

    contagem[mes] <- contagem[mes] + 1
  }

  media_mes <- numeric(periodo)

  for(m in 1:periodo){

    media_mes[m] <- soma[m] / contagem[m]
  }

  # Centraliza: nas dummies o mês 1 é a referência e o nível dele fica no intercepto
  media_geral <- calcular_media_amostral(media_mes)

  sazonalidade_mensal <- numeric(periodo)

  for(m in 1:periodo){

    sazonalidade_mensal[m] <- media_mes[m] - media_geral
  }

  return(sazonalidade_mensal)
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
