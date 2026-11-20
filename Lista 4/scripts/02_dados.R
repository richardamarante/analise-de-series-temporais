############################################################
# 02_dados.R
# Matrizes de tendência, Fourier e dummies, ruído branco e
# simulação da série.
# Dependências: nenhuma.
############################################################

# matriz de tendência polinomial: X[i, j+1] = t_i^j, com j = 0, ..., grau
# entrada: t (vetor numérico de tamanho n), grau (inteiro >= 0)
# saída: matriz n x (grau + 1), a primeira coluna é de 1
criar_matriz_tendencia <- function(t, grau){
  
  n <- length(t)
  
  numero_colunas <- grau + 1
  
  X <- matrix(0, nrow = n, ncol = numero_colunas)
  
  for(i in 1:n){
    
    for(j in 0:grau){
      X[i,j+1] <- t[i]^j
    }
  }
  return(X)
}

# matriz de Fourier: cos(2 pi k t / s) e sen(2 pi k t / s), com k = 1, ..., K
# entrada: t (vetor de tamanho n), periodo (inteiro >= 2), K (inteiro)
# saída: matriz n x 2K, com as colunas cos e sen de cada harmônico em sequência
# pré-condição: K < periodo / 2 (com K = periodo / 2 a coluna do seno é zero)
criar_matriz_fourier <- function(t, periodo, K){
  
  n <- length(t)
  
  numero_colunas <- 2 * K
  
  X <- matrix(0, nrow = n, ncol = numero_colunas)
  
  for(i in 1:n){
    
    for(k in 1:K){
      
      coluna_cos <- 2*k - 1
      coluna_sin <- 2*k
      
      X[i,coluna_cos] <- cos(2 * pi * k * t[i] / periodo)
      
      X[i,coluna_sin] <- sin(2 * pi * k * t[i] / periodo)
    }
  }
  return(X)
}

# matriz de dummies sazonais: coluna m - 1 vale 1 quando o mês de t é m, com m = 2, ..., s
# entrada: t (vetor de inteiros de tamanho n), periodo (inteiro >= 2)
# saída: matriz n x (periodo - 1); o mês 1 é a referência e fica no intercepto
criar_matriz_dummies <- function(t, periodo){
  
  n <- length(t)
  
  numero_colunas <- periodo - 1
  
  X <- matrix(0, nrow = n, ncol = numero_colunas)
  
  for(i in 1:n){
    
    mes <- ((t[i] - 1) %% periodo) + 1
    
    if(mes != 1){
      
      coluna <- mes - 1
      
      X[i,coluna] <- 1
    }
  }
  
  return(X)
}

# ruído branco gaussiano: eps_t ~ N(0, sigma^2), independentes
# entrada: n (inteiro positivo), sigma (desvio padrão, numérico > 0)
# saída: vetor eps de tamanho n
simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(i in 1:n){
    
    # eps_t ~ N(0, sigma²)
    eps[i] <- rnorm(1, mean = 0, sd = sigma)
  }
  
  return(eps)
}

# série simulada: X_t = X_tendencia beta_tendencia + X_sazonal beta_sazonal + eps_t
# entrada: beta_tendencia (vetor de tamanho p1), X_tendencia (matriz n x p1), X_sazonal (matriz n x p2),
#          beta_sazonal (vetor de tamanho p2), sigma (desvio padrão do ruído)
# saída: vetor Xt de tamanho n
# pré-condição: as matrizes têm o mesmo número de linhas e as colunas casam com os betas
simular_serie <- function(beta_tendencia,
                          X_tendencia,
                          X_sazonal,
                          beta_sazonal,
                          sigma){
  
  n <- nrow(X_tendencia)
  
  numero_colunas_tendencia <- ncol(X_tendencia)
  
  numero_colunas_sazonal <- ncol(X_sazonal)
  
  eps <- simular_ruido_branco(n, sigma)
  
  Xt <- numeric(n)
  
  for(i in 1:n){
    
    valor_tendencia <- 0
    
    for(j in 1:numero_colunas_tendencia){
      
      valor_tendencia <- valor_tendencia + X_tendencia[i,j] * beta_tendencia[j]
    }
    
    valor_sazonal <- 0
    
    for(j in 1:numero_colunas_sazonal){
      
      valor_sazonal <- valor_sazonal + X_sazonal[i,j] * beta_sazonal[j]
    }
    
    Xt[i] <- valor_tendencia + valor_sazonal + eps[i]
  }
  return(Xt)
}
