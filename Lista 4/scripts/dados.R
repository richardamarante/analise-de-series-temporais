############################################################
# dados.R
# Constrói as matrizes de tendência, Fourier e dummies, além de simular o ruído e a série.
# Dependências: nenhuma, pois recebe os parâmetros por argumento.
############################################################

# Cria a matriz de tendência com X[i,j+1] = t[i]^j, para j = 0, ..., grau.
# Entrada: t é um vetor de tamanho n e grau é um inteiro >= 0.
# Saída: matriz n x (grau + 1), com a primeira coluna igual a 1.
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

# Cria a matriz de Fourier com pares de cos(2 pi k t / periodo) e sen(2 pi k t / periodo),
# colocando o cosseno antes do seno em cada harmônico.
# Entrada: t é um vetor de tamanho n, periodo é um inteiro >= 2 e K é um inteiro.
# Saída: matriz n x 2K.
# Pré-condição: K < periodo / 2, para evitar uma coluna de seno nula.
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

# Cria as dummies sazonais, em que cada coluna indica um mês e o mês 1 fica no intercepto.
# Entrada: t é um vetor de inteiros e periodo é um inteiro >= 2.
# Saída: matriz n x (periodo - 1), com uma coluna para cada mês de 2 até periodo.
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

# Simula n observações independentes de ruído branco com eps_t ~ N(0, sigma²).
# Entrada: n é um inteiro positivo e sigma é um número > 0.
# Saída: vetor eps de tamanho n.
simular_ruido_branco <- function(n, sigma){
  
  eps <- numeric(n)
  
  for(i in 1:n){
    
    eps[i] <- rnorm(1, mean = 0, sd = sigma)
  }
  
  return(eps)
}

# Simula Xt = X_tendencia beta_tendencia + X_sazonal beta_sazonal + eps.
# Entrada: beta_tendencia e beta_sazonal contêm os coeficientes, X_tendencia
# e X_sazonal são as matrizes correspondentes e sigma é o desvio padrão do ruído.
# Saída: vetor Xt de tamanho n.
# Pré-condição: as matrizes devem ter n linhas e cada uma deve ter tantas colunas
# quanto o tamanho do respectivo vetor de coeficientes.
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
