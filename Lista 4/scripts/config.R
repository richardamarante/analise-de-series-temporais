############################################################
# config.R
# Lê os parâmetros, fixa a semente e cria o tempo e os coeficientes sazonais.
# Dependências: pacote jsonlite e arquivo parametros.json.
############################################################

# Lê os parâmetros do arquivo JSON.
# Entrada: arquivo (caminho de um .json com os campos de parametros.json).
# Saída: lista com os parâmetros da simulação, ajuste, previsão e gráficos.
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# Fixa a semente para reproduzir a simulação.
# Entrada: parametros (lista com seed).
# Saída: nenhuma.
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}

# Cria o tempo t = 1, ..., n, com n = n_anos * s.
# Entrada: parametros (lista com n_anos e s).
# Saída: vetor t de tamanho n.
criar_tempo <- function(parametros){
  n <- parametros$n_anos * parametros$s
  t <- 1:n
  return(t)
}

# Intercala alpha e beta_h na ordem cos1, sen1, cos2, sen2, ...
# Entrada: alpha, beta_h (vetores K), K (inteiro).
# Saída: vetor beta_sazonal de tamanho 2K.
# Pré-condição: length(alpha) = length(beta_h) = K.
criar_beta_sazonal <- function(alpha, beta_h, K){
  
  beta_sazonal <- numeric(2 * K)
  
  for(k in 1:K){
    
    coluna_cos <- 2*k - 1
    coluna_sin <- 2*k
    
    beta_sazonal[coluna_cos] <- alpha[k]
    
    beta_sazonal[coluna_sin] <- beta_h[k]
  }
  
  return(beta_sazonal)
}
