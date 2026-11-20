############################################################
# config.R
# Parâmetros (lidos de parametros.json, na pasta da lista),
# semente, vetor de tempo e coeficientes sazonais.
# Dependências: pacote jsonlite.
############################################################

# lê o arquivo de parâmetros em json
# entrada: arquivo (texto, caminho de um .json com os campos de parametros.json)
# saída: lista com os parâmetros (seed, n_anos, s, sigma, K, q, ...)
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# fixa a semente do gerador de números aleatórios
# entrada: parametros (lista com o campo seed, inteiro)
# saída: nenhuma; só fixa o gerador, para a simulação ser reproduzível
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}

# cria o vetor de tempo t = 1, ..., n, com n = n_anos * s
# entrada: parametros (lista com n_anos e s, inteiros positivos)
# saída: vetor t de tamanho n
criar_tempo <- function(parametros){
  n <- parametros$n_anos * parametros$s
  t <- 1:n
  return(t)
}

# intercala os coeficientes sazonais: (alpha_1, beta_1, ..., alpha_K, beta_K)
# entrada: alpha (vetor de tamanho K), beta_h (vetor de tamanho K), K (inteiro)
# saída: vetor beta_sazonal de tamanho 2K, na ordem das colunas de criar_matriz_fourier
# pré-condição: length(alpha) == length(beta_h) == K
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
