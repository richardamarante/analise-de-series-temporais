############################################################
# config.R
# Lê os parâmetros, fixa a semente e prepara o vetor de tempo e os coeficientes sazonais.
# Dependências: pacote jsonlite e arquivo parametros.json.
############################################################

# Lê o arquivo JSON e organiza os parâmetros usados nas etapas do programa.
# Entrada: arquivo é o caminho de um JSON com os campos de parametros.json.
# Saída: Lista com os parâmetros da simulação, do ajuste, da previsão e dos gráficos.
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# Fixa a semente para reproduzir os mesmos valores na simulação.
# Entrada: parametros é a lista que contém seed.
# Saída: nenhuma.
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}

# Cria o vetor t = 1, ..., n, com n = n_anos * s.
# Entrada: parametros contém n_anos e s.
# Saída: vetor t com n elementos.
criar_tempo <- function(parametros){
  n <- parametros$n_anos * parametros$s
  t <- 1:n
  return(t)
}

# Intercala alpha e beta_h na ordem cos1, sen1, cos2, sen2, ... usada na matriz de Fourier.
# Entrada: alpha e beta_h são vetores de tamanho K, que indica o número de harmônicos.
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
