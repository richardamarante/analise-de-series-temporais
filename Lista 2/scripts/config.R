############################################################
# config.R
# Lê os parâmetros de parametros.json e fixa a semente para reproduzir as simulações.
# Dependências: pacote jsonlite e arquivo parametros.json na pasta da lista.
############################################################

# Lê o arquivo JSON e retorna os parâmetros usados nas simulações e nos gráficos.
# Entrada: arquivo é o caminho do arquivo JSON.
# Saída: Lista com seed, n, max_lag, sigma2, phi e opções dos gráficos.
# Pré-condição: arquivo existente e pacote jsonlite carregado.
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# Fixa a semente do gerador de números aleatórios para reproduzir as simulações.
# Entrada: parametros é uma lista que contém seed como número inteiro.
# Saída: nenhuma, apenas fixa a semente.
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}
