############################################################
# config.R
# Lê os parâmetros de parametros.json e fixa a semente da simulação.
# Dependências: pacote jsonlite e arquivo parametros.json na pasta da lista.
############################################################

# Lê parametros.json e retorna os parâmetros da simulação e dos gráficos.
# Entrada: arquivo (texto com o caminho do arquivo JSON).
# Saída: Lista com seed, n, max_lag, sigma2, phi e opções dos gráficos.
# Pré-condição: arquivo existente e pacote jsonlite carregado.
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# Fixa a semente do gerador de números aleatórios para reproduzir as simulações.
# Entrada: parametros (lista com seed, inteiro).
# Saída: Nenhuma, apenas fixa a semente.
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}
