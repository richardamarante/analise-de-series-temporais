############################################################
# 01_config.R
# Parâmetros (n, max_lag, sigma2, phi, semente e opções dos
# gráficos), lidos de scripts/parametros.json, e semente.
# Dependências: pacote jsonlite.
############################################################

# lê o arquivo de parâmetros e devolve todos os valores numa lista
# entrada: arquivo (texto com o caminho do .json)
# saída: lista com seed, n, max_lag, sigma2, phi e opções dos gráficos
# pré-condição: o arquivo existe e o pacote jsonlite está carregado
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}

# fixa a semente do gerador de números aleatórios, para a simulação ser reprodutível
# entrada: parametros (lista com o campo seed, inteiro)
# saída: nenhuma, só fixa o gerador
fixar_semente <- function(parametros){
  set.seed(parametros$seed)
}
