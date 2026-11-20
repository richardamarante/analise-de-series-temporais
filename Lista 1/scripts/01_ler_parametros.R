############################################################
# 01_ler_parametros.R
# Lê os parâmetros da simulação (sigma2, n, m, seed e opções
# dos gráficos) de scripts/parametros.json.
# Dependências: pacote jsonlite.
############################################################

# lê os parâmetros da simulação a partir de um arquivo json
# entrada: arquivo (caminho do json, ex.: "scripts/parametros.json")
# saída: lista com os campos do json (seed, sigma2, n, m, ...)
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}
