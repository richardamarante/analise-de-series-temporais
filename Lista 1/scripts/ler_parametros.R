############################################################
# Arquivo: ler_parametros.R
# Lê os parâmetros das simulações e dos gráficos em parametros.json.
# Dependências: pacote jsonlite.
############################################################

# Lê os parâmetros de um arquivo JSON.
# Entrada: arquivo, caminho do JSON, como "parametros.json".
# Saída: lista com os parâmetros do arquivo.
ler_parametros <- function(arquivo){
  parametros <- fromJSON(arquivo)
  return(parametros)
}
