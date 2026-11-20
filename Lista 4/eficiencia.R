############################################################
# eficiencia.R
# Exercício 4.3: compara os loops de X'X, X'y e X·beta com as
# operações matriciais do R e confere o OLS com o lm().
# Rodar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(jsonlite)

source("scripts/01_config.R")
source("scripts/02_dados.R")
source("scripts/03_processamento.R")
source("scripts/05_eficiencia.R")

parametros <- ler_parametros("scripts/parametros.json")

comparar_eficiencia(parametros)
