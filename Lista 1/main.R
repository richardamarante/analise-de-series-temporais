############################################################
# main.R
# Lê os parâmetros e roda as simulações da lista: ruído branco,
# passeio aleatório e conjuntos cilíndricos.
# Rodar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

# pacotes
library(ggplot2)
library(jsonlite)

# funções, na ordem em que uma depende da outra
source("scripts/01_ler_parametros.R")
source("scripts/02_sim_ruido_branco.R")
source("scripts/03_sim_passeio_aleatorio.R")
source("scripts/04_estatisticas.R")
source("scripts/05_sim_cilindro.R")
source("scripts/06_plots.R")
source("scripts/07_executar_simulacoes.R")

# lê os parâmetros e roda tudo
parametros <- ler_parametros("scripts/parametros.json")

executar_simulacoes(parametros)
