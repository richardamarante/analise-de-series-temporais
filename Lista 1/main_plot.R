############################################################
# Arquivo: main_plot.R
# Lê os parâmetros e executa as simulações de ruído branco, passeio aleatório e conjuntos cilíndricos.
# Rodar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

# Carrega os scripts na ordem das dependências.
source("scripts/ler_parametros.R")
source("scripts/sim_ruido_branco.R")
source("scripts/sim_passeio_aleatorio.R")
source("scripts/estatisticas.R")
source("scripts/sim_cilindro.R")
source("scripts/plots.R")
source("scripts/executar_simulacoes.R")

parametros <- ler_parametros("parametros.json")

executar_simulacoes(parametros)
