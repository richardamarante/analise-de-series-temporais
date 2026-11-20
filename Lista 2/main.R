############################################################
# main.R
# Coordena a simulação dos processos, os cálculos estatísticos, as comparações e os gráficos,
# além de apresentar os resultados no console.
# Dependências: scripts/config.R, scripts/dados.R, scripts/processamento.R e scripts/visualizacao.R.
# Execução: usar a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/config.R")
source("scripts/dados.R")
source("scripts/processamento.R")
source("scripts/visualizacao.R")

############################################################
# CONFIGURAÇÃO
############################################################

parametros <- ler_parametros("parametros.json")

fixar_semente(parametros)

n <- parametros$n
max_lag <- parametros$max_lag
sigma2 <- parametros$sigma2
phi <- parametros$phi

############################################################
# SIMULAÇÃO DE SÉRIES
############################################################

eps <- simular_ruido_branco(n, sigma2)
X <- simular_passeio_aleatorio(n, sigma2)
X_ar1 <- simular_ar1(n, phi, sigma2)

############################################################
# CÁLCULO ESTATÍSTICO
############################################################

media_eps <- calcular_media_empirica(eps)
media_X <- calcular_media_empirica(X)
media_ar1 <- calcular_media_empirica(X_ar1)

fac_eps <- calcular_fac_empirica(eps, max_lag)
fac_X <- calcular_fac_empirica(X, max_lag)
fac_ar1 <- calcular_fac_empirica(X_ar1, max_lag)

fac_ar1_teorica <- calcular_fac_teorica_ar1(phi, max_lag)

############################################################
# ANÁLISE E COMPARAÇÃO
############################################################

# Na comparação com acf(), o lag h ocupa a posição h+1 nos dois vetores porque o lag zero
# também é armazenado.
acf_eps <- as.numeric(acf(eps, lag.max = max_lag, plot = FALSE)$acf)
acf_X <- as.numeric(acf(X, lag.max = max_lag, plot = FALSE)$acf)
acf_ar1 <- as.numeric(acf(X_ar1, lag.max = max_lag, plot = FALSE)$acf)

diferenca_ar1 <- abs(fac_ar1 - fac_ar1_teorica)
erro_fac_ar1 <- max(diferenca_ar1)

############################################################
# EXIBIÇÃO DE RESULTADOS
############################################################

imprimir_titulo_console("PARÂMETROS")

cat("n:", n, "\n")
cat("max_lag:", max_lag, "\n")
cat("sigma2:", sigma2, "\n")
cat("phi:", phi, "\n")
cat("semente:", parametros$seed, "\n")

imprimir_titulo_console("RUÍDO BRANCO")

cat("Média empírica (ruído branco):", round(media_eps, 4), "\n")

grafico <- plot_serie_temporal(
  eps,
  "Série Temporal do Ruído Branco",
  expression(epsilon[t]),
  "εₜ"
)
print(grafico)
salvar_grafico(grafico,
               "ruido_branco/01_serie_temporal_ruido_branco",
               parametros)

grafico <- plot_fac_empirica(
  fac_eps,
  n,
  "Função de Autocorrelação Empírica do Ruído Branco"
)
print(grafico)
salvar_grafico(grafico,
               "ruido_branco/02_funcao_autocorrelacao_empirica_ruido_branco",
               parametros)

grafico <- plot_comparacao_acf_r(
  fac_eps,
  acf_eps,
  "FAC Empírica e acf() do R do Ruído Branco"
)
print(grafico)
salvar_grafico(grafico,
               "ruido_branco/03_comparacao_fac_empirica_acf_r_ruido_branco",
               parametros)

cat("Maior diferença para o acf() do R:",
    round(max(abs(fac_eps - acf_eps)), 4), "\n")

imprimir_titulo_console("PASSEIO ALEATÓRIO")

cat("Média empírica (passeio aleatório):", round(media_X, 4), "\n")

grafico <- plot_serie_temporal(
  X,
  "Série Temporal do Passeio Aleatório",
  expression(X[t]),
  "Xₜ"
)
print(grafico)
salvar_grafico(grafico,
               "passeio_aleatorio/01_serie_temporal_passeio_aleatorio",
               parametros)

grafico <- plot_fac_empirica(
  fac_X,
  n,
  "Função de Autocorrelação Empírica do Passeio Aleatório"
)
print(grafico)
salvar_grafico(grafico,
               "passeio_aleatorio/02_funcao_autocorrelacao_empirica_passeio_aleatorio",
               parametros)

grafico <- plot_comparacao_acf_r(
  fac_X,
  acf_X,
  "FAC Empírica e acf() do R do Passeio Aleatório"
)
print(grafico)
salvar_grafico(grafico,
               "passeio_aleatorio/03_comparacao_fac_empirica_acf_r_passeio_aleatorio",
               parametros)

cat("Maior diferença para o acf() do R:",
    round(max(abs(fac_X - acf_X)), 4), "\n")

imprimir_titulo_console("PROCESSO AR(1)")

cat("Média empírica (AR(1)):", round(media_ar1, 4), "\n")

grafico <- plot_serie_temporal(
  X_ar1,
  paste0("Série Temporal do Processo AR(1) com φ = ", phi),
  expression(X[t]),
  "Xₜ"
)
print(grafico)
salvar_grafico(grafico,
               "ar1/01_serie_temporal_ar1",
               parametros)

grafico <- plot_fac_empirica(
  fac_ar1,
  n,
  "Função de Autocorrelação Empírica do Processo AR(1)"
)
print(grafico)
salvar_grafico(grafico,
               "ar1/02_funcao_autocorrelacao_empirica_ar1",
               parametros)

grafico <- plot_fac_comparacao(
  fac_ar1,
  fac_ar1_teorica,
  "FAC Empírica e FAC Teórica do Processo AR(1)",
  n
)
print(grafico)
salvar_grafico(grafico,
               "ar1/03_comparacao_fac_empirica_teorica_ar1",
               parametros)

grafico <- plot_comparacao_acf_r(
  fac_ar1,
  acf_ar1,
  "FAC Empírica e acf() do R do Processo AR(1)"
)
print(grafico)
salvar_grafico(grafico,
               "ar1/04_comparacao_fac_empirica_acf_r_ar1",
               parametros)

cat("Maior diferença para o acf() do R:",
    round(max(abs(fac_ar1 - acf_ar1)), 4), "\n")

imprimir_titulo_console("FAC DO AR(1): EMPÍRICA, TEÓRICA E DIFERENÇA")

for(h in 0:max_lag){
  cat("lag", h,
      "| empírica:", round(fac_ar1[h+1], 4),
      "| teórica:", round(fac_ar1_teorica[h+1], 4),
      "| diferença:", round(diferenca_ar1[h+1], 4), "\n")
}

cat("Maior diferença entre FAC empírica e teórica:",
    round(erro_fac_ar1, 4), "\n")

imprimir_titulo_console("RESUMO DAS MÉDIAS EMPÍRICAS")

cat("Ruído branco:", round(media_eps, 4), "\n")
cat("Passeio aleatório:", round(media_X, 4), "\n")
cat("AR(1):", round(media_ar1, 4), "\n")
