############################################################
# main.R
# Coordena a configuração, simulação, média móvel, FAC e gráficos.
# Dependências: scripts/config.R, scripts/dados.R, scripts/processamento.R e scripts/visualizacao.R.
# Executar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)
library(patchwork)

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
sigma <- parametros$sigma
q <- parametros$q
max_lag <- parametros$max_lag

t <- criar_tempo(parametros)
beta <- parametros$beta
X <- criar_matriz_regressao(parametros, t)

imprimir_titulo_console("PARÂMETROS")

cat("Semente:", parametros$seed, "\n")
cat("Tamanho da série (n):", n, "\n")
cat("Desvio padrão do ruído (sigma):", sigma, "\n")
cat("Raio da média móvel (q):", q, "\n")
cat("Maior defasagem (max_lag):", max_lag, "\n")
cat("Coeficientes (beta):", beta, "\n")

############################################################
# SIMULAÇÃO DA SÉRIE
############################################################

Xt <- simular_serie_com_tendencia(beta, X, sigma)

tendencia <- calcular_tendencia(beta, X)

############################################################
# TRANSFORMAÇÃO (MÉDIA MÓVEL)
############################################################

m_q <- calcular_media_movel(Xt, q)

############################################################
# ANÁLISE ESTATÍSTICA (FAC)
############################################################

imprimir_titulo_console("MÉDIA MÓVEL E FAC")

media_Xt <- calcular_media_amostral(Xt)

cat("Média amostral de Xt:", round(media_Xt, 4), "\n")

gamma_1 <- calcular_autocovariancia_empirica(Xt, h = 1)

cat("Autocovariância empírica de defasagem 1:", round(gamma_1, 4), "\n")

cat("Número de médias móveis:", length(m_q), "(n - 2q)\n")

fac_Xt <- calcular_fac_empirica(Xt, max_lag)

cat("FAE (autocorrelação empírica) para lags 0 a", max_lag, ":\n")

print(round(fac_Xt, 4))

# Compara a FAC calculada com a acf() do R.
fac_R <- as.numeric(acf(Xt, lag.max = max_lag, plot = FALSE)$acf)

cat("Maior diferença absoluta entre a FAC manual e a acf():",
    round(max(abs(fac_Xt - fac_R)), 4), "\n")
cat("FAC manual igual à acf():", isTRUE(all.equal(fac_Xt, fac_R)), "\n")

############################################################
# VISUALIZAÇÃO E SAÍDA
############################################################

grafico_serie <- plot_serie_temporal(
  t,
  Xt,
  "Série Temporal com Tendência Polinomial",
  expression(X[t]),
  "Série Xₜ"
)
print(grafico_serie)
salvar_grafico(grafico_serie,
               "serie_tendencia/01_serie_temporal_tendencia",
               parametros)

grafico_media_movel <- plot_serie_media_movel(
  t,
  Xt,
  m_q,
  tendencia,
  q,
  "Série Temporal com Média Móvel e Tendência",
  expression(X[t])
)
print(grafico_media_movel)
salvar_grafico(grafico_media_movel,
               "serie_tendencia/02_serie_temporal_media_movel_tendencia",
               parametros)

grafico_fac <- plot_fac_empirica(
  fac_Xt,
  n,
  "Função de Autocorrelação Empírica da Série com Tendência"
)
print(grafico_fac)
salvar_grafico(grafico_fac,
               "serie_tendencia/03_funcao_autocorrelacao_empirica_tendencia",
               parametros)

grafico <- plot_comparacao_acf_r(
  fac_Xt,
  fac_R,
  "Comparação da FAC Empírica com a Função acf() do R"
)
print(grafico)
salvar_grafico(grafico,
               "serie_tendencia/04_comparacao_fac_empirica_acf_r_tendencia",
               parametros)

grafico <- plot_grafico_integrado(grafico_media_movel, grafico_fac)
print(grafico)
salvar_grafico(grafico,
               "serie_tendencia/05_grafico_integrado_tendencia",
               parametros,
               1.6)
