############################################################
# main.R
# Pipeline: Configuração -> Simulação/Matrizes -> Estimação OLS
# -> Diagnóstico -> Previsão -> Visualização.
# Rodar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/01_config.R")
source("scripts/02_dados.R")
source("scripts/03_processamento.R")
source("scripts/04_visualizacao.R")

parametros <- ler_parametros("scripts/parametros.json")

############################################################
# CONFIGURAÇÃO
############################################################

fixar_semente(parametros)

s <- parametros$s
sigma <- parametros$sigma
K <- parametros$K
q <- parametros$q
grau_tendencia <- parametros$grau_tendencia
beta_tendencia <- parametros$beta_tendencia

t <- criar_tempo(parametros)

n <- length(t)

beta_sazonal <- criar_beta_sazonal(parametros$alpha, parametros$beta_h, K)

############################################################
# SIMULAÇÃO/MATRIZES
############################################################

imprimir_titulo_console("SIMULAÇÃO DA SÉRIE")

X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)
X_sazonal <- criar_matriz_fourier(t, s, K)

Xt <- simular_serie(beta_tendencia = beta_tendencia,
                    X_tendencia = X_tendencia,
                    X_sazonal = X_sazonal,
                    beta_sazonal = beta_sazonal,
                    sigma = sigma)

# componentes verdadeiros, para comparar com os estimados
tendencia_real <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia)
sazonalidade_real <- multiplicar_matriz_vetor(X_sazonal, beta_sazonal)

cat("Tamanho da série:", n, "\n")
cat("Colunas da matriz de tendência:", ncol(X_tendencia), "\n")
cat("Colunas da matriz de Fourier:", ncol(X_sazonal), "\n")
cat("Média da série simulada:", round(calcular_media_amostral(Xt), 4), "\n")

############################################################
# ESTIMAÇÃO OLS
############################################################

tendencia_media_movel <- calcular_media_movel(Xt, q)

modelo <- ajustar_modelo(t = t,
                         y = Xt,
                         grau_tendencia = grau_tendencia,
                         periodo = s,
                         K = K,
                         metodo_sazonal = "fourier")

modelo_dummies <- ajustar_modelo(t = t,
                                 y = Xt,
                                 grau_tendencia = grau_tendencia,
                                 periodo = s,
                                 K = K,
                                 metodo_sazonal = "dummies")

imprimir_titulo_console("COEFICIENTES (VERDADEIRO E ESTIMADO)")

beta_verdadeiro <- c(beta_tendencia, beta_sazonal)

for(i in 1:length(beta_verdadeiro)){
  cat("Coeficiente", i, "- verdadeiro:", round(beta_verdadeiro[i], 4),
      "- estimado:", round(modelo$beta_hat[i], 4), "\n")
}

############################################################
# DIAGNÓSTICO
############################################################

imprimir_titulo_console("DIAGNÓSTICO – FOURIER E DUMMIES")

diagnostico_fourier <- diagnostico(y = Xt, residuos = modelo$residuos)
diagnostico_dummies <- diagnostico(y = Xt, residuos = modelo_dummies$residuos)

cat("Parâmetros (Fourier):", length(modelo$beta_hat), "\n")
cat("Parâmetros (Dummies):", length(modelo_dummies$beta_hat), "\n")
cat("Média dos resíduos (Fourier):", round(diagnostico_fourier$media, 4), "\n")
cat("Média dos resíduos (Dummies):", round(diagnostico_dummies$media, 4), "\n")
cat("Variância dos resíduos (Fourier):", round(diagnostico_fourier$variancia, 4), "\n")
cat("Variância dos resíduos (Dummies):", round(diagnostico_dummies$variancia, 4), "\n")
cat("Variância do ruído (sigma²):", round(sigma^2, 4), "\n")
cat("RMSE (Fourier):", round(diagnostico_fourier$rmse, 4), "\n")
cat("RMSE (Dummies):", round(diagnostico_dummies$rmse, 4), "\n")
cat("R² (Fourier):", round(diagnostico_fourier$r2, 4), "\n")
cat("R² (Dummies):", round(diagnostico_dummies$r2, 4), "\n")

imprimir_titulo_console("TESTES DE HIPÓTESES – FOURIER")

teste_t <- teste_t_coeficientes(modelo)

for(j in 1:nrow(teste_t)){
  cat("Coeficiente", j, "- estatística t:", round(teste_t$estatistica_t[j], 4),
      "- valor-p:", round(teste_t$valor_p[j], 4), "\n")
}

teste_box <- teste_box_pierce(modelo$residuos, parametros$max_lag_box)

cat("Box-Pierce - Q:", round(teste_box$Q, 4), "\n")
cat("Box-Pierce - valor-p:", round(teste_box$valor_p, 4), "\n")

imprimir_titulo_console("VALIDAÇÃO – OLS MANUAL E lm()")

ajuste_lm <- lm(Xt ~ modelo$X_full - 1)

maior_diferenca_lm <- 0

for(i in 1:length(modelo$beta_hat)){
  diferenca <- abs(modelo$beta_hat[i] - as.numeric(coef(ajuste_lm))[i])
  if(diferenca > maior_diferenca_lm){
    maior_diferenca_lm <- diferenca
  }
}

cat("Maior diferença entre os coeficientes manuais e o lm():", round(maior_diferenca_lm, 4), "\n")
cat("Coeficientes iguais ao lm():",
    isTRUE(all.equal(modelo$beta_hat, as.numeric(coef(ajuste_lm)))), "\n")
cat("Valores ajustados iguais ao lm():",
    isTRUE(all.equal(modelo$y_hat, as.numeric(fitted(ajuste_lm)))), "\n")

############################################################
# PREVISÃO
############################################################

imprimir_titulo_console("PREVISÃO")

t_futuro <- (n + 1):(n + parametros$n_futuro)

previsoes <- prever(modelo, t_futuro)
previsoes_dummies <- prever(modelo_dummies, t_futuro)

X_tendencia_futuro <- criar_matriz_tendencia(t_futuro, grau_tendencia)
X_sazonal_futuro <- criar_matriz_fourier(t_futuro, s, K)

sinal_futuro <- multiplicar_matriz_vetor(X_tendencia_futuro, beta_tendencia) +
  multiplicar_matriz_vetor(X_sazonal_futuro, beta_sazonal)

for(i in 1:length(t_futuro)){
  cat("t =", t_futuro[i],
      "- Fourier:", round(previsoes[i], 4),
      "- Dummies:", round(previsoes_dummies[i], 4),
      "- sinal sem ruído:", round(sinal_futuro[i], 4), "\n")
}

rmse_previsao_fourier <- diagnostico(y = sinal_futuro,
                                     residuos = previsoes - sinal_futuro)$rmse

rmse_previsao_dummies <- diagnostico(y = sinal_futuro,
                                     residuos = previsoes_dummies - sinal_futuro)$rmse

cat("RMSE da previsão (Fourier):", round(rmse_previsao_fourier, 4), "\n")
cat("RMSE da previsão (Dummies):", round(rmse_previsao_dummies, 4), "\n")

############################################################
# VISUALIZAÇÃO
############################################################

imprimir_titulo_console("GRÁFICOS")

grafico <- plot_decomposicao(
  t,
  Xt,
  modelo$tendencia,
  modelo$sazonalidade,
  modelo$residuos
)
print(grafico)
salvar_grafico(grafico,
               "decomposicao/01_decomposicao_aditiva",
               parametros)

grafico <- plot_serie_ajustada(
  t,
  Xt,
  modelo$y_hat
)
print(grafico)
salvar_grafico(grafico,
               "decomposicao/02_serie_observada_ajustada",
               parametros)

grafico <- plot_tendencia(
  t,
  modelo$tendencia,
  tendencia_real
)
print(grafico)
salvar_grafico(grafico,
               "decomposicao/03_tendencia_estimada_real",
               parametros)

grafico <- plot_sazonalidade(
  t,
  modelo$sazonalidade,
  sazonalidade_real
)
print(grafico)
salvar_grafico(grafico,
               "decomposicao/04_sazonalidade_estimada_real",
               parametros)

grafico <- plot_serie_media_movel(
  t,
  Xt,
  tendencia_media_movel,
  modelo$tendencia,
  q
)
print(grafico)
salvar_grafico(grafico,
               "decomposicao/05_serie_media_movel_tendencia",
               parametros)

grafico <- plot_residuos(
  t,
  modelo$residuos
)
print(grafico)
salvar_grafico(grafico,
               "residuos/01_serie_temporal_residuos",
               parametros)

grafico <- plot_acf(
  modelo$residuos,
  "Função de Autocorrelação dos Resíduos"
)
print(grafico)
salvar_grafico(grafico,
               "residuos/02_funcao_autocorrelacao_residuos",
               parametros)

grafico <- plot_histograma(
  modelo$residuos,
  "Histograma dos Resíduos",
  expression(hat(epsilon)[t]),
  sigma^2
)
print(grafico)
salvar_grafico(grafico,
               "residuos/03_histograma_residuos",
               parametros)

grafico <- plot_previsao(
  t,
  Xt,
  t_futuro,
  previsoes,
  sinal_futuro
)
print(grafico)
salvar_grafico(grafico,
               "previsao/01_previsao_fora_amostra",
               parametros)
