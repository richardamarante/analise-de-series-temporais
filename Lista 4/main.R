############################################################
# main.R
# Coordena configuração, simulação, OLS, diagnóstico, previsão e gráficos.
# Dependências: parametros.json e scripts/config.R, dados.R, processamento.R e visualizacao.R.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/config.R")
source("scripts/dados.R")
source("scripts/processamento.R")
source("scripts/visualizacao.R")

parametros <- ler_parametros("parametros.json")

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

# Matrizes de regressores, usadas no ajuste e na previsão.
X_tendencia <- criar_matriz_tendencia(t, grau_tendencia)
X_fourier <- criar_matriz_fourier(t, s, K)
X_dummies <- criar_matriz_dummies(t, s)

Xt <- simular_serie(beta_tendencia = beta_tendencia,
                    X_tendencia = X_tendencia,
                    X_sazonal = X_fourier,
                    beta_sazonal = beta_sazonal,
                    sigma = sigma)

# Componentes verdadeiros, para comparar com os estimados.
tendencia_real <- multiplicar_matriz_vetor(X_tendencia, beta_tendencia)
sazonalidade_real <- multiplicar_matriz_vetor(X_fourier, beta_sazonal)

cat("Tamanho da série:", n, "\n")
cat("Colunas da matriz de tendência:", ncol(X_tendencia), "\n")
cat("Colunas da matriz de Fourier:", ncol(X_fourier), "\n")
cat("Média da série simulada:", round(calcular_media_amostral(Xt), 4), "\n")

############################################################
# ESTIMAÇÃO OLS
############################################################

tendencia_media_movel <- calcular_media_movel(Xt, q)

modelo <- ajustar_modelo(y = Xt,
                         X_tendencia = X_tendencia,
                         X_sazonal = X_fourier)

modelo_dummies <- ajustar_modelo(y = Xt,
                                 X_tendencia = X_tendencia,
                                 X_sazonal = X_dummies)

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

# Efeito de cada mês, com média zero no ano, para comparar Fourier e dummies.
sazonalidade_mensal_real <- calcular_sazonalidade_mensal(t, sazonalidade_real, s)
sazonalidade_mensal_fourier <- calcular_sazonalidade_mensal(t, modelo$sazonalidade, s)
sazonalidade_mensal_dummies <- calcular_sazonalidade_mensal(t, modelo_dummies$sazonalidade, s)

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

# Matrizes futuras com as mesmas colunas do ajuste.
X_tendencia_futuro <- criar_matriz_tendencia(t_futuro, grau_tendencia)
X_fourier_futuro <- criar_matriz_fourier(t_futuro, s, K)
X_dummies_futuro <- criar_matriz_dummies(t_futuro, s)

previsoes <- prever(modelo, X_tendencia_futuro, X_fourier_futuro)
previsoes_dummies <- prever(modelo_dummies, X_tendencia_futuro, X_dummies_futuro)

sinal_futuro <- multiplicar_matriz_vetor(X_tendencia_futuro, beta_tendencia) +
  multiplicar_matriz_vetor(X_fourier_futuro, beta_sazonal)

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

# Divisão por data para a validação fora da amostra: 60% treino, 20% validação e 20% teste.
imprimir_titulo_console("TREINO, VALIDAÇÃO E TESTE")

n_treino <- round(parametros$proporcao_treino * n)
n_validacao <- round(parametros$proporcao_validacao * n)

t_treino <- 1:n_treino
t_validacao <- (n_treino + 1):(n_treino + n_validacao)
t_ajuste_final <- 1:(n_treino + n_validacao)
t_teste <- (n_treino + n_validacao + 1):n

cat("Treino: t = 1 a", n_treino, "\n")
cat("Validação: t =", t_validacao[1], "a", t_validacao[n_validacao], "\n")
cat("Teste: t =", t_teste[1], "a", n, "\n")

media_treino <- calcular_media_amostral(Xt[t_treino])
media_ajuste_final <- calcular_media_amostral(Xt[t_ajuste_final])

# Matrizes de tendência de cada trecho.
X_tendencia_treino <- criar_matriz_tendencia(t_treino, grau_tendencia)
X_tendencia_validacao <- criar_matriz_tendencia(t_validacao, grau_tendencia)
X_tendencia_ajuste_final <- criar_matriz_tendencia(t_ajuste_final, grau_tendencia)
X_tendencia_teste <- criar_matriz_tendencia(t_teste, grau_tendencia)

# Fourier: ajusta no treino e prevê a validação; depois reajusta com treino e validação e prevê o teste.
modelo_treino_fourier <- ajustar_modelo(Xt[t_treino], X_tendencia_treino, criar_matriz_fourier(t_treino, s, K))
previsao_validacao_fourier <- prever(modelo_treino_fourier, X_tendencia_validacao, criar_matriz_fourier(t_validacao, s, K))

modelo_final_fourier <- ajustar_modelo(Xt[t_ajuste_final], X_tendencia_ajuste_final, criar_matriz_fourier(t_ajuste_final, s, K))
previsao_teste_fourier <- prever(modelo_final_fourier, X_tendencia_teste, criar_matriz_fourier(t_teste, s, K))

# Dummies: os mesmos passos.
modelo_treino_dummies <- ajustar_modelo(Xt[t_treino], X_tendencia_treino, criar_matriz_dummies(t_treino, s))
previsao_validacao_dummies <- prever(modelo_treino_dummies, X_tendencia_validacao, criar_matriz_dummies(t_validacao, s))

modelo_final_dummies <- ajustar_modelo(Xt[t_ajuste_final], X_tendencia_ajuste_final, criar_matriz_dummies(t_ajuste_final, s))
previsao_teste_dummies <- prever(modelo_final_dummies, X_tendencia_teste, criar_matriz_dummies(t_teste, s))

# Erros de cada trecho. No teste, o R² usa a média dos dados do ajuste final.
erro_treino_fourier <- avaliar_previsao(Xt[t_treino], modelo_treino_fourier$y_hat, media_treino)
erro_validacao_fourier <- avaliar_previsao(Xt[t_validacao], previsao_validacao_fourier, media_treino)
erro_teste_fourier <- avaliar_previsao(Xt[t_teste], previsao_teste_fourier, media_ajuste_final)

erro_treino_dummies <- avaliar_previsao(Xt[t_treino], modelo_treino_dummies$y_hat, media_treino)
erro_validacao_dummies <- avaliar_previsao(Xt[t_validacao], previsao_validacao_dummies, media_treino)
erro_teste_dummies <- avaliar_previsao(Xt[t_teste], previsao_teste_dummies, media_ajuste_final)

cat("Fourier no treino - RMSE:", round(erro_treino_fourier$rmse, 4), "- MAE:", round(erro_treino_fourier$mae, 4), "- R²:", round(erro_treino_fourier$r2, 4), "\n")
cat("Fourier na validação - RMSE:", round(erro_validacao_fourier$rmse, 4), "- MAE:", round(erro_validacao_fourier$mae, 4), "- R²:", round(erro_validacao_fourier$r2, 4), "\n")
cat("Fourier no teste - RMSE:", round(erro_teste_fourier$rmse, 4), "- MAE:", round(erro_teste_fourier$mae, 4), "- R²:", round(erro_teste_fourier$r2, 4), "\n")
cat("Dummies no treino - RMSE:", round(erro_treino_dummies$rmse, 4), "- MAE:", round(erro_treino_dummies$mae, 4), "- R²:", round(erro_treino_dummies$r2, 4), "\n")
cat("Dummies na validação - RMSE:", round(erro_validacao_dummies$rmse, 4), "- MAE:", round(erro_validacao_dummies$mae, 4), "- R²:", round(erro_validacao_dummies$r2, 4), "\n")
cat("Dummies no teste - RMSE:", round(erro_teste_dummies$rmse, 4), "- MAE:", round(erro_teste_dummies$mae, 4), "- R²:", round(erro_teste_dummies$r2, 4), "\n")

# Escolhe o método com menor RMSE na validação.
if(erro_validacao_fourier$rmse <= erro_validacao_dummies$rmse){
  metodo_escolhido <- "Fourier"
  rmse_teste_escolhido <- erro_teste_fourier$rmse
} else {
  metodo_escolhido <- "Dummies"
  rmse_teste_escolhido <- erro_teste_dummies$rmse
}

cat("Método escolhido pela validação:", metodo_escolhido, "\n")
cat("RMSE no teste do método escolhido:", round(rmse_teste_escolhido, 4), "\n")

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
# Gráfico de quatro painéis, com o dobro da altura dos demais.
salvar_grafico(grafico,
               "decomposicao/01_decomposicao_aditiva",
               parametros,
               fator_altura = 2)

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

grafico <- plot_sazonalidade_mensal(
  sazonalidade_mensal_real,
  sazonalidade_mensal_fourier,
  sazonalidade_mensal_dummies
)
print(grafico)
salvar_grafico(grafico,
               "decomposicao/06_sazonalidade_mensal_fourier_dummies",
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
  previsoes_dummies,
  sinal_futuro
)
print(grafico)
salvar_grafico(grafico,
               "previsao/01_previsao_fora_amostra",
               parametros)

grafico <- plot_divisao_temporal(t,
                                 Xt,
                                 t_validacao,
                                 t_teste,
                                 c(previsao_validacao_fourier, previsao_teste_fourier),
                                 c(previsao_validacao_dummies, previsao_teste_dummies))
print(grafico)
salvar_grafico(grafico,
               "previsao/02_previsao_validacao_teste",
               parametros)

grafico <- plot_metricas_divisao(c(erro_treino_fourier$rmse, erro_validacao_fourier$rmse, erro_teste_fourier$rmse),
                                 c(erro_treino_dummies$rmse, erro_validacao_dummies$rmse, erro_teste_dummies$rmse))
print(grafico)
salvar_grafico(grafico,
               "previsao/03_rmse_treino_validacao_teste",
               parametros)
