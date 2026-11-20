############################################################
# ex4_eficiencia.R
# Compara as versões com loops e vetorizadas da matriz X, simulação, média móvel e
# autocovariâncias, medindo os tempos e gerando o gráfico do tempo em função de n.
# Dependências: scripts/config.R, scripts/dados.R, scripts/processamento.R e ggplot2.
# Executar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/config.R")
source("scripts/dados.R")
source("scripts/processamento.R")
source("scripts/visualizacao.R")

# Monta X com as colunas 1, t e t² usando operações vetorizadas, sem loops.
# Entrada: parametros com n e t como vetor de tempo de tamanho n.
# Saída: matriz numérica n x 3, igual à de criar_matriz_regressao.
criar_matriz_regressao_vetorizada <- function(parametros, t){
  X <- cbind(1, t, t^2)

  return(X)
}

# Simula X %*% beta + eps usando rnorm(n), sem percorrer as linhas por loop.
# Entrada: beta com ncol(X) coeficientes, X de dimensão n x p e sigma como desvio padrão.
# Saída: vetor de tamanho n, igual ao de simular_serie_com_tendencia com a mesma semente.
# Pré-condição: length(beta) == ncol(X).
simular_serie_com_tendencia_vetorizada <- function(beta, X, sigma){

  n <- nrow(X)

  eps <- rnorm(n, mean = 0, sd = sigma)

  Xt <- as.numeric(X %*% beta) + eps

  return(Xt)
}

# Calcula gamma(h) pelo produto dos desvios deslocados h posições, dividindo a soma por n.
# Entrada: serie numérica de tamanho n e h inteiro, com 0 <= h < n.
# Saída: número igual ao de calcular_autocovariancia_empirica.
# Pré-condição: serie sem NA.
calcular_autocovariancia_empirica_vetorizada <- function(serie, h){

  n <- length(serie)

  media <- mean(serie)

  d <- serie - media

  autocovariancia <- sum(d[1:(n-h)] * d[(1+h):n]) / n

  return(autocovariancia)
}

# Calcula as autocovariâncias de h = 0 até max_lag, sem loop sobre os instantes.
# Entrada: serie numérica de tamanho n e max_lag inteiro, com 0 <= max_lag < n.
# Saída: vetor de tamanho max_lag + 1.
# Pré-condição: serie sem NA.
calcular_autocovariancias_vetorizada <- function(serie, max_lag){

  autocovariancias <- sapply(0:max_lag,
                             calcular_autocovariancia_empirica_vetorizada,
                             serie = serie)

  return(autocovariancias)
}

# Calcula a média móvel centrada pela diferença entre somas acumuladas.
# Entrada: serie numérica de tamanho n e q inteiro, com 2q + 1 <= n.
# Saída: vetor de tamanho n - 2q, igual ao de calcular_media_movel.
# Pré-condição: serie sem NA.
calcular_media_movel_vetorizada <- function(serie, q){

  n <- length(serie)

  numero_medias <- n - 2*q

  medias <- numeric(numero_medias)

  if(numero_medias > 0){

    # O zero inicial permite obter a soma de cada janela pela diferença entre duas somas acumuladas.
    soma_acumulada <- c(0, cumsum(serie))

    medias <- (soma_acumulada[(2*q+2):(n+1)] - soma_acumulada[1:numero_medias]) / (2*q + 1)
  }

  return(medias)
}

# Formata max(abs(a - b)) / max(abs(a)) em notação científica, ou "0" se não houver diferença.
# Entrada: a e b são os resultados das duas versões e têm as mesmas dimensões.
# Saída: texto com a diferença relativa.
# Pré-condição: a contém pelo menos um valor diferente de zero.
formatar_diferenca_relativa <- function(a, b){

  diferenca <- max(abs(a - b)) / max(abs(a))

  if(diferenca == 0){
    texto <- "0"
  } else {
    texto <- format(diferenca, scientific = TRUE, digits = 3)
  }

  return(texto)
}

# Compara os resultados e os tempos das versões com loop e vetorizada.
# Entrada: parametros com seed, sigma, q, max_lag, beta, n_eficiencia e repeticoes_eficiencia.
# Saída: tempos, razões, igualdade e diferenças relativas mostrados no console.
# Pré-condição: funções de config.R, dados.R e processamento.R carregadas.
comparar_eficiencia <- function(parametros){

  parametros$n <- parametros$n_eficiencia
  repeticoes <- parametros$repeticoes_eficiencia
  sigma <- parametros$sigma
  q <- parametros$q
  max_lag <- parametros$max_lag

  t <- criar_tempo(parametros)
  beta <- parametros$beta

  tempo_x_loop <- system.time({
    for(r in 1:repeticoes){
      X <- criar_matriz_regressao(parametros, t)
    }
  })[["elapsed"]]

  tempo_x_vet <- system.time({
    for(r in 1:repeticoes){
      X_vet <- criar_matriz_regressao_vetorizada(parametros, t)
    }
  })[["elapsed"]]

  # Usa a mesma semente nas duas versões para comparar os resultados da simulação.
  tempo_sim_loop <- system.time({
    for(r in 1:repeticoes){
      set.seed(parametros$seed)
      Xt_loop <- simular_serie_com_tendencia(beta, X, sigma)
    }
  })[["elapsed"]]

  tempo_sim_vet <- system.time({
    for(r in 1:repeticoes){
      set.seed(parametros$seed)
      Xt_vet <- simular_serie_com_tendencia_vetorizada(beta, X, sigma)
    }
  })[["elapsed"]]

  tempo_mm_loop <- system.time({
    for(r in 1:repeticoes){
      mm_loop <- calcular_media_movel(Xt_loop, q)
    }
  })[["elapsed"]]

  tempo_mm_vet <- system.time({
    for(r in 1:repeticoes){
      mm_vet <- calcular_media_movel_vetorizada(Xt_loop, q)
    }
  })[["elapsed"]]

  tempo_ac_loop <- system.time({
    for(r in 1:repeticoes){
      ac_loop <- calcular_autocovariancias(Xt_loop, max_lag)
    }
  })[["elapsed"]]

  tempo_ac_vet <- system.time({
    for(r in 1:repeticoes){
      ac_vet <- calcular_autocovariancias_vetorizada(Xt_loop, max_lag)
    }
  })[["elapsed"]]

  cat("Tamanho da série:", parametros$n, "\n")
  cat("Repetições:", repeticoes, "\n")

  cat("\nMatriz de regressão\n")
  cat("Tempo com loop (s):", round(tempo_x_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_x_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_x_loop / tempo_x_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(X, X_vet, check.attributes = FALSE)), "\n")
  cat("Maior diferença relativa:", formatar_diferenca_relativa(X, X_vet), "\n")

  cat("\nSimulação da série\n")
  cat("Tempo com loop (s):", round(tempo_sim_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_sim_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_sim_loop / tempo_sim_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(Xt_loop, Xt_vet)), "\n")
  cat("Maior diferença relativa:", formatar_diferenca_relativa(Xt_loop, Xt_vet), "\n")

  cat("\nMédia móvel\n")
  cat("Tempo com loop (s):", round(tempo_mm_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_mm_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_mm_loop / tempo_mm_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(mm_loop, mm_vet)), "\n")
  cat("Maior diferença relativa:", formatar_diferenca_relativa(mm_loop, mm_vet), "\n")

  cat("\nAutocovariâncias\n")
  cat("Tempo com loop (s):", round(tempo_ac_loop, 3), "\n")
  cat("Tempo vetorizado (s):", round(tempo_ac_vet, 3), "\n")
  cat("Razão loop / vetorizado:", round(tempo_ac_loop / tempo_ac_vet, 1), "\n")
  cat("Mesmo resultado nas duas versões:", isTRUE(all.equal(ac_loop, ac_vet)), "\n")
  cat("Maior diferença relativa:", formatar_diferenca_relativa(ac_loop, ac_vet), "\n")
}

# Soma os tempos de várias chamadas da mesma função.
# Entrada: funcao a medir, repeticoes como inteiro positivo e ... com os argumentos de funcao.
# Saída: tempo total em segundos.
medir_tempo <- function(funcao, repeticoes, ...){

  tempo <- system.time({
    for(r in 1:repeticoes){
      resultado <- funcao(...)
    }
  })[["elapsed"]]

  return(tempo)
}

# Mede os tempos da simulação, média móvel e autocovariâncias para cada n em valores_n.
# Entrada: parametros com seed, sigma, q, max_lag, beta e repeticoes_eficiencia, além de
# valores_n como vetor de inteiros positivos.
# Saída: data.frame com n, operacao, versao e tempo, com seis linhas para cada valor de n.
# Pré-condição: max_lag < 2q + 1 <= min(valores_n) e funções de config.R, dados.R
# e processamento.R carregadas.
medir_tempos_por_n <- function(parametros, valores_n){

  repeticoes <- parametros$repeticoes_eficiencia
  sigma <- parametros$sigma
  q <- parametros$q
  max_lag <- parametros$max_lag
  beta <- parametros$beta

  operacoes <- c("Simulação da Série", "Simulação da Série",
                 "Média Móvel", "Média Móvel",
                 "Autocovariâncias", "Autocovariâncias")
  versoes <- c("Loop", "Vetorizada", "Loop", "Vetorizada", "Loop", "Vetorizada")

  tempos <- data.frame()

  for(i in 1:length(valores_n)){

    parametros$n <- valores_n[i]
    t <- criar_tempo(parametros)
    X <- criar_matriz_regressao(parametros, t)

    set.seed(parametros$seed)
    Xt <- simular_serie_com_tendencia(beta, X, sigma)

    tempo <- c(
      medir_tempo(simular_serie_com_tendencia, repeticoes, beta, X, sigma),
      medir_tempo(simular_serie_com_tendencia_vetorizada, repeticoes, beta, X, sigma),
      medir_tempo(calcular_media_movel, repeticoes, Xt, q),
      medir_tempo(calcular_media_movel_vetorizada, repeticoes, Xt, q),
      medir_tempo(calcular_autocovariancias, repeticoes, Xt, max_lag),
      medir_tempo(calcular_autocovariancias_vetorizada, repeticoes, Xt, max_lag)
    )

    tempos <- rbind(tempos,
                    data.frame(n = valores_n[i],
                               operacao = operacoes,
                               versao = versoes,
                               tempo = tempo))
  }

  # Mantém a mesma ordem das operações nos painéis e na legenda.
  tempos$operacao <- factor(tempos$operacao,
                            levels = c("Simulação da Série", "Média Móvel", "Autocovariâncias"))
  tempos$versao <- factor(tempos$versao,
                          levels = c("Loop", "Vetorizada"))

  return(tempos)
}

# Mostra o tempo em função de n, com um painel por operação e uma linha para cada versão.
# Entrada: tempos como data.frame de medir_tempos_por_n e repeticoes para o eixo y.
# Saída: objeto ggplot.
plot_tempo_eficiencia <- function(tempos, repeticoes){

  valores_n <- unique(tempos$n)

  grafico <- ggplot(data = tempos,
                    aes(x = n,
                        y = tempo,
                        color = versao)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.4) +
    facet_wrap(~ operacao, nrow = 1, scales = "free_y") +
    scale_color_manual(
      values = c("Loop" = cor_referencia,
                 "Vetorizada" = cor_acf)
    ) +
    scale_x_continuous(breaks = valores_n,
                       labels = as.character(valores_n)) +
    expand_limits(y = 0) +
    labs(title = "Tempo de Execução por Tamanho da Série",
         x = "Tamanho da série (n)",
         y = paste0("Tempo de ", repeticoes, " repetições (s)")) +
    tema_graficos() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))

  return(grafico)
}

parametros <- ler_parametros("parametros.json")

comparar_eficiencia(parametros)

valores_n <- c(1000L, 25000L, parametros$n_eficiencia)

tempos <- medir_tempos_por_n(parametros, valores_n)

grafico <- plot_tempo_eficiencia(tempos, parametros$repeticoes_eficiencia)
print(grafico)
salvar_grafico(grafico,
               "eficiencia/01_tempo_loop_vetorizada",
               parametros)
