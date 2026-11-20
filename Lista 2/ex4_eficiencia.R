############################################################
# ex4_eficiencia.R
# Compara loops e versões vetorizadas do passeio aleatório e das autocovariâncias.
# Mostra os resultados no console e o tempo em função de n em um gráfico.
# Dependências: config.R, dados.R, processamento.R, visualizacao.R e ggplot2.
# Execução: usar a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/config.R")
source("scripts/dados.R")
source("scripts/processamento.R")
source("scripts/visualizacao.R")

# Calcula as autocovariâncias de 0 até max_lag sem loop interno sobre t.
# Entrada: serie (vetor numérico de tamanho n), max_lag (inteiro, 0 <= max_lag < n).
# Saída: Vetor de tamanho max_lag + 1 com gamma(0), ..., gamma(max_lag).
calcular_autocovariancias_vetorizada <- function(serie, max_lag){

  n <- length(serie)

  # Calcula os desvios uma vez, sem repetir a média em cada lag.
  d <- serie - mean(serie)

  autocovariancias <- numeric(max_lag + 1)

  # Os trechos d[1:(n-h)] e d[(h+1):n] são os desvios separados por h posições.
  # O produto elemento a elemento e sum() substituem o loop sobre t.
  # Com h = 0, os trechos coincidem e a soma dos quadrados dá a variância.
  for(h in 0:max_lag){
    autocovariancias[h+1] <- sum(d[1:(n-h)] * d[(1+h):n]) / n
  }

  return(autocovariancias)
}

# Calcula a FAC empírica a partir das autocovariâncias vetorizadas.
# Entrada: serie (vetor numérico de tamanho n, não constante), max_lag (inteiro, 0 <= max_lag < n).
# Saída: Vetor de tamanho max_lag + 1 com rho(0), ..., rho(max_lag).
calcular_fac_empirica_vetorizada <- function(serie, max_lag){

  autocovariancias <- calcular_autocovariancias_vetorizada(serie, max_lag)

  return(autocovariancias / autocovariancias[1])
}

# Mede o tempo decorrido na execução de uma expressão com Sys.time().
# Entrada: expressao (código R avaliado dentro da função).
# Saída: Tempo decorrido em segundos.
medir_tempo <- function(expressao){

  # Executa gc() antes da medição para evitar a coleta de lixo durante o cronômetro.
  gc()

  # Usa Sys.time() porque system.time() tem resolução de cerca de 0,016 s no Windows.
  # As repetições de cumsum são mais rápidas e podem aparecer como tempo zero.
  inicio <- Sys.time()

  # A expressão é avaliada aqui, entre as duas leituras do relógio.
  expressao

  fim <- Sys.time()

  return(as.numeric(difftime(fim, inicio, units = "secs")))
}

# Compara tempos e resultados das versões com loop e vetorizadas, sem incluir o AR(1) recursivo.
# Entrada: parametros (lista com seed, sigma2, max_lag, n_eficiencia e repeticoes_eficiencia).
# Saída: Imprime as comparações no console.
# Pré-condição: config.R, dados.R e processamento.R carregados.
comparar_eficiencia <- function(parametros){

  n <- parametros$n_eficiencia
  sigma2 <- parametros$sigma2
  max_lag <- parametros$max_lag
  repeticoes <- parametros$repeticoes_eficiencia

  cat("n:", n, "\n")
  cat("max_lag:", max_lag, "\n")
  cat("repetições:", repeticoes, "\n")

  # Sorteia os choques uma vez, fora do cronômetro, para as duas versões usarem o mesmo eps.
  fixar_semente(parametros)
  eps <- simular_ruido_branco(n, sigma2)

  x_loop <- acumular_passeio_aleatorio(eps)

  # cumsum() calcula em código compilado a mesma soma acumulada do loop.
  x_vet <- cumsum(eps)

  # Cronometra apenas o acúmulo dos choques, sem incluir rnorm().
  tempo_loop <- medir_tempo(for(r in 1:repeticoes) acumular_passeio_aleatorio(eps))
  tempo_vet <- medir_tempo(for(r in 1:repeticoes) cumsum(eps))

  cat("\nPasseio aleatório (soma acumulada)\n")
  cat("Tempo do loop for (s):", round(tempo_loop, 4), "\n")
  cat("Tempo do cumsum (s):", round(tempo_vet, 4), "\n")
  cat("Razão loop / cumsum:", round(tempo_loop / tempo_vet, 3), "\n")

  # cumsum() usa precisão estendida (long double) e o loop usa double.
  # all.equal() compara com tolerância, pois os resultados podem diferir nas últimas casas decimais.
  cat("Mesmo resultado nas duas versões (all.equal):", isTRUE(all.equal(x_loop, x_vet)), "\n")
  cat("Maior diferença absoluta entre as séries:", signif(max(abs(x_loop - x_vet)), 3), "\n")

  # Divide a maior diferença absoluta pelo maior módulo de x_loop para obter a diferença relativa.
  cat("Maior diferença relativa:", signif(max(abs(x_loop - x_vet)) / max(abs(x_loop)), 3), "\n")

  # Usa a mesma série para calcular as autocovariâncias nas duas versões.
  serie <- x_vet

  gama_loop <- calcular_autocovariancias(serie, max_lag)
  gama_vet <- calcular_autocovariancias_vetorizada(serie, max_lag)

  tempo_loop <- medir_tempo(for(r in 1:repeticoes) calcular_autocovariancias(serie, max_lag))
  tempo_vet <- medir_tempo(for(r in 1:repeticoes) calcular_autocovariancias_vetorizada(serie, max_lag))

  cat("\nAutocovariâncias de 0 a max_lag\n")
  cat("Tempo dos loops for (s):", round(tempo_loop, 4), "\n")
  cat("Tempo da versão vetorizada (s):", round(tempo_vet, 4), "\n")
  cat("Razão loop / vetorizada:", round(tempo_loop / tempo_vet, 3), "\n")
  cat("Mesmo resultado nas duas versões (all.equal):", isTRUE(all.equal(gama_loop, gama_vet)), "\n")
  cat("Maior diferença absoluta entre as autocovariâncias:", signif(max(abs(gama_loop - gama_vet)), 3), "\n")

  # Divide a maior diferença absoluta pelo maior módulo de gama_loop para comparar o erro relativo.
  cat("Maior diferença relativa:", signif(max(abs(gama_loop - gama_vet)) / max(abs(gama_loop)), 3), "\n")

  fac_loop <- calcular_fac_empirica(serie, max_lag)
  fac_vet <- calcular_fac_empirica_vetorizada(serie, max_lag)

  # A FAC fica entre -1 e 1, com rho(0) = 1, então a diferença absoluta já mostra o erro nessa escala.
  cat("\nFAC empírica\n")
  cat("Mesmo resultado nas duas versões (all.equal):", isTRUE(all.equal(fac_loop, fac_vet)), "\n")
  cat("Maior diferença absoluta entre as FACs:", signif(max(abs(fac_loop - fac_vet)), 3), "\n")
}

# Mede o tempo do loop e da versão vetorizada para cada n e cada cálculo.
# Entrada: parametros (lista com seed, sigma2, max_lag, repeticoes_eficiencia e n_grafico_eficiencia).
# Saída: data.frame com n, calculo, versao e tempo, uma linha por medição.
# Pré-condição: config.R, dados.R e processamento.R carregados.
medir_tempos_por_n <- function(parametros){

  sigma2 <- parametros$sigma2
  max_lag <- parametros$max_lag
  repeticoes <- parametros$repeticoes_eficiencia

  tempos <- data.frame()

  for(n in parametros$n_grafico_eficiencia){

    # Sorteia os choques fora do cronômetro e usa o mesmo passeio nas autocovariâncias.
    fixar_semente(parametros)
    eps <- simular_ruido_branco(n, sigma2)
    serie <- cumsum(eps)

    tempo_soma_loop <- medir_tempo(for(r in 1:repeticoes) acumular_passeio_aleatorio(eps))
    tempo_soma_vet <- medir_tempo(for(r in 1:repeticoes) cumsum(eps))
    tempo_gama_loop <- medir_tempo(for(r in 1:repeticoes) calcular_autocovariancias(serie, max_lag))
    tempo_gama_vet <- medir_tempo(for(r in 1:repeticoes) calcular_autocovariancias_vetorizada(serie, max_lag))

    tempos <- rbind(tempos, data.frame(
      n = n,
      calculo = c(rep("Passeio aleatório (soma acumulada)", 2),
                  rep("Autocovariâncias de 0 a max_lag", 2)),
      versao = c("Loop for", "Vetorizada", "Loop for", "Vetorizada"),
      tempo = c(tempo_soma_loop, tempo_soma_vet, tempo_gama_loop, tempo_gama_vet)
    ))
  }

  return(tempos)
}

# Mostra o tempo em função de n, com uma linha por versão e um painel por cálculo.
# Entrada: tempos (data.frame com n, calculo, versao e tempo), titulo (texto), repeticoes (inteiro).
# Saída: Objeto ggplot.
# Pré-condição: ggplot2 e visualizacao.R carregados.
plot_tempo_eficiencia <- function(tempos, titulo, repeticoes){

  # Mantém os painéis na ordem do console: soma acumulada e autocovariâncias.
  tempos$calculo <- factor(tempos$calculo, levels = unique(tempos$calculo))

  grafico <- ggplot(tempos,
                    aes(x = n,
                        y = tempo,
                        color = versao)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.4) +
    # Usa uma escala vertical por painel, pois as autocovariâncias levam mais tempo.
    facet_wrap(~ calculo, scales = "free_y") +
    scale_color_manual(
      values = c("Loop for" = cor_referencia, "Vetorizada" = cor_acf),
      breaks = c("Loop for", "Vetorizada")
    ) +
    labs(title = titulo,
         x = "Tamanho da série (n)",
         y = paste0("Tempo das ", repeticoes, " repetições (s)")) +
    tema_graficos()

  return(grafico)
}

parametros <- ler_parametros("parametros.json")

comparar_eficiencia(parametros)

tempos <- medir_tempos_por_n(parametros)

grafico <- plot_tempo_eficiencia(
  tempos,
  "Tempo do Loop for e da Versão Vetorizada",
  parametros$repeticoes_eficiencia
)
print(grafico)
salvar_grafico(grafico,
               "eficiencia/01_tempo_loop_vetorizada",
               parametros)
