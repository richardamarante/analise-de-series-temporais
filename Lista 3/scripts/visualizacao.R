############################################################
# visualizacao.R
# Gera os gráficos da série, média móvel, tendência e FAC, incluindo a comparação com
# acf() do R e a figura integrada.
# Dependências: pacotes ggplot2 e patchwork.
############################################################

cor_principal <- "#37474F"
cor_referencia <- "#C44E52"
cor_acf <- "#4C78A8"
cor_tendencia <- "#8E5AD7"

# Define o tema usado em todos os gráficos para manter a mesma aparência.
# Entrada: nenhuma.
# Saída: tema do ggplot2.
tema_graficos <- function(){
  return(
    theme_classic(base_size = 13) +
      theme(
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.line = element_line(linewidth = 0.7),
        axis.ticks = element_line(linewidth = 0.6),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10)
      )
  )
}

# Ajusta as escalas para o eixo x começar no primeiro instante, com 2% de folga à direita,
# enquanto o eixo y recebe 5% de folga em cima e embaixo.
# Entrada: grafico como objeto ggplot.
# Saída: objeto ggplot com as escalas ajustadas.
ajustar_eixos <- function(grafico){
  grafico <- grafico +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(expand = expansion(mult = 0.05))

  return(grafico)
}

# Salva o gráfico em graficos/<nome_arquivo>.png quando salvar_graficos é true.
# Entrada: grafico como ggplot ou patchwork, nome_arquivo sem extensão, parametros com
# salvar_graficos e medidas e fator_altura numérico, com padrão 1.
# Saída: nenhuma, apenas grava o arquivo.
salvar_grafico <- function(grafico, nome_arquivo, parametros, fator_altura = 1){
  if(parametros$salvar_graficos){
    caminho <- paste0("graficos/", nome_arquivo, ".png")
    pasta <- dirname(caminho)
    dir.create(pasta, showWarnings = FALSE, recursive = TRUE)

    ggsave(
      filename = caminho,
      plot = grafico,
      width = parametros$largura_grafico,
      height = parametros$altura_grafico * fator_altura,
      dpi = parametros$dpi_grafico
    )
  }
}

# Imprime o título de uma etapa no console.
# Entrada: titulo como texto.
# Saída: nenhuma, apenas imprime o título.
imprimir_titulo_console <- function(titulo){
  cat("\n# -- ", titulo, " -- #\n\n", sep = "")
}

# Mostra a série temporal em um gráfico.
# Entrada: tempo e serie como vetores de tamanho n, titulo e ylab como textos e
# rotulo_serie como texto da legenda.
# Saída: objeto ggplot.
plot_serie_temporal <- function(tempo,
                                serie,
                                titulo,
                                ylab = expression(X[t]),
                                rotulo_serie = "Série Simulada"){

  dados <- data.frame(
    tempo = tempo,
    serie = serie
  )

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = serie,
                  color = rotulo_serie),
              linewidth = 0.9) +
    scale_color_manual(
      values = setNames(c(cor_principal), rotulo_serie),
      breaks = c(rotulo_serie)
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = titulo,
         x = "Tempo (t)",
         y = ylab) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# Mostra a série, a média móvel centrada e a tendência verdadeira X %*% beta.
# Entrada: tempo e serie como vetores de tamanho n, media_movel de tamanho n - 2q,
# tendencia de tamanho n, q inteiro e titulo e ylab como textos.
# Saída: objeto ggplot.
# Pré-condição: length(media_movel) == n - 2q e length(tendencia) == n.
plot_serie_media_movel <- function(tempo,
                                   serie,
                                   media_movel,
                                   tendencia,
                                   q,
                                   titulo,
                                   ylab = expression(X[t])){

  n <- length(serie)

  dados <- data.frame(
    tempo = tempo,
    serie = serie,
    tendencia = tendencia
  )

  dados_media <- data.frame(
    tempo = (q+1):(n-q),
    media_movel = media_movel
  )

  rotulo_media <- paste0("Média Móvel (q = ", q, ")")
  rotulo_tendencia <- "Tendência Verdadeira"

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = serie,
                  color = "Série Xₜ"),
              linewidth = 0.9) +
    geom_line(data = dados_media,
              aes(x = tempo,
                  y = media_movel,
                  color = rotulo_media),
              linewidth = 1.3) +
    geom_line(data = dados,
              aes(x = tempo,
                  y = tendencia,
                  color = rotulo_tendencia),
              linewidth = 1.1,
              linetype = "dashed") +
    scale_color_manual(
      values = setNames(c(cor_principal, cor_referencia, cor_tendencia),
                        c("Série Xₜ", rotulo_media, rotulo_tendencia)),
      breaks = c("Série Xₜ", rotulo_media, rotulo_tendencia)
    ) +
    guides(color = guide_legend(
      override.aes = list(linetype = c("solid", "solid", "dashed"),
                          linewidth = 1.2,
                          alpha = 1)
    )) +
    labs(title = titulo,
         x = "Tempo (t)",
         y = ylab) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# Mostra a FAC empírica em hastes, com limites de 95% dados por ±1,96 / sqrt(n).
# Entrada: fac com os valores de rho(0) até rho(max_lag), n como tamanho da série
# e titulo como texto.
# Saída: objeto ggplot.
plot_fac_empirica <- function(fac, n, titulo){

  dados <- data.frame(
    lag = 0:(length(fac) - 1),
    acf = fac
  )

  limite <- 1.96 / sqrt(n)
  maior_lag <- max(dados$lag)

  limites <- data.frame(
    y = c(-limite, limite),
    grupo = "Limites de Confiança (95%)"
  )

  grafico <- ggplot() +
    geom_hline(yintercept = 0,
               color = "black",
               linewidth = 0.6) +
    geom_hline(data = limites,
               aes(yintercept = y,
                   color = grupo),
               linetype = "dashed",
               linewidth = 0.7) +
    geom_segment(data = dados,
                 aes(x = lag,
                     xend = lag,
                     y = 0,
                     yend = acf,
                     color = "FAC Empírica"),
                 linewidth = 0.8) +
    scale_color_manual(
      values = c(
        "FAC Empírica" = cor_principal,
        "Limites de Confiança (95%)" = cor_acf
      ),
      breaks = c("FAC Empírica", "Limites de Confiança (95%)")
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    scale_x_continuous(
      limits = c(-0.5, maior_lag + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
    labs(title = titulo,
         x = "Defasagem",
         y = "ACF") +
    tema_graficos()

  return(grafico)
}

# Compara a FAC manual em hastes com os valores da acf() do R em pontos.
# Entrada: fac como vetor da FAC manual, fac_r como vetor da acf() do R e titulo como texto.
# Saída: objeto ggplot.
# Pré-condição: fac e fac_r têm o mesmo tamanho.
plot_comparacao_acf_r <- function(fac, fac_r, titulo){

  dados <- data.frame(
    lag = 0:(length(fac) - 1),
    fac = fac,
    fac_r = fac_r
  )

  maior_lag <- max(dados$lag)

  grafico <- ggplot() +
    geom_hline(yintercept = 0,
               color = "black",
               linewidth = 0.6) +
    geom_segment(data = dados,
                 aes(x = lag,
                     xend = lag,
                     y = 0,
                     yend = fac,
                     color = "FAC Empírica (Manual)"),
                 linewidth = 1.6) +
    geom_point(data = dados,
               aes(x = lag,
                   y = fac_r,
                   color = "acf() do R"),
               size = 2.6) +
    scale_color_manual(
      values = c(
        "FAC Empírica (Manual)" = cor_acf,
        "acf() do R" = cor_referencia
      ),
      breaks = c("FAC Empírica (Manual)", "acf() do R")
    ) +
    guides(color = guide_legend(
      override.aes = list(linetype = c(1, 0),
                          shape = c(NA, 16),
                          linewidth = 1.2,
                          alpha = 1)
    )) +
    scale_x_continuous(
      limits = c(-0.5, maior_lag + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
    labs(title = titulo,
         x = "Defasagem",
         y = "ACF") +
    tema_graficos()

  return(grafico)
}

# Junta o gráfico da série em cima e o da FAC embaixo.
# Entrada: grafico_serie e grafico_fac como objetos ggplot.
# Saída: objeto patchwork, salvo com fator_altura = 1.6.
plot_grafico_integrado <- function(grafico_serie, grafico_fac){
  integrado <- grafico_serie / grafico_fac

  return(integrado)
}
