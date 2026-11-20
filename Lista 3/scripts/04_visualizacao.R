############################################################
# 04_visualizacao.R
# Gráficos com ggplot2: série, série com média móvel, FAC
# empírica, comparação com a acf() do R e figura integrada.
# Dependências: pacotes ggplot2 e patchwork.
############################################################

# paleta de cores
cor_principal <- "#37474F"
cor_referencia <- "#C44E52"
cor_trajetorias <- "#B0B0B0"
cor_acf <- "#4C78A8"
cor_variancia <- "#8E5AD7"
cor_covariancia <- "#3F7F8F"
cor_histograma <- "#90A4AE"
cor_janela_c1 <- "#000000"
cor_janela_c2 <- "#5B8DB8"
cor_cilindro <- "#E15759"

# tema usado em todos os gráficos
# saída: tema do ggplot2
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

# eixos sem espaço extra nas bordas
# entrada: grafico (objeto ggplot)
# saída: o mesmo gráfico com as escalas contínuas ajustadas
ajustar_eixos <- function(grafico){
  grafico <- grafico +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  return(grafico)
}

# grava o gráfico em graficos/<nome_arquivo>.png se salvar_graficos for true
# entrada: grafico (ggplot ou patchwork), nome_arquivo (texto sem extensão),
#          parametros (lista com salvar_graficos e as medidas), fator_altura (número, 1 por padrão)
# saída: nenhuma, só grava o arquivo
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

# título de etapa no console
# entrada: titulo (texto)
# saída: nenhuma, só imprime
imprimir_titulo_console <- function(titulo){
  cat("\n# -- ", titulo, " -- #\n\n", sep = "")
}

# série temporal simples
# entrada: tempo (vetor de tamanho n), serie (vetor de tamanho n), titulo (texto),
#          ylab (rótulo do eixo y), rotulo_serie (texto da legenda)
# saída: objeto ggplot
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

# série com a média móvel centrada por cima, na mesma figura
# entrada: tempo (vetor de tamanho n), serie (vetor de tamanho n),
#          media_movel (vetor de tamanho n - 2q), q (inteiro), titulo (texto), ylab (rótulo do eixo y)
# saída: objeto ggplot
# pré-condição: length(media_movel) == n - 2q
plot_serie_media_movel <- function(tempo,
                                   serie,
                                   media_movel,
                                   q,
                                   titulo,
                                   ylab = expression(X[t])){

  n <- length(serie)

  dados <- data.frame(
    tempo = tempo,
    serie = serie
  )

  dados_media <- data.frame(
    tempo = (q+1):(n-q),
    media_movel = media_movel
  )

  rotulo_media <- paste0("Média Móvel (q = ", q, ")")

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
    scale_color_manual(
      values = setNames(c(cor_principal, cor_referencia),
                        c("Série Xₜ", rotulo_media)),
      breaks = c("Série Xₜ", rotulo_media)
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

# FAC empírica em hastes, com os limites de 95% (±1,96 / raiz de n)
# entrada: fac (vetor rho(0), ..., rho(max_lag)), n (tamanho da série), titulo (texto)
# saída: objeto ggplot
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

# FAC manual (hastes) contra a da acf() do R (pontos)
# entrada: fac (vetor manual), fac_r (vetor da acf() do R), titulo (texto)
# saída: objeto ggplot
# pré-condição: os dois vetores têm o mesmo tamanho
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

# figura integrada: série com média móvel em cima e FAC embaixo
# entrada: grafico_serie (ggplot), grafico_fac (ggplot)
# saída: objeto patchwork, salvar com fator_altura = 1.6
plot_grafico_integrado <- function(grafico_serie, grafico_fac){
  integrado <- grafico_serie / grafico_fac

  return(integrado)
}
