############################################################
# 04_visualizacao.R
# Gráficos com ggplot2: série, FAC empírica, FAC empírica x
# teórica e FAC manual x acf() do R (sem cálculos).
# Dependências: pacote ggplot2.
############################################################

# paleta de cores
cor_principal <- "#37474F"
cor_referencia <- "#C44E52"
cor_acf <- "#4C78A8"
cor_fac_teorica <- "#2E7D32"

# tema comum a todos os gráficos
# entrada: nenhuma
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

# tira a folga dos eixos de um gráfico
# entrada: grafico (objeto ggplot)
# saída: objeto ggplot com os eixos sem espaço extra
ajustar_eixos <- function(grafico){
  grafico <- grafico +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  return(grafico)
}

# grava o gráfico em graficos/<nome_arquivo>.png, criando a pasta se preciso
# entrada: grafico (objeto ggplot), nome_arquivo (texto sem extensão, ex.: "ar1/01_serie_temporal_ar1"), parametros (lista com salvar_graficos, largura_grafico, altura_grafico, dpi_grafico)
# saída: nenhuma (grava o arquivo)
salvar_grafico <- function(grafico, nome_arquivo, parametros){
  if(parametros$salvar_graficos){
    caminho <- paste0("graficos/", nome_arquivo, ".png")
    pasta <- dirname(caminho)
    dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
    
    ggsave(
      filename = caminho,
      plot = grafico,
      width = parametros$largura_grafico,
      height = parametros$altura_grafico,
      dpi = parametros$dpi_grafico
    )
  }
}

# série temporal com a linha da média zero
# entrada: serie (vetor numérico de tamanho n), titulo (texto), ylab (expressão do eixo y), rotulo_serie (texto da legenda)
# saída: objeto ggplot
plot_serie_temporal <- function(serie,
                                 titulo,
                                 ylab = expression(X[t]),
                                 rotulo_serie = "Série Simulada"){

  n <- length(serie)

  dados <- data.frame(
    tempo = 1:n,
    serie = serie
  )

  media_zero <- data.frame(y = 0)

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = serie,
                  color = rotulo_serie),
              linewidth = 0.9) +
    geom_hline(data = media_zero,
               aes(yintercept = y,
                   color = "μ = 0"),
               linetype = "dashed",
               linewidth = 0.7) +
    scale_color_manual(
      values = setNames(c(cor_principal, cor_referencia),
                        c(rotulo_serie, "μ = 0")),
      breaks = c(rotulo_serie, "μ = 0")
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

# FAC empírica em segmentos, com limites de confiança de 95%: +-1,96/sqrt(n)
# entrada: fac (vetor com rho(0), ..., rho(max_lag)), n (tamanho da série), titulo (texto)
# saída: objeto ggplot
plot_fac_empirica <- function(fac,
                              n,
                              titulo){

  max_lag <- length(fac) - 1

  dados <- data.frame(
    lag = 0:max_lag,
    fac = fac
  )

  limite <- 1.96 / sqrt(n)

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
                     yend = fac,
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
      limits = c(-0.5, max_lag + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
    labs(title = titulo,
         x = "Defasagem",
         y = expression(hat(rho)(h))) +
    tema_graficos()

  return(grafico)
}

# FAC empírica (segmentos) contra a FAC teórica do AR(1) (linha e pontos)
# entrada: fac_empirica e fac_teorica (vetores do mesmo tamanho), titulo (texto), n (tamanho da série)
# saída: objeto ggplot
plot_fac_comparacao <- function(fac_empirica,
                                fac_teorica,
                                titulo,
                                n){

  max_lag <- length(fac_empirica) - 1

  dados <- data.frame(
    lag = 0:max_lag,
    empirica = fac_empirica,
    teorica = fac_teorica
  )

  limite <- 1.96 / sqrt(n)

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
                     yend = empirica,
                     color = "FAC Empírica"),
                 linewidth = 1.4) +
    geom_line(data = dados,
              aes(x = lag,
                  y = teorica,
                  color = "FAC Teórica"),
              linewidth = 0.9) +
    geom_point(data = dados,
               aes(x = lag,
                   y = teorica,
                   color = "FAC Teórica"),
               size = 2.4) +
    scale_color_manual(
      values = c(
        "FAC Empírica" = cor_acf,
        "FAC Teórica" = cor_fac_teorica,
        "Limites de Confiança (95%)" = cor_referencia
      ),
      breaks = c("FAC Empírica",
                 "FAC Teórica",
                 "Limites de Confiança (95%)")
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    scale_x_continuous(
      limits = c(-0.5, max_lag + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.04))) +
    labs(title = titulo,
         x = "Defasagem",
         y = expression(rho(h))) +
    tema_graficos()

  return(grafico)
}

# FAC calculada à mão (segmentos) contra a do acf() do R (pontos)
# entrada: fac e fac_r (vetores do mesmo tamanho), titulo (texto)
# saída: objeto ggplot
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

# imprime no console um título de seção
# entrada: titulo (texto)
# saída: nenhuma (só imprime)
imprimir_titulo_console <- function(titulo){
  cat("\n# -- ", titulo, " -- #\n\n", sep = "")
}
