############################################################
# 04_visualizacao.R
# Gráficos com ggplot2: observada x ajustada, tendência,
# sazonalidade, resíduos, decomposição, média móvel e previsão.
# Dependências: pacote ggplot2.
############################################################

# cores dos gráficos
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
cor_media_movel <- "#E08A1E"

# tema comum a todos os gráficos
# entrada: nenhuma
# saída: objeto de tema do ggplot2
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

# tira o espaço extra dos eixos
# entrada: grafico (objeto ggplot)
# saída: objeto ggplot com as escalas ajustadas
ajustar_eixos <- function(grafico){
  grafico <- grafico +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  return(grafico)
}

# salva o gráfico em png dentro de graficos/, se salvar_graficos for verdadeiro
# entrada: grafico (objeto ggplot), nome_arquivo (texto, subpasta e nome sem extensão), parametros (lista)
# saída: nenhuma; cria o arquivo e a pasta se não existirem
# pré-condição: parametros tem salvar_graficos, largura_grafico, altura_grafico e dpi_grafico
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

# imprime um título de seção no console
# entrada: titulo (texto)
# saída: nenhuma
imprimir_titulo_console <- function(titulo){
  cat("\n# -- ", titulo, " -- #\n\n", sep = "")
}

# série observada e série ajustada pelo modelo
# entrada: t (vetor de tamanho n), serie (vetor de tamanho n), ajustada (vetor de tamanho n)
# saída: objeto ggplot
plot_serie_ajustada <- function(t, serie, ajustada){

  dados <- data.frame(
    tempo = t,
    serie = serie,
    ajustada = ajustada
  )

  grafico <- ggplot(dados, aes(x = tempo)) +
    geom_line(aes(y = serie,
                  color = "Série Observada"),
              linewidth = 0.9) +
    geom_line(aes(y = ajustada,
                  color = "Série Ajustada"),
              linetype = "dashed",
              linewidth = 0.9) +
    scale_color_manual(
      values = c(
        "Série Observada" = cor_principal,
        "Série Ajustada" = cor_referencia
      ),
      breaks = c("Série Observada", "Série Ajustada")
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Série Observada e Ajustada",
         x = "Tempo (t)",
         y = expression(X[t])) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# tendência estimada, com a tendência verdadeira por cima quando houver
# entrada: t (vetor de tamanho n), tendencia_estimada (vetor de tamanho n), tendencia_real (vetor de tamanho n ou NULL)
# saída: objeto ggplot
plot_tendencia <- function(t, tendencia_estimada, tendencia_real = NULL){

  dados <- data.frame(
    tempo = t,
    estimada = tendencia_estimada
  )

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = estimada,
                  color = "Tendência Estimada"),
              linewidth = 1)

  cores <- c("Tendência Estimada" = cor_covariancia)
  rotulos <- c("Tendência Estimada")

  if(!is.null(tendencia_real)){

    dados_real <- data.frame(
      tempo = t,
      real = tendencia_real
    )

    grafico <- grafico +
      geom_line(data = dados_real,
                aes(x = tempo,
                    y = real,
                    color = "Tendência Real"),
                linetype = "dashed",
                linewidth = 0.8)

    cores <- c(cores, "Tendência Real" = cor_referencia)
    rotulos <- c("Tendência Real", rotulos)
  }

  grafico <- grafico +
    scale_color_manual(values = cores,
                       breaks = rotulos) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Tendência Estimada por OLS",
         x = "Tempo (t)",
         y = "Tendência") +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# sazonalidade estimada, com a verdadeira por cima quando houver
# entrada: t (vetor de tamanho n), sazonalidade_estimada (vetor de tamanho n), sazonalidade_real (vetor de tamanho n ou NULL)
# saída: objeto ggplot
plot_sazonalidade <- function(t, sazonalidade_estimada, sazonalidade_real = NULL){

  dados <- data.frame(
    tempo = t,
    estimada = sazonalidade_estimada
  )

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = estimada,
                  color = "Sazonalidade Estimada"),
              linewidth = 1)

  cores <- c("Sazonalidade Estimada" = cor_variancia)
  rotulos <- c("Sazonalidade Estimada")

  if(!is.null(sazonalidade_real)){

    dados_real <- data.frame(
      tempo = t,
      real = sazonalidade_real
    )

    grafico <- grafico +
      geom_line(data = dados_real,
                aes(x = tempo,
                    y = real,
                    color = "Sazonalidade Real"),
                linetype = "dashed",
                linewidth = 0.8)

    cores <- c(cores, "Sazonalidade Real" = cor_referencia)
    rotulos <- c("Sazonalidade Real", rotulos)
  }

  grafico <- grafico +
    scale_color_manual(values = cores,
                       breaks = rotulos) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Sazonalidade Estimada por OLS",
         x = "Tempo (t)",
         y = "Sazonalidade") +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# série temporal dos resíduos, com a linha em zero
# entrada: t (vetor de tamanho n), residuos (vetor de tamanho n)
# saída: objeto ggplot
plot_residuos <- function(t, residuos){

  dados <- data.frame(
    tempo = t,
    residuos = residuos
  )

  zero <- data.frame(y = 0)

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = residuos,
                  color = "Resíduos"),
              linewidth = 0.9) +
    geom_hline(data = zero,
               aes(yintercept = y,
                   color = "μ = 0"),
               linetype = "dashed",
               linewidth = 0.7) +
    scale_color_manual(
      values = c(
        "Resíduos" = cor_principal,
        "μ = 0" = cor_referencia
      ),
      breaks = c("Resíduos", "μ = 0")
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Resíduos do Modelo Ajustado",
         x = "Tempo (t)",
         y = expression(hat(epsilon)[t])) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# série, média móvel centrada e tendência estimada
# entrada: t (vetor de tamanho n), serie (vetor de tamanho n), media_movel (vetor de tamanho n - 2q),
#          tendencia (vetor de tamanho n), q (inteiro)
# saída: objeto ggplot
plot_serie_media_movel <- function(t, serie, media_movel, tendencia, q){

  n <- length(serie)

  rotulo_media_movel <- paste0("Média Móvel (q = ", q, ")")

  dados <- data.frame(
    tempo = t,
    serie = serie,
    tendencia = tendencia
  )

  dados_media <- data.frame(
    tempo = (q+1):(n-q),
    media_movel = media_movel
  )

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = serie,
                  color = "Série Observada"),
              linewidth = 0.8) +
    geom_line(data = dados_media,
              aes(x = tempo,
                  y = media_movel,
                  color = rotulo_media_movel),
              linewidth = 1.2) +
    geom_line(data = dados,
              aes(x = tempo,
                  y = tendencia,
                  color = "Tendência Estimada por OLS"),
              linewidth = 1.2) +
    scale_color_manual(
      values = setNames(c(cor_principal, cor_media_movel, cor_covariancia),
                        c("Série Observada", rotulo_media_movel,
                          "Tendência Estimada por OLS")),
      breaks = c("Série Observada", rotulo_media_movel,
                 "Tendência Estimada por OLS")
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Série Temporal, Média Móvel e Tendência",
         x = "Tempo (t)",
         y = expression(X[t])) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# decomposição aditiva em quatro painéis: série, tendência, sazonalidade e resíduos
# entrada: t, serie, tendencia, sazonalidade, residuos (todos vetores de tamanho n)
# saída: objeto ggplot com facet_wrap, cada painel com a sua escala em y
# pré-condição: série = tendência + sazonalidade + resíduos
plot_decomposicao <- function(t, serie, tendencia, sazonalidade, residuos){

  componentes <- c("Série Observada",
                   "Tendência Estimada",
                   "Sazonalidade Estimada",
                   "Resíduos")

  n <- length(t)

  dados <- data.frame(
    tempo = rep(t, times = 4),
    valor = c(serie, tendencia, sazonalidade, residuos),
    componente = factor(rep(componentes, each = n),
                        levels = componentes)
  )

  grafico <- ggplot(dados,
                    aes(x = tempo,
                        y = valor,
                        color = componente)) +
    geom_line(linewidth = 0.9) +
    facet_wrap(~ componente, ncol = 1, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Série Observada" = cor_principal,
        "Tendência Estimada" = cor_covariancia,
        "Sazonalidade Estimada" = cor_variancia,
        "Resíduos" = cor_referencia
      ),
      breaks = componentes
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(title = "Decomposição Aditiva da Série Temporal",
         x = "Tempo (t)",
         y = NULL) +
    tema_graficos() +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = 11, face = "bold"))

  return(grafico)
}

# série observada, previsões e, quando houver, o sinal verdadeiro nos instantes futuros
# entrada: t, serie (vetores de tamanho n), t_futuro, previsoes (vetores de tamanho n_futuro), sinal_futuro (vetor ou NULL)
# saída: objeto ggplot
plot_previsao <- function(t, serie, t_futuro, previsoes, sinal_futuro = NULL){

  n <- length(serie)

  dados <- data.frame(
    tempo = t,
    serie = serie
  )

  dados_previsao <- data.frame(
    tempo = c(n, t_futuro),
    previsao = c(serie[n], previsoes)
  )

  inicio <- data.frame(x = n)

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = serie,
                  color = "Série Observada"),
              linewidth = 0.8) +
    geom_vline(data = inicio,
               aes(xintercept = x,
                   color = "Início da Previsão"),
               linetype = "dotted",
               linewidth = 0.7) +
    geom_line(data = dados_previsao,
              aes(x = tempo,
                  y = previsao,
                  color = "Previsão"),
              linewidth = 1.2)

  cores <- c("Série Observada" = cor_principal,
             "Previsão" = cor_acf,
             "Início da Previsão" = cor_trajetorias)
  rotulos <- c("Série Observada", "Previsão", "Início da Previsão")

  if(!is.null(sinal_futuro)){

    dados_sinal <- data.frame(
      tempo = t_futuro,
      sinal = sinal_futuro
    )

    grafico <- grafico +
      geom_line(data = dados_sinal,
                aes(x = tempo,
                    y = sinal,
                    color = "Tendência + Sazonalidade Reais"),
                linetype = "dashed",
                linewidth = 0.9)

    cores <- c(cores, "Tendência + Sazonalidade Reais" = cor_referencia)
    rotulos <- c("Série Observada", "Previsão",
                 "Tendência + Sazonalidade Reais", "Início da Previsão")
  }

  grafico <- grafico +
    scale_color_manual(values = cores,
                       breaks = rotulos) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Previsão Fora da Amostra",
         x = "Tempo (t)",
         y = expression(X[t])) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# função de autocorrelação amostral, com as bandas de +-1,96 / sqrt(n)
# entrada: serie (vetor numérico sem NA), titulo (texto)
# saída: objeto ggplot
plot_acf <- function(serie, titulo){

  resultado <- acf(serie, plot = FALSE)

  dados <- data.frame(
    lag = as.numeric(resultado$lag),
    acf = as.numeric(resultado$acf)
  )

  limite <- 1.96 / sqrt(length(serie))
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
                     color = "ACF"),
                 linewidth = 0.8) +
    scale_color_manual(
      values = c(
        "ACF" = cor_principal,
        "Limites de Confiança (95%)" = cor_acf
      ),
      breaks = c("ACF", "Limites de Confiança (95%)")
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    scale_x_continuous(
      limits = c(-0.5, maior_lag),
      expand = c(0, 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0))) +
    labs(title = titulo,
         x = "Defasagem",
         y = "ACF") +
    tema_graficos()

  return(grafico)
}

# histograma com a densidade normal de variância sigma2 por cima
# entrada: serie (vetor numérico), titulo (texto), xlab (rótulo do eixo x), sigma2 (variância, > 0)
# saída: objeto ggplot
plot_histograma <- function(serie, titulo, xlab, sigma2){

  dados <- data.frame(valor = serie)

  valores_x <- seq(min(serie),
                   max(serie),
                   length.out = 200)

  rotulo_teorico <- paste0("Distribuição Teórica ~ Normal(0, ", sigma2, ")")

  dados_teoricos <- data.frame(
    x = valores_x,
    densidade = dnorm(valores_x,
                      mean = 0,
                      sd = sqrt(sigma2)),
    legenda = rotulo_teorico
  )

  grafico <- ggplot(dados, aes(x = valor)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30,
                   fill = cor_histograma,
                   color = cor_principal,
                   linewidth = 0.4) +
    geom_line(data = dados_teoricos,
              aes(x = x,
                  y = densidade,
                  color = legenda),
              inherit.aes = FALSE,
              linewidth = 1.1) +
    scale_color_manual(values = setNames(c(cor_referencia), rotulo_teorico)) +
    labs(title = titulo,
         x = xlab,
         y = "Densidade") +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}
