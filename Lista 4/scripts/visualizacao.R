############################################################
# visualizacao.R
# Gráficos com ggplot2: observada x ajustada, tendência,
# sazonalidade, sazonalidade por mês (Fourier x dummies),
# resíduos, decomposição, média móvel e previsão.
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

# Ajusta as folgas dos eixos: 2% à direita do eixo x, para o último rótulo não sair cortado, e 5% acima e abaixo
# no eixo y, para os picos não encostarem na borda
# Entrada: grafico (objeto ggplot), folga_y_inferior (fração da amplitude abaixo do eixo y, 0.05 por padrão),
#          marcas_x (vetor com as marcas do eixo x; por padrão o ggplot escolhe)
# Saída: o mesmo gráfico com as escalas contínuas ajustadas
ajustar_eixos <- function(grafico, folga_y_inferior = 0.05, marcas_x = waiver()){
  grafico <- grafico +
    scale_x_continuous(breaks = marcas_x,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(folga_y_inferior, 0.05)))

  return(grafico)
}

# Salva o gráfico em png dentro de graficos/, se salvar_graficos for verdadeiro
# Entrada: grafico (objeto ggplot), nome_arquivo (texto, subpasta e nome sem extensão), parametros (lista),
#          fator_altura (número que multiplica altura_grafico, 1 por padrão)
# Saída: nenhuma; cria o arquivo e a pasta se não existirem
# Pré-condição: parametros tem salvar_graficos, largura_grafico, altura_grafico e dpi_grafico
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

# Sazonalidade por mês (1 a s): a real e as estimadas por Fourier e por dummies, cada uma com média zero no ano
# Entrada: sazonalidade_real, sazonalidade_fourier, sazonalidade_dummies (vetores de tamanho s, de calcular_sazonalidade_mensal)
# Saída: objeto ggplot
# Pré-condição: os três vetores têm o mesmo tamanho
plot_sazonalidade_mensal <- function(sazonalidade_real, sazonalidade_fourier, sazonalidade_dummies){

  periodo <- length(sazonalidade_real)

  meses <- 1:periodo

  dados_real <- data.frame(
    mes = meses,
    valor = sazonalidade_real
  )

  dados_estimada <- data.frame(
    mes = rep(meses, times = 2),
    valor = c(sazonalidade_fourier, sazonalidade_dummies),
    metodo = rep(c("Estimada por Fourier", "Estimada por Dummies"), each = periodo)
  )

  rotulos <- c("Sazonalidade Real", "Estimada por Fourier", "Estimada por Dummies")

  grafico <- ggplot() +
    geom_hline(yintercept = 0,
               color = cor_trajetorias,
               linewidth = 0.5) +
    geom_line(data = dados_estimada,
              aes(x = mes,
                  y = valor,
                  color = metodo),
              linewidth = 1) +
    geom_point(data = dados_estimada,
               aes(x = mes,
                   y = valor,
                   color = metodo),
               size = 2.2) +
    geom_line(data = dados_real,
              aes(x = mes,
                  y = valor,
                  color = "Sazonalidade Real"),
              linetype = "dashed",
              linewidth = 0.9) +
    geom_point(data = dados_real,
               aes(x = mes,
                   y = valor,
                   color = "Sazonalidade Real"),
               size = 2.2) +
    scale_color_manual(
      values = c(
        "Sazonalidade Real" = cor_referencia,
        "Estimada por Fourier" = cor_variancia,
        "Estimada por Dummies" = cor_media_movel
      ),
      breaks = rotulos
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    scale_x_continuous(breaks = meses,
                       expand = expansion(add = 0.3)) +
    labs(title = "Sazonalidade por Mês: Fourier e Dummies",
         x = "Mês",
         y = "Efeito sazonal (média zero no ano)") +
    tema_graficos()

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

# Decomposição aditiva em quatro painéis: série, tendência, sazonalidade e resíduos
# Entrada: t, serie, tendencia, sazonalidade, residuos (todos vetores de tamanho n)
# Saída: objeto ggplot com facet_wrap, cada painel com a sua escala em y e folga de 2% à direita do eixo x
# Pré-condição: série = tendência + sazonalidade + resíduos
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
    scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
    labs(title = "Decomposição Aditiva da Série Temporal",
         x = "Tempo (t)",
         y = NULL) +
    tema_graficos() +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(size = 11, face = "bold"))

  return(grafico)
}

# Série observada, previsões por Fourier e por dummies e, quando houver, o sinal verdadeiro nos instantes futuros
# Entrada: t, serie (vetores de tamanho n), t_futuro, previsoes_fourier, previsoes_dummies (vetores de tamanho n_futuro),
#          sinal_futuro (vetor de tamanho n_futuro ou NULL)
# Saída: objeto ggplot
# Pré-condição: t_futuro, previsoes_fourier e previsoes_dummies têm o mesmo tamanho
plot_previsao <- function(t, serie, t_futuro, previsoes_fourier, previsoes_dummies, sinal_futuro = NULL){

  n <- length(serie)

  n_futuro <- length(t_futuro)

  dados <- data.frame(
    tempo = t,
    serie = serie
  )

  # As duas previsões partem do último valor observado
  dados_previsao <- data.frame(
    tempo = rep(c(n, t_futuro), times = 2),
    previsao = c(serie[n], previsoes_fourier, serie[n], previsoes_dummies),
    metodo = rep(c("Previsão (Fourier)", "Previsão (Dummies)"), each = n_futuro + 1)
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
                  color = metodo),
              linewidth = 1.2)

  cores <- c("Série Observada" = cor_principal,
             "Previsão (Fourier)" = cor_acf,
             "Previsão (Dummies)" = cor_media_movel,
             "Início da Previsão" = cor_trajetorias)
  rotulos <- c("Série Observada", "Previsão (Fourier)", "Previsão (Dummies)", "Início da Previsão")

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
    rotulos <- c("Série Observada", "Previsão (Fourier)", "Previsão (Dummies)",
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

  # Marcas do eixo x escolhidas sobre 1 a n + n_futuro, sem a folga, que reduziria o eixo a duas marcas
  grafico <- ajustar_eixos(grafico, marcas_x = pretty(c(1, max(t_futuro))))

  return(grafico)
}

# Função de autocorrelação amostral, com as bandas de +-1,96 / sqrt(n) e folga à direita e acima para o último rótulo e o pico
# Entrada: serie (vetor numérico sem NA), titulo (texto)
# Saída: objeto ggplot
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
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.05))) +
    labs(title = titulo,
         x = "Defasagem",
         y = "ACF") +
    tema_graficos()

  return(grafico)
}

# Histograma com a densidade normal de variância sigma2 por cima, com as barras apoiadas no eixo x
# Entrada: serie (vetor numérico), titulo (texto), xlab (rótulo do eixo x), sigma2 (variância, > 0)
# Saída: objeto ggplot
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

  # Sem folga embaixo, para as barras começarem no zero
  grafico <- ajustar_eixos(grafico, folga_y_inferior = 0)

  return(grafico)
}
