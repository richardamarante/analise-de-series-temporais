############################################################
# visualizacao.R
# Cria os gráficos da série, do ajuste, dos componentes, do diagnóstico e das previsões.
# Dependências: pacote ggplot2.
############################################################

cor_principal <- "#37474F"
cor_referencia <- "#C44E52"
cor_trajetorias <- "#B0B0B0"
cor_acf <- "#4C78A8"
cor_variancia <- "#8E5AD7"
cor_covariancia <- "#3F7F8F"
cor_histograma <- "#90A4AE"
cor_media_movel <- "#E08A1E"

# Define um tema comum para manter a mesma aparência nos gráficos.
# Entrada: nenhuma.
# Saída: objeto de tema do ggplot2.
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

# Ajusta as folgas dos eixos e permite escolher as marcas do eixo x.
# Entrada: grafico é um objeto ggplot, folga_y_inferior define a folga inferior
# com padrão 0.05 e marcas_x recebe um vetor ou waiver().
# Saída: gráfico com 2% de folga à direita, 5% acima e a folga inferior escolhida.
ajustar_eixos <- function(grafico, folga_y_inferior = 0.05, marcas_x = waiver()){
  grafico <- grafico +
    scale_x_continuous(breaks = marcas_x,
                       expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(folga_y_inferior, 0.05)))

  return(grafico)
}

# Salva o gráfico em PNG na pasta graficos/ quando salvar_graficos é verdadeiro.
# Entrada: grafico é um objeto ggplot, nome_arquivo é o caminho sem extensão,
# parametros contém as configurações e fator_altura ajusta a altura.
# Saída: nenhuma, pois cria a pasta e salva o arquivo quando necessário.
# Pré-condição: parametros deve conter salvar_graficos, largura_grafico,
# altura_grafico e dpi_grafico.
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

# Imprime um título para separar as seções da saída no console.
# Entrada: titulo é o texto que será exibido.
# Saída: nenhuma.
imprimir_titulo_console <- function(titulo){
  cat("\n# -- ", titulo, " -- #\n\n", sep = "")
}

# Mostra a série observada e os valores ajustados no mesmo gráfico.
# Entrada: t, serie e ajustada são vetores de tamanho n.
# Saída: objeto ggplot.
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

# Mostra a tendência estimada e acrescenta a verdadeira quando ela está disponível.
# Entrada: t e tendencia_estimada são vetores de tamanho n, enquanto
# tendencia_real pode ser um vetor do mesmo tamanho ou NULL.
# Saída: objeto ggplot.
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

# Mostra a sazonalidade estimada e acrescenta a verdadeira quando ela está disponível.
# Entrada: t e sazonalidade_estimada são vetores de tamanho n, enquanto
# sazonalidade_real pode ser um vetor do mesmo tamanho ou NULL.
# Saída: objeto ggplot.
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

# Compara os efeitos mensais reais com os estimados por Fourier e dummies,
# usando valores centralizados para que as três curvas tenham média zero.
# Entrada: sazonalidade_real, sazonalidade_fourier e sazonalidade_dummies
# são vetores de tamanho s.
# Saída: objeto ggplot.
# Pré-condição: os três vetores devem ter o mesmo tamanho.
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

# Mostra os resíduos ao longo do tempo, usando zero como linha de referência.
# Entrada: t e residuos são vetores de tamanho n.
# Saída: objeto ggplot.
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

# Compara a série observada com a média móvel centrada e a tendência estimada.
# Entrada: t, serie e tendencia são vetores de tamanho n, media_movel
# tem n - 2q elementos e q é o raio da janela.
# Saída: objeto ggplot.
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

# Mostra a decomposição aditiva em quatro painéis, com escalas independentes
# para série, tendência, sazonalidade e resíduos.
# Entrada: t, serie, tendencia, sazonalidade e residuos são vetores de tamanho n.
# Saída: objeto ggplot com folga de 2% à direita.
# Pré-condição: serie = tendencia + sazonalidade + residuos.
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

# Mostra a série observada e as previsões por Fourier e dummies, acrescentando
# o sinal verdadeiro quando ele está disponível.
# Entrada: t e serie são vetores de tamanho n, enquanto t_futuro,
# previsoes_fourier e previsoes_dummies têm tamanho n_futuro, e sinal_futuro
# pode ser um vetor desse mesmo tamanho ou NULL.
# Saída: objeto ggplot.
# Pré-condição: os vetores futuros devem ter o mesmo tamanho.
plot_previsao <- function(t, serie, t_futuro, previsoes_fourier, previsoes_dummies, sinal_futuro = NULL){

  n <- length(serie)

  n_futuro <- length(t_futuro)

  dados <- data.frame(
    tempo = t,
    serie = serie
  )

  # Faz as linhas de Fourier e dummies começarem no último valor observado,
  # para mostrar a continuidade entre a série e as previsões.
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

  # Define as marcas do eixo x entre 1 e n + n_futuro, sem incluir a folga visual
  # acrescentada pelo gráfico.
  grafico <- ajustar_eixos(grafico, marcas_x = pretty(c(1, max(t_futuro))))

  return(grafico)
}

# Mostra a FAC amostral com bandas de ±1,96 / sqrt(n).
# Entrada: serie é um vetor numérico sem NA e titulo é o texto do gráfico.
# Saída: objeto ggplot com folga à direita e acima.
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

# Mostra o histograma da série junto com a densidade normal de variância sigma2.
# Entrada: serie é um vetor numérico, titulo e xlab são textos e sigma2 é um número > 0.
# Saída: objeto ggplot.
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

  # Retira a folga inferior para que as barras do histograma comecem em zero.
  grafico <- ajustar_eixos(grafico, folga_y_inferior = 0)

  return(grafico)
}

# Mostra a divisão cronológica da série e compara as previsões de Fourier e dummies
# nos trechos de validação e teste.
# Entrada: t e serie são vetores de tamanho n, t_validacao e t_teste indicam
# os instantes de cada trecho, e previsoes_fourier e previsoes_dummies
# contêm as previsões da validação seguidas das previsões do teste.
# Saída: objeto ggplot.
plot_divisao_temporal <- function(t, serie, t_validacao, t_teste, previsoes_fourier, previsoes_dummies){

  dados <- data.frame(tempo = t, serie = serie)

  tempo_previsao <- c(t_validacao, t_teste)

  # Identifica os trechos de validação e teste separadamente para que as linhas
  # das previsões não sejam ligadas entre os dois períodos.
  trecho <- c(rep("Validação", length(t_validacao)), rep("Teste", length(t_teste)))

  dados_previsao <- data.frame(
    tempo = c(tempo_previsao, tempo_previsao),
    previsao = c(previsoes_fourier, previsoes_dummies),
    metodo = c(rep("Previsão (Fourier)", length(tempo_previsao)), rep("Previsão (Dummies)", length(tempo_previsao))),
    grupo = c(paste("Fourier", trecho), paste("Dummies", trecho))
  )

  cortes <- data.frame(x = c(t_validacao[1], t_teste[1]) - 0.5)

  rotulos <- data.frame(
    x = c((1 + t_validacao[1]) / 2, (t_validacao[1] + t_teste[1]) / 2, (t_teste[1] + max(t)) / 2),
    y = max(serie) + 0.06 * (max(serie) - min(serie)),
    texto = c("Treino", "Validação", "Teste")
  )

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo, y = serie, color = "Série Observada"),
              linewidth = 0.8) +
    geom_vline(data = cortes,
               aes(xintercept = x, color = "Cortes por Data"),
               linetype = "dotted",
               linewidth = 0.7) +
    geom_line(data = dados_previsao,
              aes(x = tempo, y = previsao, color = metodo, group = grupo),
              linewidth = 1.1) +
    geom_text(data = rotulos,
              aes(x = x, y = y, label = texto),
              color = cor_principal,
              size = 4)

  cores <- c("Série Observada" = cor_principal,
             "Previsão (Fourier)" = cor_acf,
             "Previsão (Dummies)" = cor_media_movel,
             "Cortes por Data" = cor_trajetorias)

  grafico <- grafico +
    scale_color_manual(values = cores, breaks = names(cores)) +
    labs(title = "Previsões na Validação e no Teste",
         x = "Tempo (t)",
         y = expression(X[t])) +
    tema_graficos()

  grafico <- ajustar_eixos(grafico)

  return(grafico)
}

# Compara o RMSE de Fourier e dummies no treino, na validação e no teste.
# Entrada: rmse_fourier e rmse_dummies são vetores com os valores de treino,
# validação e teste, nessa ordem.
# Saída: objeto ggplot de barras.
plot_metricas_divisao <- function(rmse_fourier, rmse_dummies){

  conjuntos <- c("Treino", "Validação", "Teste")

  dados <- data.frame(
    metodo = c(rep("Fourier", 3), rep("Dummies", 3)),
    conjunto = factor(c(conjuntos, conjuntos), levels = conjuntos),
    rmse = c(rmse_fourier, rmse_dummies)
  )

  grafico <- ggplot(dados, aes(x = metodo, y = rmse, fill = conjunto)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = format(round(rmse, 2), nsmall = 2)),
              position = position_dodge(width = 0.8),
              vjust = -0.4,
              size = 3.8,
              color = cor_principal) +
    scale_fill_manual(values = c("Treino" = cor_histograma,
                                 "Validação" = cor_acf,
                                 "Teste" = cor_media_movel)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "RMSE no Treino, na Validação e no Teste",
         x = "Sazonalidade",
         y = "RMSE",
         fill = NULL) +
    tema_graficos()

  return(grafico)
}
