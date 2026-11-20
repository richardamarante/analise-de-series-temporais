############################################################
# Arquivo: plots.R
# Gera gráficos de séries, histogramas, FAC, trajetórias, variância, covariância e cilindros.
# Dependências: pacote ggplot2.
############################################################

# Define a paleta de cores dos gráficos.

cor_principal <- "#37474F"
cor_referencia <- "#C44E52"
cor_trajetorias <- "#B0B0B0"
cor_acf <- "#4C78A8"
cor_envelope <- "#4C78A8"
cor_variancia <- "#8E5AD7"
cor_covariancia <- "#3F7F8F"
cor_histograma <- "#90A4AE"
cor_janela_c1 <- "#000000"
cor_janela_c2 <- "#5B8DB8"
cor_cilindro <- "#E15759"

# Define o tema usado em todos os gráficos.
# Entrada: nenhuma.
# Saída: objeto theme do ggplot2.
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

# Ajusta os eixos com 2% de folga à direita no x e folga configurável no y.
# Entrada: grafico (objeto ggplot) e folga_y_inferior (fração da amplitude, padrão 0.05).
# Saída: objeto ggplot com os eixos ajustados.
ajustar_eixos <- function(grafico, folga_y_inferior = 0.05){
  grafico <- grafico +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
    scale_y_continuous(expand = expansion(mult = c(folga_y_inferior, 0.05)))
  
  return(grafico)
}

# Salva um gráfico em PNG na pasta graficos/, se parametros$salvar_graficos for TRUE.
# Entrada: grafico (objeto ggplot), nome_arquivo (caminho sem extensão) e parametros.
# Saída: arquivo PNG com largura, altura e dpi definidos em parametros.
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

# Desenha uma série temporal com a linha de referência em mu = 0.
# Entrada: serie (vetor numérico), titulo, ylab e rotulo_serie (textos).
# Saída: objeto ggplot.
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

# Desenha o histograma com a densidade normal teórica N(0, sigma2).
# Entrada: serie (vetor numérico), titulo, xlab e sigma2 (variância teórica).
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

  # Remove a folga inferior para as barras começarem no zero.
  grafico <- ajustar_eixos(grafico, folga_y_inferior = 0)

  return(grafico)
}

# Desenha a FAC amostral com limites de 95% em +-1,96/sqrt(n).
# Entrada: serie (vetor numérico) e titulo (texto).
# Saída: objeto ggplot.
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

# Converte a matriz de trajetórias para o formato longo do ggplot2.
# Entrada: trajetorias (matriz n x m, uma coluna por trajetória).
# Saída: data.frame com n*m linhas e colunas tempo, valor e trajetoria (fator).
matriz_para_df <- function(trajetorias){
  
  n <- nrow(trajetorias)
  m <- ncol(trajetorias)
  
  dados <- data.frame(
    tempo = rep(1:n, times = m),
    valor = as.vector(trajetorias),
    trajetoria = factor(rep(1:m, each = n))
  )
  
  return(dados)
}

# Desenha todas as trajetórias com a linha de referência em mu = 0.
# Entrada: trajetorias (matriz n x m), titulo e ylab (textos).
# Saída: objeto ggplot.
plot_multiplas_trajetorias <- function(trajetorias, titulo,
                                       ylab = expression(X[t])){
  
  dados <- matriz_para_df(trajetorias)
  
  media_zero <- data.frame(y = 0)

  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = valor,
                  group = trajetoria,
                  color = "Trajetórias"),
              linewidth = 0.55,
              alpha = 0.7) +
    geom_hline(data = media_zero,
               aes(yintercept = y,
                   color = "μ = 0"),
               linetype = "dashed",
               linewidth = 0.7) +
    scale_color_manual(values = c(
      "Trajetórias" = cor_trajetorias,
      "μ = 0" = cor_referencia
    ),
    breaks = c("Trajetórias", "μ = 0")) +
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

# Desenha as trajetórias, a média amostral, a referência mu = 0 e o envelope opcional.
# Entrada: trajetorias (matriz n x m), media (vetor n), titulo, ylab e rotulo_envelope (textos).
# Entrada: envelope (vetor n com a meia largura do envelope ou NULL).
# Saída: objeto ggplot.
plot_trajetorias_media <- function(trajetorias, media, titulo,
                                   ylab = expression(X[t]),
                                   envelope = NULL,
                                   rotulo_envelope = "Envelope"){
  
  n <- nrow(trajetorias)
  dados <- matriz_para_df(trajetorias)
  
  dados_media <- data.frame(
    tempo = 1:n,
    media = media
  )
  
  media_zero <- data.frame(y = 0)
  
  grafico <- ggplot() +
    geom_line(data = dados,
              aes(x = tempo,
                  y = valor,
                  group = trajetoria,
                  color = "Trajetórias"),
              linewidth = 0.55,
              alpha = 0.65) +
    geom_line(data = dados_media,
              aes(x = tempo,
                  y = media,
                  color = "Média Amostral"),
              linewidth = 1.3) +
    geom_hline(data = media_zero,
               aes(yintercept = y,
                   color = "μ = 0"),
               linetype = "dashed",
               linewidth = 0.7)

  cores <- c(
    "Média Amostral" = "black",
    "μ = 0" = cor_referencia,
    "Trajetórias" = cor_trajetorias
  )
  ordem <- c("Média Amostral", "μ = 0", "Trajetórias")

  # Usa a mesma entrada na legenda para os dois limites do envelope.
  if(!is.null(envelope)){
    dados_envelope <- data.frame(
      tempo = 1:n,
      superior = envelope,
      inferior = -envelope
    )

    grafico <- grafico +
      geom_line(data = dados_envelope,
                aes(x = tempo,
                    y = superior,
                    color = rotulo_envelope),
                linetype = "dashed",
                linewidth = 0.7) +
      geom_line(data = dados_envelope,
                aes(x = tempo,
                    y = inferior,
                    color = rotulo_envelope),
                linetype = "dashed",
                linewidth = 0.7)

    cores <- c(cores, setNames(cor_envelope, rotulo_envelope))
    ordem <- c("Média Amostral", "μ = 0", rotulo_envelope, "Trajetórias")
  }

  grafico <- grafico +
    scale_color_manual(values = cores, breaks = ordem) +
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

# Desenha a variância amostral em cada instante com uma referência opcional.
# Entrada: variancia (vetor n), referencia (vetor n ou NULL), titulo, rotulo_referencia e ylab.
# Saída: objeto ggplot.
plot_variancia <- function(variancia, referencia = NULL, titulo,
                           rotulo_referencia = "Referência",
                           ylab = expression(s[t]^2)){
  
  n <- length(variancia)
  
  dados <- data.frame(
    tempo = 1:n,
    variancia = variancia
  )
  
  grafico <- ggplot(dados, aes(x = tempo, y = variancia)) +
    geom_line(aes(color = "Variância Amostral"),
              linewidth = 1)
  
  if(!is.null(referencia)){
    dados_referencia <- data.frame(
      tempo = 1:n,
      referencia = referencia
    )
    
    grafico <- grafico +
      geom_line(data = dados_referencia,
                aes(x = tempo,
                    y = referencia,
                    color = "Referência"),
                inherit.aes = FALSE,
                linetype = "dashed",
                linewidth = 0.7)
  }
  
  cores <- c(
    "Variância Amostral" = cor_variancia,
    "Referência" = cor_referencia
  )
  
  if(!is.null(referencia)){
    grafico <- grafico +
      scale_color_manual(values = cores,
                         breaks = c("Referência", "Variância Amostral"),
                         labels = c(rotulo_referencia,
                                    "Variância Amostral"))
  } else {
    grafico <- grafico +
      scale_color_manual(values = cores["Variância Amostral"],
                         breaks = "Variância Amostral")
  }
  
  grafico <- grafico +
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

# Desenha a covariância amostral entre t e t-1 com uma curva de referência.
# Entrada: covariancia e referencia (vetores n), titulo, rotulo_referencia e ylab.
# Saída: objeto ggplot, sem o primeiro instante.
plot_covariancia <- function(covariancia,
                             referencia,
                             titulo,
                             rotulo_referencia,
                             ylab = "Covariância"){

  n <- length(covariancia)

  dados <- data.frame(
    tempo = 2:n,
    covariancia = covariancia[2:n],
    referencia = referencia[2:n]
  )

  grafico <- ggplot(dados, aes(x = tempo)) +
    geom_line(aes(y = covariancia,
                  color = "Covariância Amostral"),
              linewidth = 1) +
    geom_line(aes(y = referencia,
                  color = "Referência"),
              linetype = "dashed",
              linewidth = 0.7) +
    scale_color_manual(
      values = c(
        "Covariância Amostral" = cor_covariancia,
        "Referência" = cor_referencia
      ),
      breaks = c("Referência", "Covariância Amostral"),
      labels = c(rotulo_referencia, "Covariância Amostral")
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

# Desenha as trajetórias com as pertencentes ao cilindro em destaque e suas janelas.
# Entrada: trajetorias (matriz n x m), pertence_cilindro (vetor lógico de tamanho m).
# Entrada: tempos_fix, limite_inferior e limite_superior (vetores das janelas).
# Saída: objeto ggplot.
plot_cilindro <- function(trajetorias,
                          pertence_cilindro,
                          tempos_fix,
                          limite_inferior,
                          limite_superior){
  
  n <- nrow(trajetorias)
  
  dados <- matriz_para_df(trajetorias)
  dados$dentro <- rep(pertence_cilindro, each = n)
  
  janelas <- data.frame(
    tempo = tempos_fix,
    limite_inf = limite_inferior,
    limite_sup = limite_superior
  )
  
  grafico <- ggplot()
  
  grafico <- grafico +
    geom_line(data = subset(dados, !dentro),
              aes(x = tempo,
                  y = valor,
                  group = trajetoria,
                  color = "Trajetórias Livres"),
              alpha = 0.08,
              linewidth = 0.4)
  
  grafico <- grafico +
    geom_line(data = subset(dados, dentro),
              aes(x = tempo,
                  y = valor,
                  group = trajetoria,
                  color = "Interseção"),
              alpha = 0.8,
              linewidth = 0.8)
  
  grafico <- grafico +
    geom_segment(data = janelas,
                 aes(x = tempo,
                     xend = tempo,
                     y = limite_inf,
                     yend = limite_sup,
                     color = "Janelas do Cilindro"),
                 linewidth = 3.5)
  
  grafico <- grafico +
    scale_color_manual(values = c(
      "Janelas do Cilindro" = cor_janela_c1,
      "Interseção" = cor_cilindro,
      "Trajetórias Livres" = cor_trajetorias
    ),
    breaks = c("Janelas do Cilindro", "Interseção", "Trajetórias Livres")) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.4, alpha = 1)
    )) +
    labs(title = "Cilindro de um Passeio Aleatório",
         x = "Tempo (t)",
         y = expression(X[t])) +
    tema_graficos()
  
  grafico <- ajustar_eixos(grafico)
  
  return(grafico)
}

# Desenha as trajetórias com a interseção em destaque e as janelas dos dois cilindros lado a lado.
# Entrada: trajetorias (matriz n x m), intersecao (vetor lógico de tamanho m).
# Entrada: tempos1, inf1, sup1, tempos2, inf2 e sup2 (vetores das janelas).
# Saída: objeto ggplot.
plot_intersecao_cilindros <- function(trajetorias,
                                      intersecao,
                                      tempos1,
                                      inf1,
                                      sup1,
                                      tempos2,
                                      inf2,
                                      sup2){
  
  n <- nrow(trajetorias)
  
  dados <- matriz_para_df(trajetorias)
  dados$intersecao <- rep(intersecao, each = n)
  
  janelas1 <- data.frame(
    tempo = tempos1,
    limite_inf = inf1,
    limite_sup = sup1
  )
  
  janelas2 <- data.frame(
    tempo = tempos2,
    limite_inf = inf2,
    limite_sup = sup2
  )
  
  grafico <- ggplot() +
    geom_line(data = subset(dados, !intersecao),
              aes(x = tempo,
                  y = valor,
                  group = trajetoria,
                  color = "Trajetórias Livres"),
              alpha = 0.06,
              linewidth = 0.35) +
    geom_line(data = subset(dados, intersecao),
              aes(x = tempo,
                  y = valor,
                  group = trajetoria,
                  color = "Interseção"),
              alpha = 0.65,
              linewidth = 0.65) +
    geom_segment(data = janelas1,
                 aes(x = tempo - 0.4,
                     xend = tempo - 0.4,
                     y = limite_inf,
                     yend = limite_sup,
                     color = "Janelas do Cilindro 1"),
                 linewidth = 3.5) +
    geom_segment(data = janelas2,
                 aes(x = tempo + 0.4,
                     xend = tempo + 0.4,
                     y = limite_inf,
                     yend = limite_sup,
                     color = "Janelas do Cilindro 2"),
                 linewidth = 3.5) +
    scale_color_manual(values = c(
      "Janelas do Cilindro 1" = cor_janela_c1,
      "Janelas do Cilindro 2" = cor_janela_c2,
      "Interseção" = cor_cilindro,
      "Trajetórias Livres" = cor_trajetorias
    ),
    breaks = c("Janelas do Cilindro 1", "Janelas do Cilindro 2",
               "Interseção", "Trajetórias Livres")) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.4, alpha = 1)
    )) +
    labs(title = "Interseção de Dois Cilindros",
         x = "Tempo (t)",
         y = expression(X[t])) +
    tema_graficos()
  
  grafico <- ajustar_eixos(grafico)
  
  return(grafico)
}
