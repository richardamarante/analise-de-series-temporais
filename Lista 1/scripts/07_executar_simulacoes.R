############################################################
# 07_executar_simulacoes.R
# Roda as simulações na ordem (ruído branco, passeio aleatório,
# cilindro e interseção de dois cilindros), imprime os resultados
# e salva os gráficos.
# Dependências: todos os outros arquivos de scripts/.
############################################################

# imprime o título de uma seção no console
# entrada: titulo (texto)
# saída: nenhuma (só imprime)
imprimir_titulo_console <- function(titulo){
  cat("\n# -- ", titulo, " -- #\n\n", sep = "")
}

# imprime as janelas de um cilindro no console, uma por linha
# entrada: tempos (instantes fixados), limite_inferior e limite_superior
#          (vetores do mesmo tamanho de tempos)
# saída: nenhuma (só imprime)
imprimir_janelas <- function(tempos, limite_inferior, limite_superior){

  for(i in 1:length(tempos)){
    cat("t = ", tempos[i],
        ": [", limite_inferior[i], ", ", limite_superior[i], "]\n",
        sep = "")
  }
}

# roda as quatro partes da lista, imprimindo e salvando os gráficos
# entrada: parametros (lista lida de parametros.json: seed, sigma2, n, m, dados
#          dos cilindros e opções de gráfico)
# saída: nenhuma (imprime no console, mostra os gráficos e salva os png)
executar_simulacoes <- function(parametros){
  
  seed <- parametros$seed
  sigma2 <- parametros$sigma2
  n <- parametros$n
  m <- parametros$m
  
  set.seed(seed)
  
  # ruído branco: série, trajetórias, estatísticas e gráficos
  imprimir_titulo_console("RUÍDO BRANCO")
  
  eps <- simular_ruido_branco(n, sigma2)
  traj <- simular_multiplas_trajetorias_ruido(n, m, sigma2)
  media <- calcular_media_trajetoria(traj)
  variancia <- calcular_variancia_trajetoria(traj)
  # covariância entre εₜ e εₜ₋₁, para cada t
  covariancia <- calcular_covariancia_trajetoria(traj)
  
  grafico <- plot_serie_temporal(
    eps,
    "Série Temporal do Ruído Branco",
    expression(epsilon[t]),
    "εₜ"
  )
  print(grafico)
  salvar_grafico(grafico,
                 "ruido_branco/01_serie_temporal_ruido_branco",
                 parametros)
  
  grafico <- plot_acf(
    eps,
    "Função de Autocorrelação do Ruído Branco"
  )
  print(grafico)
  salvar_grafico(grafico,
                 "ruido_branco/02_funcao_autocorrelacao_ruido_branco",
                 parametros)
  
  grafico <- plot_histograma(
    eps,
    "Histograma do Ruído Branco",
    expression(epsilon[t]),
    sigma2
  )
  print(grafico)
  salvar_grafico(grafico,
                 "ruido_branco/03_histograma_ruido_branco",
                 parametros)
  
  grafico <- plot_trajetorias_media(
    traj,
    media,
    "Trajetórias de Ruído Branco e Trajetória Média",
    expression(epsilon[t])
  )
  print(grafico)
  salvar_grafico(grafico,
                 "ruido_branco/04_trajetorias_media_ruido_branco",
                 parametros)
  
  grafico <- plot_variancia(
    variancia = variancia,
    referencia = rep(sigma2, n),
    titulo = "Variância Amostral das Trajetórias de Ruído Branco",
    rotulo_referencia = "σ²",
    ylab = expression(s[t]^2)
  )
  print(grafico)
  salvar_grafico(grafico,
                 "ruido_branco/05_variancia_amostral_trajetorias_ruido_branco",
                 parametros)
  
  # no ruído branco a covariância entre t e t-1 é zero
  referencia_cov_ruido <- rep(0, n)

  grafico <- plot_covariancia(
    covariancia,
    referencia_cov_ruido,
    "Covariância entre εₜ e εₜ₋₁ no Ruído Branco",
    "Cov(εₜ, εₜ₋₁) = 0",
    "Cov(εₜ, εₜ₋₁)"
  )
  print(grafico)
  salvar_grafico(grafico,
                 "ruido_branco/06_covariancia_tempos_consecutivos_ruido_branco",
                 parametros)

  cat("Média da série simulada:", round(calcular_media_serie(eps), 4), "\n")
  cat("Variância da série simulada:", round(calcular_variancia_serie(eps), 4), "\n")
  
  # passeio aleatório: série, incrementos, trajetórias e gráficos
  imprimir_titulo_console("PASSEIO ALEATÓRIO")
  
  pa <- simular_passeio_aleatorio(n, sigma2)
  # incrementos X_t - X_{t-1}, que devem se comportar como ruído branco
  incrementos <- diff(pa)
  traj_pa <- simular_multiplos_passeio_aleatorio(n, m, sigma2)
  media_pa <- calcular_media_trajetoria(traj_pa)
  var_pa <- calcular_variancia_trajetoria(traj_pa)
  
  grafico <- plot_serie_temporal(
    pa,
    "Série Temporal do Passeio Aleatório",
    expression(X[t]),
    "Xₜ"
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/01_serie_temporal_passeio_aleatorio",
                 parametros)
  
  grafico <- plot_serie_temporal(
    incrementos,
    "Série Temporal dos Incrementos do Passeio Aleatório",
    expression(X[t] - X[t-1]),
    "Xₜ - Xₜ₋₁"
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/02_serie_temporal_incrementos_passeio_aleatorio",
                 parametros)
  
  grafico <- plot_acf(
    incrementos,
    "Função de Autocorrelação dos Incrementos do Passeio Aleatório"
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/03_funcao_autocorrelacao_incrementos_passeio_aleatorio",
                 parametros)
  
  grafico <- plot_histograma(
    incrementos,
    "Histograma dos Incrementos do Passeio Aleatório",
    expression(X[t] - X[t-1]),
    sigma2
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/04_histograma_incrementos_passeio_aleatorio",
                 parametros)
  
  grafico <- plot_trajetorias_media(
    traj_pa,
    media_pa,
    "Trajetórias do Passeio Aleatório e Trajetória Média",
    expression(X[t])
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/05_trajetorias_media_passeio_aleatorio",
                 parametros)
  
  grafico <- plot_variancia(
    var_pa,
    referencia = sigma2 * (1:n), # variância teórica do passeio: sigma2 * t
    titulo = "Variância Amostral das Trajetórias do Passeio Aleatório",
    rotulo_referencia = "σ².t",
    ylab = expression(s[t]^2)
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/06_variancia_amostral_trajetorias_passeio_aleatorio",
                 parametros)
  
  cat("Média da trajetória simulada:", round(calcular_media_serie(pa), 4), "\n")
  cat("Variância da trajetória simulada:", round(calcular_variancia_serie(pa), 4), "\n")

  cat("\nMédia dos Incrementos entre Xₜ e Xₜ₋₁:",
      round(calcular_media_serie(incrementos), 4), "\n")
  cat("Variância dos Incrementos entre Xₜ e Xₜ₋₁:",
      round(calcular_variancia_serie(incrementos), 4), "\n")
  
  # cilindro: quantas trajetórias passam por todas as janelas
  imprimir_titulo_console("CILINDRO DE UM PASSEIO ALEATÓRIO")
  
  n_cilindro <- parametros$n_cilindro
  m_cilindro <- parametros$m_cilindro
  tempos_fix <- parametros$tempos_fix
  limite_inferior <- parametros$limite_inferior
  limite_superior <- parametros$limite_superior
  
  trajetorias <- simular_multiplos_passeio_aleatorio(n_cilindro, m_cilindro, sigma2)
  
  pertence_cilindro <- verificar_cilindro(
    trajetorias,
    tempos_fix,
    limite_inferior,
    limite_superior
  )
  
  numero_trajetorias_cilindro <-
    contar_trajetorias_cilindro(pertence_cilindro)
  
  proporcao <- calcular_proporcao_cilindro(
    numero_trajetorias_cilindro,
    m_cilindro
  )
  
  grafico <- plot_cilindro(
    trajetorias,
    pertence_cilindro,
    tempos_fix,
    limite_inferior,
    limite_superior
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/cilindro/01_cilindro_passeio_aleatorio",
                 parametros)
  
  cat("Janelas do cilindro:\n")
  imprimir_janelas(tempos_fix, limite_inferior, limite_superior)

  cat("\nNúmero total de trajetórias:", m_cilindro, "\n")
  cat("Número de trajetórias no cilindro:", numero_trajetorias_cilindro, "\n")
  cat("Proporção de trajetórias no cilindro:", round(proporcao, 4), "\n")
  
  # interseção: trajetórias que estão nos dois cilindros ao mesmo tempo
  imprimir_titulo_console("INTERSEÇÃO DE DOIS CILINDROS")
  
  n_intersecao <- parametros$n_intersecao
  m_intersecao <- parametros$m_intersecao
  
  trajetorias_intersecao <-
    simular_multiplos_passeio_aleatorio(n_intersecao, m_intersecao, sigma2)
  
  tempos1 <- parametros$tempos_cilindro1
  inf1 <- parametros$limite_inferior_cilindro1
  sup1 <- parametros$limite_superior_cilindro1
  
  tempos2 <- parametros$tempos_cilindro2
  inf2 <- parametros$limite_inferior_cilindro2
  sup2 <- parametros$limite_superior_cilindro2
  
  cilindro1 <- verificar_cilindro(
    trajetorias_intersecao,
    tempos1,
    inf1,
    sup1
  )
  
  cilindro2 <- verificar_cilindro(
    trajetorias_intersecao,
    tempos2,
    inf2,
    sup2
  )
  
  # está na interseção quem está nos dois cilindros
  intersecao <- cilindro1 & cilindro2
  
  numero_intersecao <- contar_trajetorias_cilindro(intersecao)
  proporcao_intersecao <-
    calcular_proporcao_cilindro(numero_intersecao, m_intersecao)
  
  grafico <- plot_intersecao_cilindros(
    trajetorias_intersecao,
    intersecao,
    tempos1,
    inf1,
    sup1,
    tempos2,
    inf2,
    sup2
  )
  print(grafico)
  salvar_grafico(grafico,
                 "passeio_aleatorio/cilindro/02_intersecao_dois_cilindros",
                 parametros)
  
  cat("Janelas do cilindro 1:\n")
  imprimir_janelas(tempos1, inf1, sup1)

  cat("\nJanelas do cilindro 2:\n")
  imprimir_janelas(tempos2, inf2, sup2)

  cat("\nNúmero total de trajetórias:", m_intersecao, "\n")
  cat("Número de trajetórias na interseção:", numero_intersecao, "\n")
  cat("Proporção de trajetórias na interseção:",
      round(proporcao_intersecao, 4), "\n")
}
