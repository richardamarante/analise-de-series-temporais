############################################################
# ex4_eficiencia.R
# Exercício 4.3: compara os loops de X'X, X'y e X·beta com as
# operações matriciais do R (crossprod e o operador %*%),
# confere o OLS com o lm() e desenha o tempo em função de n.
# Rodar com a pasta da lista como diretório de trabalho.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/config.R")
source("scripts/dados.R")
source("scripts/processamento.R")
source("scripts/visualizacao.R")

# X'X com crossprod, sem loops
# entrada: X (matriz n x p)
# saída: matriz p x p, igual à de calcular_XtX
calcular_XtX_vetorizada <- function(X){
  resultado <- crossprod(X)
  return(resultado)
}

# X'y com crossprod, sem loops
# entrada: X (matriz n x p), y (vetor de tamanho n)
# saída: vetor de tamanho p, igual ao de calcular_Xty
calcular_Xty_vetorizada <- function(X, y){
  resultado <- as.numeric(crossprod(X, y))
  return(resultado)
}

# X'X com a transposta montada e o operador %*%: t(X) %*% X, sem loops
# Entrada: X (matriz n x p)
# Saída: matriz p x p, igual à de calcular_XtX
calcular_XtX_transposta <- function(X){
  resultado <- t(X) %*% X
  return(resultado)
}

# X'y com a transposta montada e o operador %*%: t(X) %*% y, sem loops
# Entrada: X (matriz n x p), y (vetor de tamanho n)
# Saída: vetor de tamanho p, igual ao de calcular_Xty
calcular_Xty_transposta <- function(X, y){
  resultado <- as.numeric(t(X) %*% y)
  return(resultado)
}

# X beta com o operador %*%, sem loops
# entrada: X (matriz n x p), beta (vetor de tamanho p)
# saída: vetor de tamanho n, igual ao de multiplicar_matriz_vetor
multiplicar_matriz_vetor_vetorizada <- function(X, beta){
  resultado <- as.numeric(X %*% beta)
  return(resultado)
}

# OLS com as versões vetorizadas de X'X e X'y: beta_hat = solve(X'X, X'y)
# entrada: X (matriz n x p), y (vetor de tamanho n)
# saída: vetor beta_hat de tamanho p
# pré-condição: X'X invertível
ajustar_ols_vetorizada <- function(X, y){

  XtX <- calcular_XtX_vetorizada(X)

  Xty <- calcular_Xty_vetorizada(X, y)

  beta_hat <- as.numeric(solve(XtX, Xty))

  return(beta_hat)
}

# OLS com t(X) %*% X e t(X) %*% y: beta_hat = solve(X'X, X'y)
# Entrada: X (matriz n x p), y (vetor de tamanho n)
# Saída: vetor beta_hat de tamanho p
# Pré-condição: X'X invertível
ajustar_ols_transposta <- function(X, y){

  XtX <- calcular_XtX_transposta(X)

  Xty <- calcular_Xty_transposta(X, y)

  beta_hat <- as.numeric(solve(XtX, Xty))

  return(beta_hat)
}

# Maior diferença relativa entre dois resultados: max |a - b| / max |a|
# Entrada: a (resultado de referência, vetor ou matriz), b (resultado comparado, do mesmo tamanho)
# Saída: número; da ordem de 1e-16 quer dizer que as duas contas só diferem no arredondamento
# Pré-condição: a tem algum elemento diferente de zero
calcular_diferenca_relativa <- function(a, b){
  diferenca <- max(abs(a - b)) / max(abs(a))
  return(diferenca)
}

# Mede o tempo de X'X, X'y e X beta com loops, com crossprod e com o operador %*% e compara os resultados com os dos loops
# Entrada: X (matriz n x p), y (vetor de tamanho n), beta (vetor de tamanho p), repeticoes (inteiro positivo)
# Saída: data.frame com uma linha por operação e versão: n, operacao, versao, expressao, tempo (s, soma das repetições),
#        igual (all.equal com os loops) e diferenca_relativa (em relação aos loops)
# Pré-condição: nrow(X) == length(y) e ncol(X) == length(beta)
medir_tempos <- function(X, y, beta, repeticoes){

  # X'X
  tempo_loop_XtX <- system.time({
    for(r in 1:repeticoes){
      XtX_loop <- calcular_XtX(X)
    }
  })[["elapsed"]]

  tempo_cross_XtX <- system.time({
    for(r in 1:repeticoes){
      XtX_cross <- calcular_XtX_vetorizada(X)
    }
  })[["elapsed"]]

  tempo_transposta_XtX <- system.time({
    for(r in 1:repeticoes){
      XtX_transposta <- calcular_XtX_transposta(X)
    }
  })[["elapsed"]]

  # X'y
  tempo_loop_Xty <- system.time({
    for(r in 1:repeticoes){
      Xty_loop <- calcular_Xty(X, y)
    }
  })[["elapsed"]]

  tempo_cross_Xty <- system.time({
    for(r in 1:repeticoes){
      Xty_cross <- calcular_Xty_vetorizada(X, y)
    }
  })[["elapsed"]]

  tempo_transposta_Xty <- system.time({
    for(r in 1:repeticoes){
      Xty_transposta <- calcular_Xty_transposta(X, y)
    }
  })[["elapsed"]]

  # X beta
  tempo_loop_mv <- system.time({
    for(r in 1:repeticoes){
      mv_loop <- multiplicar_matriz_vetor(X, beta)
    }
  })[["elapsed"]]

  tempo_vet_mv <- system.time({
    for(r in 1:repeticoes){
      mv_vet <- multiplicar_matriz_vetor_vetorizada(X, beta)
    }
  })[["elapsed"]]

  tempos <- data.frame(
    n = nrow(X),
    operacao = c("X'X", "X'X", "X'X",
                 "X'y", "X'y", "X'y",
                 "X·beta", "X·beta"),
    versao = c("Loops", "crossprod()", "Operador %*%",
               "Loops", "crossprod()", "Operador %*%",
               "Loops", "Operador %*%"),
    expressao = c("loops", "crossprod(X)", "t(X) %*% X",
                  "loops", "crossprod(X, y)", "t(X) %*% y",
                  "loops", "X %*% beta"),
    tempo = c(tempo_loop_XtX, tempo_cross_XtX, tempo_transposta_XtX,
              tempo_loop_Xty, tempo_cross_Xty, tempo_transposta_Xty,
              tempo_loop_mv, tempo_vet_mv),
    igual = c(TRUE,
              isTRUE(all.equal(XtX_loop, XtX_cross)),
              isTRUE(all.equal(XtX_loop, XtX_transposta)),
              TRUE,
              isTRUE(all.equal(Xty_loop, Xty_cross)),
              isTRUE(all.equal(Xty_loop, Xty_transposta)),
              TRUE,
              isTRUE(all.equal(mv_loop, mv_vet))),
    diferenca_relativa = c(0,
                           calcular_diferenca_relativa(XtX_loop, XtX_cross),
                           calcular_diferenca_relativa(XtX_loop, XtX_transposta),
                           0,
                           calcular_diferenca_relativa(Xty_loop, Xty_cross),
                           calcular_diferenca_relativa(Xty_loop, Xty_transposta),
                           0,
                           calcular_diferenca_relativa(mv_loop, mv_vet))
  )

  return(tempos)
}

# Compara loops e operações vetorizadas: exatidão do OLS com o n da série e tempo de X'X, X'y e X beta para vários n
# Entrada: parametros (lista com seed, n_anos, s, K, grau_tendencia, beta_tendencia, alpha, beta_h, sigma,
#          n_eficiencia, n_grafico_eficiencia e repeticoes_eficiencia)
# Saída: data.frame de medir_tempos com todos os tamanhos medidos; imprime a exatidão e os tempos com n_eficiencia
# Pré-condição: parametros lido por ler_parametros
comparar_eficiencia <- function(parametros){

  fixar_semente(parametros)

  K <- parametros$K
  repeticoes <- parametros$repeticoes_eficiencia

  beta_sazonal <- criar_beta_sazonal(parametros$alpha, parametros$beta_h, K)

  # Exatidão: OLS completo com o n da série, contra o lm()
  t <- criar_tempo(parametros)

  X_tendencia <- criar_matriz_tendencia(t, parametros$grau_tendencia)
  X_sazonal <- criar_matriz_fourier(t, parametros$s, K)
  X <- cbind(X_tendencia, X_sazonal)

  y <- simular_serie(parametros$beta_tendencia, X_tendencia, X_sazonal,
                     beta_sazonal, parametros$sigma)

  beta_loops <- ajustar_ols(X, y)
  beta_vetorizada <- ajustar_ols_vetorizada(X, y)
  beta_transposta <- ajustar_ols_transposta(X, y)
  beta_lm <- as.numeric(coef(lm(y ~ X - 1)))

  cat("Tamanho da série:", length(t), "\n")
  cat("Coeficientes iguais (loops e crossprod):", isTRUE(all.equal(beta_loops, beta_vetorizada)), "\n")
  cat("Coeficientes iguais (loops e t(X) %*% X):", isTRUE(all.equal(beta_loops, beta_transposta)), "\n")
  cat("Coeficientes iguais ao lm() (loops):", isTRUE(all.equal(beta_loops, beta_lm)), "\n")
  cat("Coeficientes iguais ao lm() (crossprod):", isTRUE(all.equal(beta_vetorizada, beta_lm)), "\n")
  cat("Coeficientes iguais ao lm() (t(X) %*% X):", isTRUE(all.equal(beta_transposta, beta_lm)), "\n")

  # Tempo: X'X, X'y e X beta (sem o solve) em cada tamanho do gráfico e em n_eficiencia, cada um medido uma vez
  n_grande <- parametros$n_eficiencia

  tamanhos <- unique(c(parametros$n_grafico_eficiencia, n_grande))

  beta_teste <- c(parametros$beta_tendencia, beta_sazonal)

  tempos <- NULL

  for(n_atual in tamanhos){

    t_atual <- 1:n_atual

    X_atual <- cbind(criar_matriz_tendencia(t_atual, parametros$grau_tendencia),
                     criar_matriz_fourier(t_atual, parametros$s, K))

    y_atual <- rnorm(n_atual)

    tempos <- rbind(tempos, medir_tempos(X_atual, y_atual, beta_teste, repeticoes))
  }

  cat("Tamanho da amostra para o tempo:", n_grande, "\n")
  cat("Colunas de X:", ncol(X_atual), "\n")
  cat("Repetições:", repeticoes, "\n")

  # Cada versão vetorizada contra os loops da mesma operação, com n_eficiencia
  tempos_grande <- tempos[tempos$n == n_grande, ]

  for(i in 1:nrow(tempos_grande)){

    if(tempos_grande$versao[i] != "Loops"){

      linha_loops <- tempos_grande$operacao == tempos_grande$operacao[i] & tempos_grande$versao == "Loops"

      tempo_loops <- tempos_grande$tempo[linha_loops]

      cat("Operação:", tempos_grande$operacao[i], "com", tempos_grande$expressao[i], "\n")
      cat("Tempo com loops (s):", round(tempo_loops, 3), "\n")
      cat("Tempo vetorizado (s):", round(tempos_grande$tempo[i], 3), "\n")
      cat("Razão (loops / vetorizado):", round(tempo_loops / tempos_grande$tempo[i], 1), "\n")
      cat("Mesmo resultado nas duas versões:", tempos_grande$igual[i], "\n")
      cat("Maior diferença relativa:", signif(tempos_grande$diferenca_relativa[i], 3), "\n")
    }
  }

  return(tempos)
}

# Tempo em função de n, um painel por operação e uma linha por versão (loops, crossprod e operador %*%)
# Entrada: tempos (data.frame de comparar_eficiencia), repeticoes (inteiro, entra no rótulo do eixo y)
# Saída: objeto ggplot com facet_wrap, cada painel com a sua escala em y
# Pré-condição: visualizacao.R carregado (cores e tema_graficos)
plot_tempo_eficiencia <- function(tempos, repeticoes){

  versoes <- c("Loops", "crossprod()", "Operador %*%")

  operacoes <- c("X'X", "X'y", "X·beta")

  dados <- data.frame(
    n_mil = tempos$n / 1000,
    tempo = tempos$tempo,
    versao = factor(tempos$versao, levels = versoes),
    operacao = factor(tempos$operacao, levels = operacoes)
  )

  grafico <- ggplot(dados,
                    aes(x = n_mil,
                        y = tempo,
                        color = versao)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    facet_wrap(~ operacao, ncol = 3, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Loops" = cor_referencia,
        "crossprod()" = cor_acf,
        "Operador %*%" = cor_principal
      ),
      breaks = versoes
    ) +
    guides(color = guide_legend(
      override.aes = list(linewidth = 1.2, alpha = 1)
    )) +
    labs(title = "Tempo de Execução em Função de n",
         x = "Tamanho da amostra n (milhares)",
         y = paste0("Tempo de ", repeticoes, " repetições (s)")) +
    tema_graficos() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 11, face = "bold"))

  return(grafico)
}

parametros <- ler_parametros("parametros.json")

tempos <- comparar_eficiencia(parametros)

grafico <- plot_tempo_eficiencia(tempos, parametros$repeticoes_eficiencia)
print(grafico)
salvar_grafico(grafico,
               "eficiencia/01_tempo_loop_vetorizada",
               parametros)
