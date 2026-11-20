############################################################
# ex4_eficiencia.R
# Compara loops, crossprod e %*%, confere o OLS com lm() e mede os tempos para vários n.
# Dependências: parametros.json e scripts/config.R, dados.R, processamento.R e visualizacao.R.
############################################################

rm(list = ls())

library(ggplot2)
library(jsonlite)

source("scripts/config.R")
source("scripts/dados.R")
source("scripts/processamento.R")
source("scripts/visualizacao.R")

# Calcula X'X com crossprod(X), sem usar loops.
# Entrada: X é uma matriz n x p.
# Saída: matriz p x p.
calcular_XtX_vetorizada <- function(X){
  resultado <- crossprod(X)
  return(resultado)
}

# Calcula X'y com crossprod(X, y), sem usar loops.
# Entrada: X é uma matriz n x p e y é um vetor de tamanho n.
# Saída: vetor de tamanho p.
calcular_Xty_vetorizada <- function(X, y){
  resultado <- as.numeric(crossprod(X, y))
  return(resultado)
}

# Calcula X'X com t(X) %*% X, formando a transposta explicitamente.
# Entrada: X é uma matriz n x p.
# Saída: matriz p x p.
calcular_XtX_transposta <- function(X){
  resultado <- t(X) %*% X
  return(resultado)
}

# Calcula X'y com t(X) %*% y, formando a transposta explicitamente.
# Entrada: X é uma matriz n x p e y é um vetor de tamanho n.
# Saída: vetor de tamanho p.
calcular_Xty_transposta <- function(X, y){
  resultado <- as.numeric(t(X) %*% y)
  return(resultado)
}

# Multiplica X por beta com %*%, sem usar loops.
# Entrada: X é uma matriz n x p e beta é um vetor de tamanho p.
# Saída: vetor de tamanho n.
multiplicar_matriz_vetor_vetorizada <- function(X, beta){
  resultado <- as.numeric(X %*% beta)
  return(resultado)
}

# Ajusta OLS pelas equações normais, usando crossprod para calcular X'X e X'y.
# Entrada: X é uma matriz n x p e y é um vetor de tamanho n.
# Saída: vetor beta_hat de tamanho p.
# Pré-condição: X'X deve ser invertível.
ajustar_ols_vetorizada <- function(X, y){

  XtX <- calcular_XtX_vetorizada(X)

  Xty <- calcular_Xty_vetorizada(X, y)

  beta_hat <- as.numeric(solve(XtX, Xty))

  return(beta_hat)
}

# Ajusta OLS pelas equações normais, usando t(X) %*% X e t(X) %*% y.
# Entrada: X é uma matriz n x p e y é um vetor de tamanho n.
# Saída: vetor beta_hat de tamanho p.
# Pré-condição: X'X deve ser invertível.
ajustar_ols_transposta <- function(X, y){

  XtX <- calcular_XtX_transposta(X)

  Xty <- calcular_Xty_transposta(X, y)

  beta_hat <- as.numeric(solve(XtX, Xty))

  return(beta_hat)
}

# Calcula max(abs(a - b)) / max(abs(a)) para comparar os resultados em escala relativa.
# Entrada: a é a referência e b é o resultado comparado, ambos do mesmo tamanho.
# Saída: diferença relativa máxima, que perto de 1e-16 indica erro de arredondamento.
# Pré-condição: a deve conter pelo menos um valor diferente de zero.
calcular_diferenca_relativa <- function(a, b){
  diferenca <- max(abs(a - b)) / max(abs(a))
  return(diferenca)
}

# Mede os tempos de X'X, X'y e X beta com loops, crossprod e %*%, conferindo os resultados.
# Entrada: X é uma matriz n x p, y tem tamanho n, beta tem tamanho p e repeticoes
# é um inteiro positivo.
# Saída: data.frame com n, operacao, versao, expressao, tempo, igual e diferenca_relativa.
# Pré-condição: nrow(X) = length(y) e ncol(X) = length(beta).
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

# Compara o OLS com lm() e mede os tempos das operações para diferentes tamanhos de série.
# Entrada: parametros contém a configuração, a simulação e os tamanhos do teste.
# Saída: data.frame com os tempos, além dos resultados de exatidão e dos tempos
# em n_eficiencia apresentados no console.
# Pré-condição: parametros deve ter sido lido por ler_parametros.
comparar_eficiencia <- function(parametros){

  fixar_semente(parametros)

  K <- parametros$K
  repeticoes <- parametros$repeticoes_eficiencia

  beta_sazonal <- criar_beta_sazonal(parametros$alpha, parametros$beta_h, K)

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

  # Mede cada operação separadamente para os tamanhos escolhidos, sem incluir solve() nos tempos.
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

# Mostra os tempos em função de n, separando as operações em painéis e as versões em linhas.
# Entrada: tempos é o data.frame das medições e repeticoes define a unidade do eixo y.
# Saída: objeto ggplot com escalas independentes no eixo y.
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
