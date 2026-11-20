############################################################
# 05_sim_cilindro.R
# Verifica quais trajetórias ficam dentro de um conjunto
# cilíndrico e calcula a contagem e a proporção.
# Dependências: nenhuma.
############################################################

# verifica quais trajetórias passam por todas as janelas do cilindro
# entrada: trajetorias (matriz n x m, uma coluna por trajetória),
#          tempos_fix (instantes fixados), limite_inferior e
#          limite_superior (vetores do mesmo tamanho de tempos_fix)
# saída: vetor lógico de tamanho m, TRUE se a trajetória está no cilindro
verificar_cilindro <- function(trajetorias,
                               tempos_fix,
                               limite_inferior,
                               limite_superior){

  m <- ncol(trajetorias)

  pertence_cilindro <- logical(m)

  for(i in 1:m){

    pertence <- TRUE

    # a trajetória precisa estar dentro de todas as janelas
    for(j in 1:length(tempos_fix)){

      t <- tempos_fix[j]

      valor <- trajetorias[t,i]

      limite_inf <- limite_inferior[j]
      limite_sup <- limite_superior[j]

      if(valor < limite_inf || valor > limite_sup){
        pertence <- FALSE
      }
    }

    pertence_cilindro[i] <- pertence
  }

  return(pertence_cilindro)
}

# conta as trajetórias que pertencem ao cilindro
# entrada: pertence_cilindro (vetor lógico, um valor por trajetória)
# saída: um número, a quantidade de TRUE
contar_trajetorias_cilindro <- function(pertence_cilindro){

  numero_trajetorias_cilindro <- 0

  for(i in 1:length(pertence_cilindro)){

    if(pertence_cilindro[i]){
      numero_trajetorias_cilindro <-
        numero_trajetorias_cilindro + 1
    }
  }

  return(numero_trajetorias_cilindro)
}

# proporção de trajetórias no cilindro, contagem / m
# entrada: numero_trajetorias_cilindro (contagem), m (total de trajetórias)
# saída: um número entre 0 e 1
calcular_proporcao_cilindro <- function(numero_trajetorias_cilindro, m){
  proporcao <- numero_trajetorias_cilindro / m
  return(proporcao)
}
