############################################################
# Arquivo: sim_cilindro.R
# Verifica o pertencimento a conjuntos cilíndricos e calcula contagem e proporção.
# Dependências: nenhuma.
############################################################

# Verifica quais trajetórias pertencem a um cilindro.
# Entrada: trajetorias (matriz n x m), tempos_fix, limite_inferior e limite_superior.
# Saída: vetor lógico de tamanho m, com TRUE para as trajetórias que pertencem ao cilindro.
verificar_cilindro <- function(trajetorias,
                               tempos_fix,
                               limite_inferior,
                               limite_superior){

  m <- ncol(trajetorias)

  pertence_cilindro <- logical(m)

  for(i in 1:m){

    pertence <- TRUE

    # Verifica todas as janelas, pois a trajetória precisa atender a cada uma delas.
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

# Conta as trajetórias que pertencem ao cilindro.
# Entrada: pertence_cilindro (vetor lógico, um valor por trajetória).
# Saída: número de valores TRUE.
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

# Calcula a proporção de trajetórias no cilindro: numero_trajetorias_cilindro / m.
# Entrada: numero_trajetorias_cilindro (contagem) e m (total de trajetórias).
# Saída: proporção entre 0 e 1.
calcular_proporcao_cilindro <- function(numero_trajetorias_cilindro, m){
  proporcao <- numero_trajetorias_cilindro / m
  return(proporcao)
}
