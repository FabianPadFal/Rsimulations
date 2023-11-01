
#--------------- Generadores --------------------
source("GenFunctions.R")

#------------ Simulacion -----------------
# simulacion de 100 partidas
for(i in 1:100) {
  # #Partida
  partida <- i
  # Monedas
  ## Escudos
  escudos <- 0
  ## Coronas
  coronas <- 0
  # contador de rondas
  rondas <- 0
  # resultado
  result <- 0
  ## ----- Eventos --------
  while (abs(escudos-coronas) < 3) {
    result <- coinflip()
    if(result == 1) {
      escudos <- escudos + 1
    } else {
      coronas <- coronas + 1
    }
    rondas <- rondas + 1
    
  }
  cat("Se ha alcanzado la condicion de victoria!\n")
  cat("Partida:", partida, "\tRondas:", rondas,"\tEscudos: ",escudos, "\tCoronas", coronas, "\n")
}

# final de la simulacion
# Evaluacion de resultados
