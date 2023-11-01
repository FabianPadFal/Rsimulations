
#--------------- Generadores --------------------
source("GenFunctions.R")

#------------ Simulacion -----------------
# simulacion de 100 partidas
# Variables para el seguimiento
ganancias <- c() # Almacena las ganancias o pérdidas de cada juego
lanzamientos_por_juego <- c() # Almacena la cantidad de lanzamientos por juego


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
  # Dinero ganado o perdido en este juego
  dinero <- -8 # Se resta el pago inicial de 8 dólares
  ## ----- Eventos --------
  while (abs(escudos-coronas) < 3) {
    result <- coinflip()
    if(result == 1) {
      escudos <- escudos + 1
    } else {
      coronas <- coronas + 1
    }
    rondas <- rondas + 1
    dinero <- dinero + 1 # Se resta 1 dólar por cada lanzamiento
  }
  
  ganancias <- c(ganancias, dinero) # Guarda la ganancia o pérdida de este juego
  lanzamientos_por_juego <- c(lanzamientos_por_juego, rondas) # Guarda la cantidad de lanzamientos de este juego
  
  cat("Se ha alcanzado la condicion de victoria!\n")
  cat("Partida:", partida, "\tRondas:", rondas,"\tEscudos: ",escudos, "\tCoronas", coronas, "\n")
}

# final de la simulacion
# Evaluacion de resultados

# Cálculo de los promedios
promedio_ganancia <- mean(ganancias)
promedio_lanzamientos <- mean(lanzamientos_por_juego)

cat("Promedio de ganancia por juego: $", promedio_ganancia, "\n")
cat("Promedio de lanzamientos por juego: ", promedio_lanzamientos, "\n")
