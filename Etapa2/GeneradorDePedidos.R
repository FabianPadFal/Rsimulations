#pedidos iniciales. Considerarlos para el promedio y la desviación inicial
pedidosTomate <- c(30,30,30)
pedidosMaizDulce <- c(30,30,30)
pedidosGarbanzos <- c(30,30,30)
pedidosPalmito <- c(30,30,30)

productosRechazados <- c(FALSE, FALSE, FALSE)

#simulación de un año
reporteAnualGarbanzos <- matrix(nrow = 12, ncol = 3)
reporteAnualTomate <- matrix(nrow = 12, ncol = 3)
reporteAnualMaizDulce <- matrix(nrow = 12, ncol = 3)
reporteAnualPalmito <- matrix(nrow = 12, ncol = 3)
for (i in 1:12){
  #Generar la cantidad de productos vendidos en un mes para los 3 clientes
  #Considerar los eventos como fechas, tiempo y ventas del mes anterior
  
  reporteAnualGarbanzos [i, ] <- rnorm(3,25,5)
  reporteAnualTomate [i, ] <- rnorm(3,25,5)
  reporteAnualMaizDulce [i, ] <- rnorm(3,25,5)
  reporteAnualPalmito [i, ] <- rnorm(3,25,5)
}

reporteAnualGarbanzos
reporteAnualTomate
reporteAnualMaizDulce
reporteAnualPalmito