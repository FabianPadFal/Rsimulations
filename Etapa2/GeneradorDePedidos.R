#Etapa 2 del proyecto de simulación de Métodos de Modelado y Optimización
#Fecha de entrega: 19/11/2023
#Equipo: Azúcar Sintáctico

#En este proyecto de simulación se emplea una distribución normal para la
#generación de números aleatorios que serán utilizados para simular la demanda
#de productos para cada uno de los tres clientes de la empresa. Esto debido a
#que la distribución se aproxima considerablemente a la realidad ya que la empresa
#tiene una producción estable con pedidos estables. Para ello se hace uso de la 
#función rnorm de R, tomándose como media inicial el promedio de pedidos para
#los tres clientes en determinado mes, posteriormente se toma como media la demanda
#del mes anterio. La desviación estándar no es grande ya que los productos son de
#consumo usual, por lo que no es un riesgo considerable para la empresa. 

#pedidos iniciales. Considerarlos para el promedio y la desviación inicial
pedidosTomate <- c(45,50,35)
pedidosMaizDulce <- c(120,140,160)
pedidosGarbanzos <- c(70,80,90)
pedidosPalmito <- c(25,30,35)

mediaTomate <-mean(pedidosTomate)
mediaMaizDulce <-mean(pedidosMaizDulce)
mediaGarbanzos <-mean(pedidosGarbanzos)
mediaPalmito <-mean(pedidosPalmito)

#simulación de un año utilizando una distribución normal
reporteAnualGarbanzos <- matrix(nrow = 12, ncol = 3)
reporteAnualTomate <- matrix(nrow = 12, ncol = 3)
reporteAnualMaizDulce <- matrix(nrow = 12, ncol = 3)
reporteAnualPalmito <- matrix(nrow = 12, ncol = 3)
for (i in 1:12){
  #Generar la cantidad de productos vendidos en un mes para los 3 clientes
  #Considerar los eventos como fechas, tiempo y ventas del mes anterior 
  #(Se encuentran al final del documento)
  reporteAnualGarbanzos [i, ] <- rnorm(3,mediaGarbanzos,5)
  reporteAnualTomate [i, ] <- rnorm(3,mediaTomate,5)
  reporteAnualMaizDulce [i, ] <- rnorm(3,mediaMaizDulce,5)
  reporteAnualPalmito [i, ] <- rnorm(3,mediaPalmito,3)
  
  #calcular la media de productos vendidos del mes anterior
  mediaTomate <-mean(reporteAnualTomate [i, ])
  mediaMaizDulce <-mean(reporteAnualMaizDulce [i, ])
  mediaGarbanzos <-mean(reporteAnualGarbanzos [i, ])
  mediaPalmito <-mean(reporteAnualPalmito [i, ])
}

reporteAnualGarbanzos
reporteAnualTomate
reporteAnualMaizDulce
reporteAnualPalmito

"
Eventos: tienen un peso en la producción y demanda de ciertos productos a lo largo
del año dado por fechas, estaciones, plagas, etc, que afectan positiva o negativamente
a la empresa. Existen eventos que ocurren esporádicamente y otros que por estudios
realizados previamente afectan la producción con un porcentaje estable en determinados
periodos del año.
  1-Semana Santa: Aumenta la demanda del palmito un 30% y baja la demanda de 
    garbanzos un 45%
  2-Días lluviosos: Aumenta la demanda de sopa de tomate un 50% y de garbanzos un 15%
  3-Días de temporada seca: Aumenta el tomate un 10%, el palmito un 25% y baja la demanda de garbanzos un 5%
  4-Navidad y fin de año: Aumenta la demanda de garbanzos un 50% y baja la del tomate un 25%
  5-Feria del maíz: Aumenta el maíz dulce un 70%
  6-Cuesta de enero: la demanda de todos los productos pueden bajar entre un 20% o 40%
  7-Vacaciones de medio año: la demanda de todos los productos pueden bajar entre un 10% o 20%
  8-Plagas:
    Hay un 5% de probabilidad de que ocurra una plaga del tomate bajando la producción un 40%
    Hay un 2% de probabilidad de que ocurra una plaga del palmito bajando la producción un 20%
    Hay un 10% de probabilidad de que ocurra una plaga del maíz bajando la producción un 35%
  
"