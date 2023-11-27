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
#del mes anterior. La desviación estándar no es grande ya que los productos son de
#consumo usual, por lo que no es un riesgo considerable para la empresa. 
deuda <- 40000
ganancia <- 0
presupuesto <- 10000000 + ganancia

cantidadDeChefs <- 2
salarioDeChef <- 280000 * 12
tasaDeProduccion <- 600

cantidadDeConductores <- 2
salarioDeConductor <- 280000 * 12

cantidadDeLatas <- 1000
precioPorLata <- 100

gastosEnGasolina <- 350000

registroDePedidos <- c()
registroDeLatasTomate <- 0
registroDeLatasGarbanzos <- 0
registroDeLatasMaizDulce <- 0
registroDeLatasPalmito <- 0

gastosLataTomate <- 500
gastosLataPalmito <- 600
gastosLataGarbanzos <- 350
gastosLataMaizDulce <- 200

precioLataTomate <- 1300
precioLataPalmito <- 1300
precioLataGarbanzos <- 1300
precioLataMaizDulce <- 1300

gananciaLataTomate <- precioLataTomate - gastosLataTomate
gananciaLataPalmito <- precioLataPalmito - gastosLataPalmito
gananciaLataGarbanzos <- precioLataGarbanzos - gastosLataGarbanzos
gananciaLataMaizDulce <- precioLataMaizDulce - gastosLataMaizDulce

gananciaAnualGarbanzos <- 0
gananciaAnualTomate <- 0
gananciaAnualPalmito <- 0
gananciaAnualMaizDulce <- 0
gananciaAnualTotal <- c()
#pedidos por mes (aumentar la cantidad para obtener una mayor ganancia)
pedidosTomate <- c(45,50,35)
pedidosMaizDulce <- c(120,140,160)
pedidosGarbanzos <- c(70,80,90)
pedidosPalmito <- c(25,30,35)

mediaTomate <-mean(pedidosTomate)
mediaMaizDulce <-mean(pedidosMaizDulce)
mediaGarbanzos <-mean(pedidosGarbanzos)
mediaPalmito <-mean(pedidosPalmito)

#simulación de un año utilizando una distribución de Poisson
reporteAnualGarbanzos <- matrix(nrow = 12, ncol = 3)
reporteAnualTomate <- matrix(nrow = 12, ncol = 3)
reporteAnualMaizDulce <- matrix(nrow = 12, ncol = 3)
reporteAnualPalmito <- matrix(nrow = 12, ncol = 3)
for (año in 1:15){
  print("Año ")
  print(año)
  for (mes in 1:12){
    mediaTomate <-mean(pedidosTomate)
    mediaMaizDulce <-mean(pedidosMaizDulce)
    mediaGarbanzos <-mean(pedidosGarbanzos)
    mediaPalmito <-mean(pedidosPalmito)
    #Generar la cantidad de productos vendidos en un mes para los 3 clientes
    #Considerar los eventos como fechas, tiempo y ventas del mes anterior 
    #Fechas
    if (mes == 1){
      #Cuesta de enero: la demanda de todos los productos
      #pueden bajar entre un 20% o 40%
      mediaGarbanzos <- mediaGarbanzos - (mediaGarbanzos*20/100)
      mediaTomate <- mediaTomate - (mediaTomate*20/100)
      mediaMaizDulce <- mediaMaizDulce - (mediaMaizDulce*20/100)
      mediaPalmito <- mediaPalmito - (mediaPalmito*20/100)
    } else {
      if (mes == 3){
        #Semana Santa: Aumenta la demanda del palmito un 30% y
        #baja la demanda de garbanzos un 45%
        mediaPalmito <- mediaPalmito + (mediaPalmito*45/100)
        mediaGarbanzos <- mediaGarbanzos - (mediaGarbanzos*30/100)
      } else if (mes == 7){
        #Vacaciones de medio año: la demanda de todos los productos pueden bajar
        #entre un 10% o 20%
        mediaGarbanzos <- mediaGarbanzos - (mediaGarbanzos*10/100)
        mediaTomate <- mediaTomate - (mediaTomate*10/100)
        mediaMaizDulce <- mediaMaizDulce - (mediaMaizDulce*10/100)
        mediaPalmito <- mediaPalmito - (mediaPalmito*10/100)
      } else if (mes == 12){
        #Navidad y fin de año: Aumenta la demanda de garbanzos un 50% y
        #baja la del tomate un 25%
        mediaGarbanzos <- mediaGarbanzos + (mediaGarbanzos*50/100)
        mediaTomate <- mediaTomate - (mediaTomate*25/100)
      }
    }
    #Tiempo atmosférico: en Costa Rica llueve en promedio 163 días del año
    #Al mes son 163/12, la tasa de lluvia al mes es de 12/163
    tasaDeDiasLluviosos <- 12/163
    registroAtmosféricoMensual <- rep(0,30)
    if (mes == 4 || mes == 5 || mes == 9 || mes == 10){
      #si es abril, mayo, septiembre u octubre aumenta la tasa de días 
      #lluviosos un 60%:
      tasaDeDiasLluviosos <- tasaDeDiasLluviosos + (tasaDeDiasLluviosos*60/100)
      registroAtmosféricoMensual <- rep(0,30)
      for(dia in 1:30){
        registroAtmosféricoMensual[dia] <- rpois(1,tasaDeDiasLluviosos)
      }
    } else {
      for(dia in 1:30){
        registroAtmosféricoMensual[dia] <- rpois(1,tasaDeDiasLluviosos)
      }
    }
    #se cuentan cuántos días lluvió durante el mes, si es mayor que 4 se
    #considera un aumento de demanda significativa
    if (sum(registroAtmosféricoMensual) > 6){
      #Días lluviosos: Aumenta la demanda de sopa de tomate un 50%
      #y de garbanzos un 15%
      mediaTomate <- mediaTomate + (mediaTomate*50/100)
      mediaGarbanzos <- mediaGarbanzos + (mediaGarbanzos*15/100)
    }
    
    reporteAnualGarbanzos [mes, ] <- rpois(3,mediaGarbanzos)
    reporteAnualTomate [mes, ] <- rpois(3,mediaTomate)
    reporteAnualMaizDulce [mes, ] <- rpois(3,mediaMaizDulce)
    reporteAnualPalmito [mes, ] <- rpois(3,mediaPalmito)
    
    latasTotalesPedidas <- sum(reporteAnualGarbanzos[mes,]
                               ,reporteAnualMaizDulce[mes,]
                               ,reporteAnualPalmito[mes,]
                               ,reporteAnualTomate[mes,])
    
    cantidadDeLatas <- cantidadDeLatas - latasTotalesPedidas
    print("Latas pedidas durante el mes")
    print(latasTotalesPedidas)
    if (latasTotalesPedidas > cantidadDeChefs*tasaDeProduccion){
      #considerar contratar o despedir chefs
    }
    
    #pagar salarios
    
    #Reabastecer inventario (compra de latas, ingredientes y gasolina)
    cantidadDeLatas <- cantidadDeLatas + latasTotalesPedidas
    
  }
  print("Reporte Garbanzos")
  print(reporteAnualGarbanzos)
  print("Reporte Tomate")
  print(reporteAnualTomate)
  print("Reporte MaizDulce")
  print(reporteAnualMaizDulce)
  print("Reporte Palmito")
  print(reporteAnualPalmito)
  
  gananciaAnualGarbanzos <- sum(reporteAnualGarbanzos)*gananciaLataGarbanzos
  gananciaAnualTomate <- sum(reporteAnualTomate)*gananciaLataTomate
  gananciaAnualPalmito <- sum(reporteAnualPalmito)*gananciaLataPalmito
  gananciaAnualMaizDulce <- sum(reporteAnualMaizDulce)*gananciaLataMaizDulce
  gananciaAnualTotal[año] <- gananciaAnualGarbanzos + gananciaAnualTomate + gananciaAnualPalmito + gananciaAnualMaizDulce
  
  gananciaAnualTotal[año] <- gananciaAnualTotal[año] - salarioDeChef*cantidadDeChefs - salarioDeConductor*cantidadDeConductores - gastosEnGasolina - (deuda - deuda*5/100)
  #considerar mantenimiento de los vehículos, pago de aguinaldos, aumento de pedidos...
}

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