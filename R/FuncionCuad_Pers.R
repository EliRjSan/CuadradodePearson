#' Cuadrado de Pearson
#'
#' El modelo Doble Cuadrado de Pearson permite balancear raciones,la cual tiene limitaciones no se pueden utilizar muchos ingredientes.Consiste en realizar un cuadro donde en el extremo superior,se pone el valor de proteína que aportan los dos primeros alimentos proteicos los cuales deben de estar por encima del requerimiento y en la esquina inferior se ponen los dos alimentos energéticos los cuales estarán por debajo del requerimiento y en el centro se pone el valor del requerimientos que necesitamos.
#'
#' @param datos de la variable respuesta
#' @param req es la necesidad que necesitan de proteina que necesitan nuestros animales.
#' @param kilos es la cantidad de kilos que queremos producir de alimento.
#' @param p1 es el alimento proteico 1
#' @param p2 es el alimento proteico 2
#' @param e1 es el alimento energético 1
#' @param e2 es el alimento energético 2
#' @param precio1 es el precio 1 del alimento proteico 1
#' @param precio2 es el precio del alimento proteico 2
#' @param precio3 es el precio del alimento enegético 1
#' @param precio4 es le precio del alimento energético 2
#' @return devuelve una tabla donde esta $Tabla_MayorReq la cual nos indica los alimentos proteicos que son mayores al requerimiento que necesitamos y tambien la cantidad de proteina que contiene cada alimento,tambien una tabla llamada $Tabla_MenorReq la cual no indica los alimentos energéticos que son menores al requerimiento,posteriormente tenemos una tabla en donde sólo estan los dos alimentos de la clasificacion 1 y la clasificación 2 que cumplen con las condiciones que nos indica el cuadrado de Pearson y juento con ellas no da el porcentaje de proteina que contiene cada una de ellas ,finalmente nos devuelve una tabla final en donde tenemos el prrocentaje de proteina bruta de cada alimento,las diferencias,una comprobación de si es correctyo o noel aporte de proteína de cada uno de los alimentos,y el precio total por alimento y la suma de los precios de todos los alimentos,de igual forma elabora un gráfico de pastel  de proteinas donde se observa el aporte de proteína de cada uno de los alimentos,y otro gráfico en histograma  donde se observa el precio de cada alimento y ver cual de estos alimentos es más caro.
#' @export
CuadradoPearson <- function(datos, req, kilos=1000, p1=1, p2=2, e1=1, e2=2,
                            precio1=40, precio2=70, precio3=80, precio4=70) {
  precios <- c(precio1, precio2, precio3, precio4)
  valores1 <- datos[datos$PB1 > req, c(1, 2, 3)] # mayores a req
  valores2 <- datos[datos$PB2 < req, c(4, 5, 6)] # menores a req
  ppb1 <- valores1[c(p1, p2),] # dos valores
  ppb2 <- valores2[c(e1, e2),] # dos valores
  PB <- c(ppb1[1, 3], ppb1[2, 3], ppb2[1, 3], ppb2[2, 3]) # proteina bruta
  difp1 <- abs(ppb1[1, 3] - req) # diferencias con el requerimiento
  difp2 <- abs(ppb1[2, 3] - req)
  dife1 <- abs(ppb2[1, 3] - req)
  dife2 <- abs(ppb2[2, 3] - req)
  difs <- c(difp1, difp2, dife1, dife2) # vector de diferencias
  sumdif <- sum(difs) # suma de las diferencias
  PACR <- round(c((dife2*100)/sumdif, (dife1*100)/sumdif, (difp2*100)/sumdif,
                  (difp1*100)/sumdif), 2)
  Comp <- round((PB*PACR)/100,2)  # comprobacion

  Cantidad <- (difs/100)* kilos
  precio_total <- format(Cantidad*precios, big.mak=",")

  Tabla_MayorReq <- data.frame(valores1)
  Tabla_MenorReq <- data.frame(valores2)
  Proteinas <- ppb1
  Energeticos <- ppb2
  Tabla_Final <- data.frame(cbind(PB, difs, PACR, Comp, Cantidad, precio_total))
  otros <- c(suma_PACR = sum(PACR),
             suma_Comprobacion = sum(Comp),
             suma_precios = sum(Cantidad*precios))
  colores<-c("springgreen1","blue","red1","purple1","yellow")
  grafica_pastel <- pie(PACR, labels = c("P1", "P2", "E1", "E2"), col=colores,
                        main = "Proteína De Cada Alimento")

  tabprec <- c("P1" = Cantidad[1]*precio1, "P2" = Cantidad[2]*precio2,
               "E1" = Cantidad[3]*precio3, "E2" = Cantidad[4]*precio4)
  grafica_precios <- barplot(tabprec, col=c("cyan","royalblue1","brown1","purple1","magenta"),
                             main = "Precio Por Alimento")

  return(list("Tabla_MayorReq"=Tabla_MayorReq, "Tabla_MenorReq"=Tabla_MenorReq,
              "Proteinas"=Proteinas, "Energeticos"=Energeticos,
              "Tabla Final" = Tabla_Final, "Otros" = otros,
              "Gráfico de proteínas" = grafica_pastel,
              "Gráfico de precios"= grafica_precios))
}
