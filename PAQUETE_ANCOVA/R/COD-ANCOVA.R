#' Información del paquete ANCOVA
#'
#' El paquete ANCOVA tiene la función ANCOVA: Esta función le ayudará a obtener una tabla resumen del Análisis de covarianza (ANCOVA)
#' para un Diseño en Bloques Completamente al Azar.
#' Esto le permitirá conocer que tanto ha influido una covariable en la variable respuesta de
#' su diseño experimental.
#'
#' Para comenzar usted necesita tener sus datos acomodados en formato largo ya sea en Excel, csv, data.frame, u otra aplicación
#' que Rstudio le permita cargar, asegurese de que sus tratamientos o bloques sean nombrados con datos numericos; una vez cubierta esa parte, tendrá que cargar la función en su ordenador junto con el paquete Knitr
#' (este ultimo ayudará a la función a ordenar sus resultados en una tabla) la función ya cargará el knitr por usted: "install.packages("knitr")"
#'
#'
#' @param y (vector) nombre de la variable respuesta.
#' @param covariable (vector) nombre de la covariable.
#' @param tratamiento (string) nombre de la variable que representa a los tratamientos.
#' @param bloque (string) nombre de la variable que representa los bloques.
#' @param significancia (numeric) El valor de alfa o el valor de cuanto error se permite para el experimento
#' @param data Nombre de su archivo donde alberga la información la cual tiene que estar perfectamente acomodada en una tabla de datos formato largo con los datos
#'    de los tratamientos, bloques, covariable y de la variable respuesta.
#' @return Devuelve una tabla con los datos de la suma de cuadrados y productos cruzados de x,y,xy para los tratamientos, bloques, error, totales y error ajustado, tambien devuelve la F calculada de la covariable y de los tratamientos, la F tabulada de la covariable y de los tratamientos, ademas le proporciona el dato de coeficiente de covarianza para que en el caso de que su covariable sea significativa pueda hacer el ajuste de medias correspondiente.
#' @export
#'
#' @examples
#'
#'# A continuación se muestra un ejemplo para comprender el uso de la función que alberga el paquete ANCOVA:
#'
#'En un cultivo de sorgo forrajero se evaluó el rendimiento de grano y
#'la producción de forraje. Para ello, se compararon 3 tratamientos: 30, 40, y 50 días
#'después de ocurrida la floración. El número de plantas por parcela útil fue de 52.
#'A la cosecha el número de plantas por tratamiento y repetición fue diferente.
#'Los datos obtenidos al finalizar el experimento se presentan en el siguiente cuadro,
#'en el cual X representa el número de plantas por parcela y Y la producción
#'en grano en kilogramos por parcela. El diseño experimental.
#'
#'DATOS:
#'  df <- data.frame(bloque = c(1,1,1,1,1,1,,2,2,2,2,2,2,3,3,3,3,3,3),
#'                   tratamiento = c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3),
#'                   Nplantas = c(41,24,31,46,32,38,37,32,38,41,42,40,37,34,47,41,50,39),
#'                   producción = c(4.08,2.78, 2.79, 4.24, 4.17, 2.62, 4.72, 4.92, 4.5,
#'                                  5.62, 5.15, 4.32, 4, 5.05, 5.54, 6.46, 6.65, 5.7))
#'
#'Significancia de 0.05
#'
#'# Paso 1: Limpiar la memoria de R con el comando
#'
#'rm(list=ls())
#'
#'# Paso 2: Cargamos el paquete ANCOVA:
#'
#'library(ANCOVA)
#'
#'
#'# la función pide los argumentos siguientes
#'
#'ANCOVA(y, tratamiento, bloque, covariable, significancia, data)
#'
#'# Para este caso tendriamos que argumentar a la función así:
#'
#'  ANCOVA(y="producción", tratamiento = "tratamiento", bloque = "bloque",
#'         covariable = "Nplantas", significancia = 0.05, data = df)
#'
#'# O simplemente puede:
#'
#'  ANCOVA("producción", "tratamiento","bloque", "Nplantas", 0.05, df)
#'
#'
#'# Paso 3: Usted tendrá consigo dos tablas del análisis de covarianza de sus datos:
#'Una tabla será de la suma de cuadrados y la otra de las F
#'calculadas y tabuladas tanto para la covarianza como  para el tratamiento.
#'
#'CON DICHAS TABLAS PODRÁ HACER DEDUCCIONES DE QUE TAN INFLUYENTE RESULTÓ SU COVARIABLE EN
#'LOS RESULTADOS EXPERIMENTALES.
#'
#'1. SI F_cal_trat > F_tab_trat, se rechaza Ho por lo que se deduce que la covariable
#'es significativa al menos en alguno de los bloques y por lo tanto se tendría que hacer un ajuste de medias.
#'
#'2. SI F_cal_cov > F_tab_cov, NO se rechaza Ho por lo que la covariable en
#'su experimento no es significativa.
#'
#'
#'Las tablas de las F calculadas y tabuladas le anexará el coeficiente de covarianza (COEF_COVARIANZA) el cual servirá para hacer el ajuste de medias correspondiente.
#'
#'Para hacer este ajuste de medias usted puede usar la función "ANCOVA_AJUST" que al alberga este paquete, para más información puede consultar el manual.
ANCOVA <- function(y, tratamiento, bloque, covariable, significancia, data){



  trat <- factor(data[,tratamiento])
  bloque <- factor(data[, bloque])
  cov <- data[, covariable]
  (y <- data[,y])
  (t <- nlevels(trat))
  (r <- nlevels(bloque))
  class(trat)
  #Gran total de x y y
  (suma_totay <- sum (y))
  (suma_totax <- sum(cov))


  #Suma de cuadrados
  (Sum_cuadrado_x <- (suma_totax^2)/(t*r))
  (Sum_cuadrado_y <- (suma_totay^2)/(t*r))
  (Sum_cuadrado_xy <- (suma_totay*suma_totax)/(t*r))


  #Suma de cuadrados o de productos totales
  (SCTX <- (sum(cov^2))-Sum_cuadrado_x)
  (SCTY <- sum(y^2)-Sum_cuadrado_y)
  (SCTXY <- sum(cov*y)-Sum_cuadrado_xy)

  #Suma de cuadrados o de productos de bloques:
  #Se hace primero una sumatoria de todas las x que esten en el bloque 1, de todas las de bloque 2
  #de todas las del bloque 3. Lo mismo para las y:
  (Tbloque1 <- tapply(cov, INDEX = bloque, FUN = sum))
  (Tbloque2 <- tapply(y, INDEX = bloque, FUN = sum))

  (SCBX <- sum((Tbloque1^2)/(t))-(Sum_cuadrado_x))
  (SCBY <- ((sum(Tbloque2^2))/t)-(Sum_cuadrado_y))
  (SCBXY <- ((sum(Tbloque1*Tbloque2))/t)-(Sum_cuadrado_xy))

  #Suma de cuadrados o de productos de tratamientos:
  #total de tratamientos
  (Ttrat1 <- tapply(cov, INDEX = trat, FUN = sum))
  (Ttrat2 <- tapply(y, INDEX = trat, FUN = sum))

  (SCTRX <- sum((Ttrat1^2)/(r))-(Sum_cuadrado_x))
  (SCTRY <- sum((Ttrat2^2)/(r))-(Sum_cuadrado_y))
  (SCTRXY <- sum((Ttrat1*Ttrat2)/(r))-(Sum_cuadrado_xy))

  #Suma de cuadrados o de productos del error para x:
  (SCPEX <- SCTX-SCBX-SCTRX)
  #Suma de cuadrados o de productos del error para y:
  (SCPEY <- SCTY-SCBY-SCTRY)
  #Suma de cuadrados o de productos del error para xy:
  (SCPEXY <- SCTXY-SCBXY-SCTRXY)

  #Grados de libertad
  #Gl para total
  (glT <- (t*r)-1)
  #Gl para bloques
  (glB <- (r-1))
  #Gl para tratamientos
  (glTrat <- (t-1))
  #Gl para error
  (glError <- (t-1)*(r-1))

  #Calculamos el error ajustado

  (SCErrorAx <- SCTRX + SCPEX)
  (SCErrorAy <- SCTRY + SCPEY)
  (SCErrorAxy <- SCTRXY + SCPEXY)
  (SCErrorAg.l <- r*t-r)

  #Calculamos la suma de cuadrados para tratamientos, error y error ajustado:
  (SCErrorajust <- SCErrorAy-(SCErrorAxy^2/SCErrorAx))
  (SCError <- SCPEY-(SCPEXY^2/SCPEX))
  (SCtratamientos <- SCErrorajust-SCError)

  TANCOVA <- data.frame(FV = c("TOTAL","BLOQUES","TRAT","ERROR", "ERROR_AJUST"),
                        g.l = c(glT,glB,glTrat,glError,SCErrorAg.l),
                        SCX = c(SCTX,SCBX,SCTRX,SCPEX,SCErrorAx),
                        SCY = c(SCTY,SCBY,SCTRY,SCPEY,SCErrorAy),
                        SCXY = c(SCTXY,SCBXY,SCTRXY,SCPEXY,SCErrorAxy),
                        SC = c(NA,NA,SCtratamientos,SCError,SCErrorajust))

  library(knitr)
  tabla <- kable(TANCOVA)
  print(tabla)

  # Calculamos el coeficiente de covarianza
  (CoefCOVARIANZA <- SCPEXY/SCPEX)

  # Calculamos el cuadrado medio de Tratamiento y error
  (CM_TRAT <- SCtratamientos/(t-1))
  (CM_E <- SCError/((t-1)*(r-1)-1))

  #Ahora calcularemos F calculada de tratamiento y F calculada de la covariable
  (Fcaltrat <- CM_TRAT/CM_E )
  (Fcalcov <-(SCPEXY^2/SCPEX)/CM_E)

  (Fcalv1 <- t-1)
  (Fcalv2 <- (r-1)*(t-1)-1)
  (Fcalv3 <- 1)

  (Ftabtrat <- qf(significancia,Fcalv1,Fcalv2,lower.tail=F))
  (Ftabcov <- qf(significancia,Fcalv3,Fcalv2,lower.tail=F))

  HO <- data.frame(F_cal_trat = c(Fcaltrat),
                   F_tab_trat = c(Ftabtrat),
                   F_cal_cov = c(Fcalcov),
                   F_tab_cov = c(Ftabcov),
                   COEF_COVARIANZA = c(CoefCOVARIANZA))
  library(knitr)
  tabla <- kable(HO)
  print(tabla)
}

