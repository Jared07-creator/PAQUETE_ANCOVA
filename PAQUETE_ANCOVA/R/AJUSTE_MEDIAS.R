#' Información de la función ANCOVA_AJUST
#'
#'Esta función le ayudara a obtener el ajuste de las medias de sus tratamientos en caso de que la covariable haya resultado significativa en los resultados.
#'
#' De igual manera que en la función principal, usted necesita tener sus datos acomodados en formato largo ya sea en Excel,
#' csv, data.frame u otra presentación que Rstudio le permita cargar, asegurese de que sus tratamientos o bloques
#' sean nombrados con datos numericos: Tratamiento 1: 1, Tratamiento 2: 2, Tratamiento 3: 3.
#'
#' @param coef_covarianza (numeric) el valor del coeficiente de covarianza que la función principal le haya dado o que haya sido obtenido de otra fuente.
#' @param y (vector) nombre de la variable respuesta.
#' @param covariable (vector) nombre de la covariable.
#' @param tratamiento (string) nombre de la variable que representa a los tratamientos.
#' @param bloque (string) nombre de la variable que representa los bloques.
#' @param data Nombre de su archivo donde alberga la información la cual tiene que estar
#' perfectamente acomodada en una tabla de datos formato largo con los datos
#'    de los tratamientos, bloques, covariable y de la variable respuesta.
#' @return Devuelve la media ajustada para cada uno de sus tratamientos.
#' @export
#'
#' @examples
#'
#'# A continuación se muestra un ejemplo para comprender el uso de esta función que alberga el paquete ANCOVA:
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
#'
#'# Paso 2: Cargamos el paquete ANCOVA con:
#'
#'library(ANCOVA)
#'
#'#Al cargar el paquete ANCOVA automaticamente la función ANCOVA_AJUST estará lista para usarse.
#'
#'# la función le pedirá los argumentos siguientes:
#'
#'ANCOVA(y, tratamiento, bloque, covariable, significancia, data)
#'
#'# Argumentamos la función de acuerdo a nuestros datos:
#'
#'  ANCOVA("producción", "tratamiento","bloque", "Nplantas", 0.05, df)
#'
#'
#'# Paso 3: Usted tendrá consigo dos tablas del análisis de covarianza de sus datos:
#'
#'Una tabla será de la suma de cuadrados y la otra de las F
#'calculadas y tabuladas tanto para la covarianza como  para el tratamiento.
#'Las tablas de las F calculadas y tabuladas le anexará el valor de **coeficiente de covarianza** (COF_COVARIANZA)
#' el cual servirá para hacer el ajuste de medias correspondiente si su covariable ha resultado significativa.
#'Para hacer este ajuste de medias usted puede usar la función "ANCOVA_AJUST" que alberga este paquete;
#' Para el uso de esta función se necesita lo siguiente:
#'
#'ANCOVA_AJUST <- function(coef_covarianza, y, tratamiento, bloque, covariable, data)
#'
#'Como puede ver es basicamente similar a la función ANCOVA, solo que aquí no tendrá que agregar
#'la significancia pero si el coeficiente de covarianza que le arroje la función principal o bien, que
#'haya obtenido de otra fuente; destacar que esta función es independiente de la función ANCOVA,
#'PUEDE USARLA SIN HABER USADO ANTES LA FUNCIÓN ANCOVA.
#'
#'# SIGUIENDO CON EL EJEMPLO:
#'La función se argumentaria así suponiendo que el coeficiente es:0.1237089
#'
#'ANCOVA_AJUST(0.1247089, "producción","tratamiento","bloque", "Nplantas", df)
#'
#'Una vez argumentada nuestra función, deberá ejecutarla y le regresará el valor de las medias ajustadas para sus tratamientos.
ANCOVA_AJUST <- function(coef_covarianza, y, tratamiento, bloque, covariable, data){
  trat <- factor(data[,tratamiento])
  bloque <- factor(data[, bloque])
  cov <- data[, covariable]
  (y <- data[,y])
  (t <- nlevels(trat))
  (r <- nlevels(bloque))

  #Hacemos ajuste de las medias haciendo primero, una sumatoria de de todas las respuestas por tratamiento en los bloques,
  #tambien de todos los datos de la covariable.

  #Totales de tratamiento
  (Totales_tratx <- tapply(cov, INDEX = trat, FUN = sum))
  (Totales_traty <- tapply(y, INDEX = trat, FUN = sum))

  #medias de tratamientos
  (MTx <- tapply(cov, INDEX = trat, FUN = mean))
  (MTy <- tapply(y, INDEX = trat, FUN = mean))

  #Media general de todas las observaciones de la covariable
  (SUMxBLOQUE <- tapply(cov, INDEX = bloque, FUN = sum))
  (SUMTBLOQUE <- sum(SUMxBLOQUE))
  (X.. <- SUMTBLOQUE/(r*t))

  #se calcula la media ajustada por covariable
  (COEF <- coef_covarianza)
  (MOBSERVACIONES <- MTy- (COEF)*(MTx-X..))

  #data.frame
  df <- data.frame(MEDIAS_AJUSTADAS_PARA_TRATAMIENTOS = c(MOBSERVACIONES))
  print(df)
}
