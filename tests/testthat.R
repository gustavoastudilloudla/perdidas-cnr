# Archivo principal para ejecutar las pruebas con testthat
# Ubicación: tests/testthat.R

# Cargar librerías necesarias
library(testthat)

# Cargar las funciones del documento principal
# Nota: En un paquete real, esto se haría automáticamente
# Para este caso, necesitamos cargar las funciones desde el documento Quarto

# Opción 1: Si las funciones están en un archivo .R separado
# source("../../R/analizar_desempeno_perdidas.R")

# Opción 2: Para testing en el contexto del documento Quarto
# Las funciones ya estarán cargadas en el entorno cuando se ejecute desde el documento

# Verificar que las funciones estén disponibles
if (!exists("analizar_desempeno_perdidas")) {
  stop("La función analizar_desempeno_perdidas no está disponible. 
       Asegúrate de ejecutar primero los chunks que definen la función.")
}

# Ejecutar todas las pruebas en el directorio testthat/
test_check("analisis_perdidas")