# Test: Verificar clases de objetos devueltos
# Ubicación: tests/testthat/test-clase.R
# Objetivo: Asegurar que la función devuelve objetos con las clases correctas

# Cargar datos de prueba helper
crear_datos_prueba <- function(n = 100) {
  set.seed(123)
  data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:5]), n, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal", "Visita fallida", "Mantenimiento Medidor"), 
                             n, replace = TRUE, prob = c(0.15, 0.60, 0.20, 0.05)),
    Tipo_de_CNR = sample(c("Directo", "Bypass", "Manipulación", "-"), n, replace = TRUE),
    Fecha_ejecución = sample(seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
                            n, replace = TRUE)
  )
}

test_that("analizar_desempeno_perdidas devuelve objeto con clase correcta", {
  # Preparar datos de prueba
  datos_test <- crear_datos_prueba()
  
  # Ejecutar función
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Verificar clase principal
  expect_s3_class(resultado, "desempeno_perdidas")
  expect_s3_class(resultado, "list")
  
  # Verificar que tiene ambas clases
  expect_equal(class(resultado), c("desempeno_perdidas", "list"))
})

test_that("Métodos S3 están disponibles para la clase desempeno_perdidas", {
  # Preparar datos
  datos_test <- crear_datos_prueba()
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Verificar que los métodos existen
  expect_true(exists("print.desempeno_perdidas"))
  expect_true(exists("summary.desempeno_perdidas"))
  expect_true(exists("plot.desempeno_perdidas"))
  expect_true(exists("as_tibble.desempeno_perdidas"))
  
  # Verificar que los métodos se pueden ejecutar sin errores
  expect_silent(capture.output(print(resultado)))
  expect_silent(capture.output(summary(resultado)))
  expect_s3_class(plot(resultado), "gg")
  
  # as_tibble debe devolver un tibble/data.frame
  tibble_result <- as_tibble(resultado)
  expect_true(inherits(tibble_result, "data.frame") || inherits(tibble_result, "tbl_df"))
})

test_that("Función maneja correctamente vectores numéricos", {
  # Crear vector de prueba
  vector_test <- rpois(50, lambda = 2)
  
  # Ejecutar función
  resultado <- analizar_desempeno_perdidas(vector_test)
  
  # Verificar clase
  expect_s3_class(resultado, "desempeno_perdidas")
  expect_equal(resultado$estadisticas_globales$tipo_entrada, "vector")
  
  # Verificar que tiene estadísticas del vector
  expect_true("estadisticas_vector" %in% names(resultado))
  expect_type(resultado$estadisticas_vector, "list")
})

test_that("Función maneja correctamente matrices numéricas", {
  # Crear matriz de prueba
  set.seed(456)
  matriz_test <- matrix(
    rpois(150, lambda = 1.5), 
    nrow = 5, 
    ncol = 30,
    dimnames = list(paste("Técnico", 1:5), paste("Día", 1:30))
  )
  
  # Ejecutar función
  resultado <- analizar_desempeno_perdidas(matriz_test)
  
  # Verificar clase
  expect_s3_class(resultado, "desempeno_perdidas")
  expect_equal(resultado$estadisticas_globales$tipo_entrada, "matrix")
  
  # Verificar estructura específica de matriz
  expect_true("estadisticas_vector" %in% names(resultado))
  expect_true("estadisticas_filas" %in% names(resultado$estadisticas_vector))
  expect_true("estadisticas_columnas" %in% names(resultado$estadisticas_vector))
})

test_that("Clases internas son correctas para data.frame", {
  # Preparar datos
  datos_test <- crear_datos_prueba(200)
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Verificar clases de componentes internos
  expect_s3_class(resultado$metricas_individuales, "data.frame")
  expect_s3_class(resultado$analisis_temporal, "data.frame")
  expect_type(resultado$estadisticas_globales, "list")
  expect_s3_class(resultado$anomalias, "data.frame")
  expect_type(resultado$parametros, "list")
  expect_s3_class(resultado$datos_procesados, "data.frame")
})

test_that("Procesamiento con data.table mantiene las clases correctas", {
  # Crear dataset grande para forzar uso de data.table
  datos_grandes <- crear_datos_prueba(1000)
  
  # Forzar uso de data.table
  resultado_dt <- analizar_desempeno_perdidas(
    datos_grandes, 
    usar_data_table = TRUE, 
    verbose = FALSE
  )
  
  # Verificar que las clases son correctas (deben ser data.frame, no data.table)
  expect_s3_class(resultado_dt, "desempeno_perdidas")
  expect_s3_class(resultado_dt$metricas_individuales, "data.frame")
  expect_s3_class(resultado_dt$analisis_temporal, "data.frame")
  expect_equal(resultado_dt$estadisticas_globales$metodo_procesamiento, "data.table")
})
