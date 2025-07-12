# Test: Verificar estructura de objetos devueltos
# Ubicación: tests/testthat/test-estructura.R
# Objetivo: Validar que la lista devuelta tenga todos los componentes esperados con la estructura correcta

# Función helper para crear datos de prueba
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

test_that("Estructura principal contiene todos los componentes requeridos", {
  # Preparar datos
  datos_test <- crear_datos_prueba()
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Componentes obligatorios
  componentes_requeridos <- c(
    "metricas_individuales", 
    "analisis_temporal", 
    "estadisticas_globales",
    "anomalias",
    "parametros",
    "datos_procesados"
  )
  
  # Verificar que todos los componentes existen
  for (comp in componentes_requeridos) {
    expect_true(comp %in% names(resultado), 
                info = paste("Falta el componente:", comp))
  }
  
  # Verificar que no hay componentes extra no esperados
  expect_setequal(names(resultado), componentes_requeridos)
})

test_that("metricas_individuales tiene todas las columnas esperadas", {
  # Preparar datos
  datos_test <- crear_datos_prueba(150)
  resultado <- analizar_desempeno_perdidas(datos_test, min_casos = 10, verbose = FALSE)
  
  # Columnas esperadas en metricas_individuales
  columnas_esperadas <- c(
    "Nombre_asignado", "total_casos", "casos_cnr", "casos_normal",
    "casos_fallidos", "casos_mant", "visitas_efectivas",
    "tasa_deteccion_cnr", "tasa_exito_visita", "fecha_primer",
    "fecha_ultimo", "dias_activo", "dias_trabajo",
    "visitas_efectivas_dia_real", "cnr_dia_real",
    "cumplimiento_visitas", "cumplimiento_cnr", "indice_eficiencia",
    "promedio_visitas_dia", "promedio_cnr_dia", "promedio_efectivas_dia",
    "max_visitas_dia", "max_cnr_dia", "dias_sin_cnr",
    "dias_meta_visitas", "dias_meta_cnr"
  )
  
  # Verificar columnas
  columnas_actuales <- names(resultado$metricas_individuales)
  for (col in columnas_esperadas) {
    expect_true(col %in% columnas_actuales,
                info = paste("Falta la columna:", col))
  }
  
  # Verificar tipos de datos de columnas clave
  expect_type(resultado$metricas_individuales$total_casos, "integer")
  expect_type(resultado$metricas_individuales$tasa_deteccion_cnr, "double")
  expect_type(resultado$metricas_individuales$indice_eficiencia, "double")
  expect_s3_class(resultado$metricas_individuales$fecha_primer, "Date")
})

test_that("analisis_temporal tiene estructura correcta", {
  # Preparar datos con múltiples meses
  set.seed(789)
  datos_test <- data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:5]), 500, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal", "Visita fallida", "Mantenimiento Medidor"), 
                             500, replace = TRUE, prob = c(0.15, 0.60, 0.20, 0.05)),
    Tipo_de_CNR = sample(c("Directo", "Bypass", "Manipulación", "-"), 500, replace = TRUE),
    Fecha_ejecución = sample(seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day"),
                            500, replace = TRUE)
  )
  
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Columnas esperadas en analisis_temporal
  columnas_temporales <- c(
    "mes", "total_insp", "total_cnr", "total_efectivas",
    "tasa_cnr_mensual", "tasa_efectividad", "dias_mes",
    "tecnicos_activos", "efectivas_dia_promedio", "cnr_dia_promedio"
  )
  
  # Verificar columnas
  for (col in columnas_temporales) {
    expect_true(col %in% names(resultado$analisis_temporal),
                info = paste("Falta columna temporal:", col))
  }
  
  # Verificar que mes es una fecha
  expect_s3_class(resultado$analisis_temporal$mes, "Date")
  
  # Verificar que las tasas están entre 0 y 100
  expect_true(all(resultado$analisis_temporal$tasa_cnr_mensual >= 0))
  expect_true(all(resultado$analisis_temporal$tasa_cnr_mensual <= 100))
})

test_that("estadisticas_globales contiene todos los elementos requeridos", {
  # Preparar datos
  datos_test <- crear_datos_prueba()
  resultado <- analizar_desempeno_perdidas(
    datos_test, 
    visitas_efectivas_dia = 8,
    cnr_esperados_dia = 2,
    verbose = FALSE
  )
  
  # Elementos esperados en estadisticas_globales
  elementos_esperados <- c(
    "total_inspecciones", "total_tecnicos", "total_cnr_detectados",
    "total_visitas_efectivas", "tasa_global_cnr", "tasa_global_efectividad",
    "periodo_analisis", "parametros_meta", "tipo_entrada", "metodo_procesamiento"
  )
  
  # Verificar elementos
  for (elem in elementos_esperados) {
    expect_true(elem %in% names(resultado$estadisticas_globales),
                info = paste("Falta elemento global:", elem))
  }
  
  # Verificar tipos
  expect_type(resultado$estadisticas_globales$total_inspecciones, "integer")
  expect_type(resultado$estadisticas_globales$tasa_global_cnr, "double")
  expect_type(resultado$estadisticas_globales$periodo_analisis, "character")
  
  # Verificar parametros_meta
  expect_equal(resultado$estadisticas_globales$parametros_meta$visitas_efectivas_dia, 8)
  expect_equal(resultado$estadisticas_globales$parametros_meta$cnr_esperados_dia, 2)
})

test_that("parametros almacena correctamente todos los valores de entrada", {
  # Ejecutar con parámetros específicos
  fecha_ini <- as.Date("2024-02-01")
  fecha_fin <- as.Date("2024-03-31")
  
  datos_test <- crear_datos_prueba()
  resultado <- analizar_desempeno_perdidas(
    datos_test,
    fecha_inicio = fecha_ini,
    fecha_fin = fecha_fin,
    min_casos = 15,
    visitas_efectivas_dia = 10,
    cnr_esperados_dia = 3,
    metodo_anomalias = "zscore",
    umbral_zscore = 2.5,
    verbose = FALSE
  )
  
  # Verificar que todos los parámetros se guardaron
  expect_equal(resultado$parametros$fecha_inicio, fecha_ini)
  expect_equal(resultado$parametros$fecha_fin, fecha_fin)
  expect_equal(resultado$parametros$min_casos, 15)
  expect_equal(resultado$parametros$visitas_efectivas_dia, 10)
  expect_equal(resultado$parametros$cnr_esperados_dia, 3)
  expect_equal(resultado$parametros$metodo_anomalias, "zscore")
  expect_equal(resultado$parametros$umbral_zscore, 2.5)
})

test_that("Estructura para vectores es correcta", {
  # Crear y analizar vector
  vector_test <- rpois(100, lambda = 2.5)
  resultado <- analizar_desempeno_perdidas(vector_test)
  
  # Verificar estructura específica para vectores
  expect_true("estadisticas_vector" %in% names(resultado))
  
  # Elementos esperados en estadisticas_vector
  elementos_vector <- c(
    "n", "n_na", "media", "mediana", "desviacion",
    "varianza", "minimo", "maximo", "rango", "q1", "q3",
    "iqr", "cv", "suma", "n_outliers", "outliers"
  )
  
  for (elem in elementos_vector) {
    expect_true(elem %in% names(resultado$estadisticas_vector),
                info = paste("Falta elemento en vector:", elem))
  }
})

test_that("Estructura para matrices es correcta", {
  # Crear y analizar matriz
  matriz_test <- matrix(rpois(90, lambda = 1.5), nrow = 3, ncol = 30)
  resultado <- analizar_desempeno_perdidas(matriz_test)
  
  # Verificar estructura específica para matrices
  expect_true("estadisticas_vector" %in% names(resultado))
  expect_true("dimensiones" %in% names(resultado$estadisticas_vector))
  expect_true("estadisticas_filas" %in% names(resultado$estadisticas_vector))
  expect_true("estadisticas_columnas" %in% names(resultado$estadisticas_vector))
  
  # Verificar dimensiones
  expect_equal(resultado$estadisticas_vector$dimensiones, c(3, 30))
  
  # Verificar data frames de estadísticas
  expect_s3_class(resultado$estadisticas_vector$estadisticas_filas, "data.frame")
  expect_s3_class(resultado$estadisticas_vector$estadisticas_columnas, "data.frame")
})

test_that("anomalias tiene estructura correcta cuando hay anomalías", {
  # Crear datos con valores extremos para generar anomalías
  set.seed(999)
  datos_test <- crear_datos_prueba(200)
  
  # Forzar algunos valores extremos
  indices_extremos <- sample(1:200, 20)
  datos_test$Resultado_visita[indices_extremos] <- "CNR"
  
  resultado <- analizar_desempeno_perdidas(
    datos_test, 
    metodo_anomalias = "tukey",
    verbose = FALSE
  )
  
  # Si hay anomalías, verificar estructura
  if (nrow(resultado$anomalias) > 0) {
    # Debe tener todas las columnas de metricas_individuales más tipo_anomalia
    expect_true("tipo_anomalia" %in% names(resultado$anomalias))
    
    # Verificar que tipo_anomalia tiene valores válidos
    tipos_validos <- c("Tasa CNR baja", "Tasa CNR alta", 
                      "Baja eficiencia", "Valor atípico (Z-score)", "Múltiple")
    expect_true(all(resultado$anomalias$tipo_anomalia %in% tipos_validos))
  }
})
