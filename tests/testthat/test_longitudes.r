# Test: Verificar coherencia de longitudes
# Ubicación: tests/testthat/test-longitudes.R  
# Objetivo: Asegurar que las longitudes y dimensiones de los componentes sean coherentes

# Función helper para crear datos de prueba
crear_datos_prueba <- function(n = 100, n_tecnicos = 5) {
  set.seed(123)
  data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:n_tecnicos]), n, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal", "Visita fallida", "Mantenimiento Medidor"), 
                             n, replace = TRUE, prob = c(0.15, 0.60, 0.20, 0.05)),
    Tipo_de_CNR = sample(c("Directo", "Bypass", "Manipulación", "-"), n, replace = TRUE),
    Fecha_ejecución = sample(seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
                            n, replace = TRUE)
  )
}

test_that("metricas_individuales tiene longitud coherente con técnicos únicos", {
  # Crear datos con número conocido de técnicos
  n_tecnicos <- 8
  datos_test <- crear_datos_prueba(n = 200, n_tecnicos = n_tecnicos)
  
  resultado <- analizar_desempeno_perdidas(
    datos_test, 
    min_casos = 5,  # Bajo para incluir más técnicos
    verbose = FALSE
  )
  
  # El número de filas debe ser <= número de técnicos únicos
  expect_lte(nrow(resultado$metricas_individuales), n_tecnicos)
  
  # Debe haber al menos 1 técnico (si hay datos)
  expect_gte(nrow(resultado$metricas_individuales), 1)
  
  # Cada técnico debe aparecer solo una vez
  expect_equal(length(unique(resultado$metricas_individuales$Nombre_asignado)),
               nrow(resultado$metricas_individuales))
})

test_that("analisis_temporal tiene longitud coherente con período de datos", {
  # Crear datos con período específico (3 meses)
  set.seed(456)
  fechas <- seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day")
  n_datos <- 300
  
  datos_test <- data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:5]), n_datos, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal", "Visita fallida"), n_datos, replace = TRUE),
    Tipo_de_CNR = sample(c("Directo", "Bypass", "-"), n_datos, replace = TRUE),
    Fecha_ejecución = sample(fechas, n_datos, replace = TRUE)
  )
  
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Debe haber exactamente 3 filas (enero, febrero, marzo)
  expect_equal(nrow(resultado$analisis_temporal), 3)
  
  # Los meses deben ser consecutivos
  meses <- resultado$analisis_temporal$mes
  expect_equal(month(meses), 1:3)
})

test_that("Filtrado por min_casos reduce correctamente las filas", {
  # Crear datos donde algunos técnicos tengan pocos casos
  set.seed(789)
  datos_test <- rbind(
    # Técnico A: muchos casos
    data.frame(
      Nombre_asignado = rep("Técnico A", 50),
      Resultado_visita = sample(c("CNR", "Normal"), 50, replace = TRUE),
      Tipo_de_CNR = sample(c("Directo", "-"), 50, replace = TRUE),
      Fecha_ejecución = sample(seq(as.Date("2024-01-01"), 
                                  as.Date("2024-01-31"), by = "day"), 50, replace = TRUE)
    ),
    # Técnico B: pocos casos
    data.frame(
      Nombre_asignado = rep("Técnico B", 5),
      Resultado_visita = sample(c("CNR", "Normal"), 5, replace = TRUE),
      Tipo_de_CNR = sample(c("Directo", "-"), 5, replace = TRUE),
      Fecha_ejecución = sample(seq(as.Date("2024-01-01"), 
                                  as.Date("2024-01-31"), by = "day"), 5, replace = TRUE)
    ),
    # Técnico C: casos intermedios
    data.frame(
      Nombre_asignado = rep("Técnico C", 25),
      Resultado_visita = sample(c("CNR", "Normal"), 25, replace = TRUE),
      Tipo_de_CNR = sample(c("Directo", "-"), 25, replace = TRUE),
      Fecha_ejecución = sample(seq(as.Date("2024-01-01"), 
                                  as.Date("2024-01-31"), by = "day"), 25, replace = TRUE)
    )
  )
  
  # Test con diferentes valores de min_casos
  resultado_10 <- analizar_desempeno_perdidas(datos_test, min_casos = 10, verbose = FALSE)
  resultado_30 <- analizar_desempeno_perdidas(datos_test, min_casos = 30, verbose = FALSE)
  
  # Con min_casos = 10: técnicos A y C (2 técnicos)
  expect_equal(nrow(resultado_10$metricas_individuales), 2)
  expect_setequal(resultado_10$metricas_individuales$Nombre_asignado, 
                  c("Técnico A", "Técnico C"))
  
  # Con min_casos = 30: solo técnico A (1 técnico)
  expect_equal(nrow(resultado_30$metricas_individuales), 1)
  expect_equal(resultado_30$metricas_individuales$Nombre_asignado, "Técnico A")
})

test_that("datos_procesados mantiene coherencia con datos originales", {
  # Crear datos con algunos NA
  datos_test <- crear_datos_prueba(150)
  
  # Añadir algunos NA
  datos_test$Nombre_asignado[c(1, 5, 10)] <- NA
  datos_test$Fecha_ejecución[c(2, 8)] <- NA
  
  n_original <- nrow(datos_test)
  n_na_nombre <- sum(is.na(datos_test$Nombre_asignado))
  n_na_fecha <- sum(is.na(datos_test$Fecha_ejecución))
  
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # datos_procesados debe tener menos filas (sin NA)
  expect_lt(nrow(resultado$datos_procesados), n_original)
  
  # La reducción debe ser coherente con los NA
  n_esperado <- n_original - max(n_na_nombre, n_na_fecha)
  expect_lte(nrow(resultado$datos_procesados), n_esperado)
  
  # No debe haber NA en columnas críticas
  expect_equal(sum(is.na(resultado$datos_procesados$Nombre_asignado)), 0)
  expect_equal(sum(is.na(resultado$datos_procesados$Fecha_ejecución)), 0)
})

test_that("Longitudes son coherentes entre métricas diarias y resumen", {
  # Preparar datos
  datos_test <- crear_datos_prueba(300, n_tecnicos = 5)
  resultado <- analizar_desempeno_perdidas(datos_test, min_casos = 10, verbose = FALSE)
  
  # Para cada técnico en metricas_individuales
  for (i in 1:nrow(resultado$metricas_individuales)) {
    tecnico <- resultado$metricas_individuales$Nombre_asignado[i]
    
    # Verificar coherencia de días trabajados
    dias_trabajo <- resultado$metricas_individuales$dias_trabajo[i]
    dias_activo <- resultado$metricas_individuales$dias_activo[i]
    
    # días_trabajo <= días_activo (puede trabajar menos días que el período activo)
    expect_lte(dias_trabajo, dias_activo)
    
    # días sin CNR <= días trabajados
    dias_sin_cnr <- resultado$metricas_individuales$dias_sin_cnr[i]
    expect_lte(dias_sin_cnr, dias_trabajo)
    
    # días que cumplió metas <= días trabajados
    dias_meta_visitas <- resultado$metricas_individuales$dias_meta_visitas[i]
    dias_meta_cnr <- resultado$metricas_individuales$dias_meta_cnr[i]
    expect_lte(dias_meta_visitas, dias_trabajo)
    expect_lte(dias_meta_cnr, dias_trabajo)
  }
})

test_that("estadisticas_globales tienen valores coherentes con los datos", {
  # Preparar datos
  datos_test <- crear_datos_prueba(500, n_tecnicos = 10)
  resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Total inspecciones = filas en datos_procesados
  expect_equal(resultado$estadisticas_globales$total_inspecciones,
               nrow(resultado$datos_procesados))
  
  # Total CNR <= total inspecciones
  expect_lte(resultado$estadisticas_globales$total_cnr_detectados,
             resultado$estadisticas_globales$total_inspecciones)
  
  # Total visitas efectivas <= total inspecciones
  expect_lte(resultado$estadisticas_globales$total_visitas_efectivas,
             resultado$estadisticas_globales$total_inspecciones)
  
  # Tasas entre 0 y 100
  expect_gte(resultado$estadisticas_globales$tasa_global_cnr, 0)
  expect_lte(resultado$estadisticas_globales$tasa_global_cnr, 100)
  expect_gte(resultado$estadisticas_globales$tasa_global_efectividad, 0)
  expect_lte(resultado$estadisticas_globales$tasa_global_efectividad, 100)
})

test_that("Longitudes de vectores analizados son correctas", {
  # Vector con NAs
  vector_test <- c(rpois(80, lambda = 2), rep(NA, 20))
  resultado <- analizar_desempeno_perdidas(vector_test)
  
  # Verificar que n + n_na = longitud original
  expect_equal(
    resultado$estadisticas_vector$n + resultado$estadisticas_vector$n_na,
    length(vector_test)
  )
  
  # Verificar outliers
  n_outliers <- resultado$estadisticas_vector$n_outliers
  expect_gte(n_outliers, 0)
  expect_lte(n_outliers, resultado$estadisticas_vector$n)
  
  # Longitud de outliers debe coincidir con n_outliers
  expect_equal(length(resultado$estadisticas_vector$outliers), n_outliers)
})

test_that("Dimensiones de matrices son preservadas correctamente", {
  # Matriz de prueba
  filas <- 6
  columnas <- 20
  matriz_test <- matrix(rpois(filas * columnas, lambda = 1.5), 
                       nrow = filas, ncol = columnas)
  
  resultado <- analizar_desempeno_perdidas(matriz_test)
  
  # Verificar dimensiones
  expect_equal(resultado$estadisticas_vector$dimensiones, c(filas, columnas))
  
  # estadisticas_filas debe tener tantas filas como la matriz original
  expect_equal(nrow(resultado$estadisticas_vector$estadisticas_filas), filas)
  
  # estadisticas_columnas debe tener tantas filas como columnas originales
  expect_equal(nrow(resultado$estadisticas_vector$estadisticas_columnas), columnas)
  
  # metricas_individuales debe ser igual a estadisticas_filas para matrices
  expect_equal(nrow(resultado$metricas_individuales), filas)
})

test_that("Filtrado por fechas reduce correctamente los datos", {
  # Crear datos con rango amplio de fechas
  fechas_todas <- seq(as.Date("2024-01-01"), as.Date("2024-06-30"), by = "day")
  n_datos <- 600
  
  datos_test <- data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:5]), n_datos, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal"), n_datos, replace = TRUE),
    Tipo_de_CNR = sample(c("Directo", "-"), n_datos, replace = TRUE),
    Fecha_ejecución = sample(fechas_todas, n_datos, replace = TRUE)
  )
  
  # Sin filtro
  resultado_total <- analizar_desempeno_perdidas(datos_test, verbose = FALSE)
  
  # Con filtro de fechas (solo febrero)
  resultado_febrero <- analizar_desempeno_perdidas(
    datos_test,
    fecha_inicio = as.Date("2024-02-01"),
    fecha_fin = as.Date("2024-02-29"),
    verbose = FALSE
  )
  
  # Debe haber menos datos en febrero
  expect_lt(nrow(resultado_febrero$datos_procesados), 
           nrow(resultado_total$datos_procesados))
  
  # analisis_temporal debe tener solo 1 mes
  expect_equal(nrow(resultado_febrero$analisis_temporal), 1)
  expect_equal(month(resultado_febrero$analisis_temporal$mes), 2)
})
