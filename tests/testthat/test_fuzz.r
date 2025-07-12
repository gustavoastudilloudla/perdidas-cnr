# Test: Pruebas aleatorias (Fuzz Testing)
# Ubicación: tests/testthat/test-fuzz.R
# Objetivo: Verificar robustez de la función con entradas aleatorias y casos extremos

test_that("Función maneja correctamente 100 vectores aleatorios", {
  # Generar 100 vectores con diferentes características
  set.seed(42)
  
  for (i in 1:100) {
    # Variar tamaño del vector
    n <- sample(c(10, 50, 100, 500, 1000), 1)
    
    # Variar distribución
    tipo_dist <- sample(c("poisson", "normal", "uniforme", "binomial"), 1)
    
    vector_test <- switch(tipo_dist,
      "poisson" = rpois(n, lambda = runif(1, 0.5, 5)),
      "normal" = round(abs(rnorm(n, mean = runif(1, 0, 10), sd = runif(1, 1, 5)))),
      "uniforme" = round(runif(n, min = 0, max = 10)),
      "binomial" = rbinom(n, size = 10, prob = runif(1, 0.1, 0.9))
    )
    
    # Añadir algunos NA aleatoriamente (10% de probabilidad)
    if (runif(1) < 0.1) {
      n_na <- sample(1:min(10, n/2), 1)
      vector_test[sample(1:n, n_na)] <- NA
    }
    
    # La función no debe fallar
    expect_error(
      resultado <- analizar_desempeno_perdidas(vector_test),
      NA,  # NA significa que NO esperamos error
      info = paste("Falló con vector", i, "tipo:", tipo_dist, "n:", n)
    )
    
    # Verificar que devuelve la clase correcta
    expect_s3_class(resultado, "desempeno_perdidas")
  }
})

test_that("Función maneja correctamente 100 matrices aleatorias", {
  set.seed(123)
  
  for (i in 1:100) {
    # Variar dimensiones
    n_filas <- sample(2:20, 1)
    n_cols <- sample(5:50, 1)
    
    # Variar distribución
    tipo_dist <- sample(c("poisson", "normal", "uniforme"), 1)
    
    valores <- switch(tipo_dist,
      "poisson" = rpois(n_filas * n_cols, lambda = runif(1, 0.5, 3)),
      "normal" = round(abs(rnorm(n_filas * n_cols, mean = runif(1, 1, 5)))),
      "uniforme" = round(runif(n_filas * n_cols, min = 0, max = 10))
    )
    
    matriz_test <- matrix(valores, nrow = n_filas, ncol = n_cols)
    
    # Añadir nombres aleatorios (50% probabilidad)
    if (runif(1) < 0.5) {
      rownames(matriz_test) <- paste("Fila", 1:n_filas)
      colnames(matriz_test) <- paste("Col", 1:n_cols)
    }
    
    # La función no debe fallar
    expect_error(
      resultado <- analizar_desempeno_perdidas(matriz_test),
      NA,
      info = paste("Falló con matriz", i, "dims:", n_filas, "x", n_cols)
    )
    
    # Verificar resultado
    expect_s3_class(resultado, "desempeno_perdidas")
    expect_equal(resultado$estadisticas_globales$tipo_entrada, "matrix")
  }
})

test_that("Función maneja data.frames con características aleatorias", {
  set.seed(456)
  
  # Generar 50 data.frames con diferentes características
  for (i in 1:50) {
    # Variar número de filas y técnicos
    n_filas <- sample(c(50, 100, 500, 1000, 5000), 1)
    n_tecnicos <- sample(2:20, 1)
    
    # Variar período de tiempo
    fecha_inicio <- as.Date("2024-01-01") + sample(-90:0, 1)
    dias_periodo <- sample(30:180, 1)
    fechas_posibles <- seq(fecha_inicio, fecha_inicio + dias_periodo, by = "day")
    
    # Crear data.frame
    datos_test <- data.frame(
      Nombre_asignado = sample(paste("Técnico", 1:n_tecnicos), n_filas, replace = TRUE),
      Resultado_visita = sample(
        c("CNR", "Normal", "Visita fallida", "Mantenimiento Medidor"),
        n_filas, 
        replace = TRUE,
        prob = c(
          runif(1, 0.05, 0.3),   # CNR
          runif(1, 0.4, 0.7),    # Normal
          runif(1, 0.1, 0.3),    # Visita fallida
          runif(1, 0.01, 0.1)    # Mantenimiento
        ) |> (\(x) x/sum(x))()  # Normalizar probabilidades
      ),
      Tipo_de_CNR = sample(c("Directo", "Bypass", "Manipulación", "-"), 
                          n_filas, replace = TRUE),
      Fecha_ejecución = sample(fechas_posibles, n_filas, replace = TRUE)
    )
    
    # Añadir algunos NA (10% probabilidad)
    if (runif(1) < 0.1) {
      cols_na <- sample(names(datos_test), sample(1:2, 1))
      for (col in cols_na) {
        n_na <- sample(1:min(10, n_filas/10), 1)
        datos_test[sample(1:n_filas, n_na), col] <- NA
      }
    }
    
    # Parámetros aleatorios
    params <- list(
      min_casos = sample(c(5, 10, 20, 50), 1),
      visitas_efectivas_dia = runif(1, 3, 12),
      cnr_esperados_dia = runif(1, 0.5, 4),
      metodo_anomalias = sample(c("tukey", "zscore", "none"), 1),
      usar_data_table = sample(c(TRUE, FALSE, NULL), 1),
      verbose = FALSE
    )
    
    # La función no debe fallar
    expect_error(
      resultado <- do.call(analizar_desempeno_perdidas, c(list(data = datos_test), params)),
      NA,
      info = paste("Falló con df", i, "filas:", n_filas, "tecnicos:", n_tecnicos)
    )
    
    # Verificaciones básicas
    expect_s3_class(resultado, "desempeno_perdidas")
    expect_gte(nrow(resultado$metricas_individuales), 0)
  }
})

test_that("Función maneja casos extremos sin fallar", {
  
  # Caso 1: Vector con un solo valor
  expect_error(analizar_desempeno_perdidas(c(5)), NA)
  
  # Caso 2: Vector con todos valores iguales
  expect_error(analizar_desempeno_perdidas(rep(2, 100)), NA)
  
  # Caso 3: Vector con todos NA
  expect_error(
    analizar_desempeno_perdidas(rep(NA_real_, 50)),
    "El vector solo contiene valores NA"
  )
  
  # Caso 4: Matriz 1x1
  expect_error(analizar_desempeno_perdidas(matrix(5)), NA)
  
  # Caso 5: Matriz con una sola fila
  expect_error(analizar_desempeno_perdidas(matrix(1:10, nrow = 1)), NA)
  
  # Caso 6: Matriz con una sola columna
  expect_error(analizar_desempeno_perdidas(matrix(1:10, ncol = 1)), NA)
  
  # Caso 7: Data.frame con un solo técnico
  df_un_tecnico <- data.frame(
    Nombre_asignado = rep("Técnico A", 100),
    Resultado_visita = sample(c("CNR", "Normal"), 100, replace = TRUE),
    Tipo_de_CNR = sample(c("Directo", "-"), 100, replace = TRUE),
    Fecha_ejecución = as.Date("2024-01-01") + sample(0:30, 100, replace = TRUE)
  )
  expect_error(
    resultado <- analizar_desempeno_perdidas(df_un_tecnico, verbose = FALSE),
    NA
  )
  expect_equal(nrow(resultado$metricas_individuales), 1)
  
  # Caso 8: Data.frame con fechas en un solo día
  df_un_dia <- data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:5]), 50, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal"), 50, replace = TRUE),
    Tipo_de_CNR = sample(c("Directo", "-"), 50, replace = TRUE),
    Fecha_ejecución = as.Date("2024-01-15")  # Todas las mismas
  )
  expect_error(analizar_desempeno_perdidas(df_un_dia, verbose = FALSE), NA)
})

test_that("Función maneja valores extremos en parámetros", {
  datos_test <- data.frame(
    Nombre_asignado = sample(paste("Técnico", LETTERS[1:5]), 200, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal"), 200, replace = TRUE),
    Tipo_de_CNR = sample(c("Directo", "-"), 200, replace = TRUE),
    Fecha_ejecución = sample(seq(as.Date("2024-01-01"), 
                                as.Date("2024-03-31"), by = "day"), 200, replace = TRUE)
  )
  
  # min_casos muy alto
  expect_error(
    resultado <- analizar_desempeno_perdidas(datos_test, min_casos = 1000, verbose = FALSE),
    NA
  )
  expect_equal(nrow(resultado$metricas_individuales), 0)  # Ningún técnico cumple
  
  # Metas muy altas
  expect_error(
    analizar_desempeno_perdidas(
      datos_test, 
      visitas_efectivas_dia = 1000,
      cnr_esperados_dia = 500,
      verbose = FALSE
    ),
    NA
  )
  
  # umbral_zscore extremo
  expect_error(
    analizar_desempeno_perdidas(
      datos_test,
      metodo_anomalias = "zscore",
      umbral_zscore = 0.01,  # Muy restrictivo
      verbose = FALSE
    ),
    NA
  )
  
  # Fechas invertidas
  expect_warning(
    analizar_desempeno_perdidas(
      datos_test,
      fecha_inicio = as.Date("2024-03-01"),
      fecha_fin = as.Date("2024-01-01"),
      verbose = FALSE
    ),
    "Fecha inicio posterior a fecha fin"
  )
})

test_that("Métodos S3 funcionan con resultados de casos aleatorios", {
  set.seed(789)
  
  # Generar 20 casos aleatorios y probar métodos S3
  for (i in 1:20) {
    # Tipo aleatorio
    tipo <- sample(c("vector", "matriz", "dataframe"), 1)
    
    if (tipo == "vector") {
      datos <- rpois(sample(50:200, 1), lambda = runif(1, 1, 3))
    } else if (tipo == "matriz") {
      dims <- c(sample(3:10, 1), sample(10:30, 1))
      datos <- matrix(rpois(prod(dims), lambda = 2), nrow = dims[1])
    } else {
      n <- sample(100:500, 1)
      datos <- data.frame(
        Nombre_asignado = sample(paste("T", 1:8), n, replace = TRUE),
        Resultado_visita = sample(c("CNR", "Normal", "Visita fallida"), n, replace = TRUE),
        Tipo_de_CNR = sample(c("Directo", "-"), n, replace = TRUE),
        Fecha_ejecución = sample(seq(as.Date("2024-01-01"), 
                                    as.Date("2024-02-29"), by = "day"), n, replace = TRUE)
      )
    }
    
    resultado <- analizar_desempeno_perdidas(datos, verbose = FALSE)
    
    # print no debe fallar
    expect_error(capture.output(print(resultado)), NA)
    
    # summary no debe fallar
    expect_error(capture.output(summary(resultado)), NA)
    
    # plot no debe fallar (solo para dataframes)
    if (tipo == "dataframe") {
      expect_error(plot(resultado), NA)
    }
    
    # as_tibble no debe fallar
    expect_error(as_tibble(resultado), NA)
  }
})

test_that("Función es robusta ante caracteres especiales y encoding", {
  # Crear datos con caracteres especiales
  datos_test <- data.frame(
    Nombre_asignado = c(
      "Técnico Ñáñez", 
      "José María", 
      "O'Brien",
      "Müller-Schmidt",
      "李明",  # Caracteres chinos
      "Técnico #1",
      "Smith & Jones"
    ) |> rep(each = 20),
    Resultado_visita = sample(c("CNR", "Normal"), 140, replace = TRUE),
    Tipo_de_CNR = sample(c("Directo", "Indirecto/Bypass", "Manipulación", "-"), 
                        140, replace = TRUE),
    Fecha_ejecución = sample(seq(as.Date("2024-01-01"), 
                                as.Date("2024-01-31"), by = "day"), 140, replace = TRUE)
  )
  
  # No debe fallar con caracteres especiales
  expect_error(
    resultado <- analizar_desempeno_perdidas(datos_test, verbose = FALSE),
    NA
  )
  
  # Verificar que los nombres se preservan
  tecnicos_resultado <- resultado$metricas_individuales$Nombre_asignado
  expect_true(all(unique(datos_test$Nombre_asignado) %in% tecnicos_resultado))
})

test_that("Rendimiento es aceptable con datasets grandes", {
  skip_if_not(requireNamespace("data.table", quietly = TRUE), 
              "data.table no disponible para test de rendimiento")
  
  # Crear dataset grande
  set.seed(999)
  n_grande <- 100000
  
  datos_grandes <- data.frame(
    Nombre_asignado = sample(paste("Técnico", 1:50), n_grande, replace = TRUE),
    Resultado_visita = sample(c("CNR", "Normal", "Visita fallida"), 
                             n_grande, replace = TRUE, prob = c(0.15, 0.70, 0.15)),
    Tipo_de_CNR = sample(c("Directo", "-"), n_grande, replace = TRUE),
    Fecha_ejecución = sample(seq(as.Date("2023-01-01"), 
                                as.Date("2024-12-31"), by = "day"), n_grande, replace = TRUE)
  )
  
  # Medir tiempo
  tiempo_inicio <- Sys.time()
  
  expect_error(
    resultado <- analizar_desempeno_perdidas(
      datos_grandes, 
      usar_data_table = TRUE,  # Forzar data.table
      verbose = FALSE
    ),
    NA
  )
  
  tiempo_total <- difftime(Sys.time(), tiempo_inicio, units = "secs")
  
  # Debe completarse en tiempo razonable (menos de 10 segundos)
  expect_lt(as.numeric(tiempo_total), 10)
  
  # Verificar que usó data.table
  expect_equal(resultado$estadisticas_globales$metodo_procesamiento, "data.table")
})
