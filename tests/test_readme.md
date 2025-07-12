# Sistema de Pruebas para analizar_desempeno_perdidas()

## Estructura de Directorios

```
tests/
├── testthat.R          # Archivo principal para ejecutar todas las pruebas
├── README.md           # Este archivo
└── testthat/
    ├── test-clase.R      # Pruebas de clases y tipos
    ├── test-estructura.R # Pruebas de estructura de datos
    ├── test-longitudes.R # Pruebas de coherencia de longitudes
    └── test-fuzz.R       # Pruebas aleatorias (fuzz testing)
```

## Descripción de las Pruebas

### 1. test-clase.R
- Verifica que la función devuelva objetos con la clase correcta (`desempeno_perdidas`)
- Valida que todos los métodos S3 estén disponibles y funcionen
- Prueba el manejo de vectores, matrices y data frames
- Asegura que el procesamiento con data.table mantenga las clases correctas

### 2. test-estructura.R
- Valida la estructura completa del objeto devuelto
- Verifica que todos los componentes requeridos estén presentes
- Comprueba que cada componente tenga las columnas/elementos esperados
- Valida tipos de datos internos

### 3. test-longitudes.R
- Asegura coherencia entre las dimensiones de los componentes
- Verifica que el filtrado funcione correctamente
- Valida la relación entre técnicos únicos y filas en métricas
- Comprueba consistencia temporal en los análisis

### 4. test-fuzz.R
- Ejecuta 100+ pruebas con entradas aleatorias
- Prueba casos extremos y valores límite
- Verifica robustez con caracteres especiales
- Evalúa rendimiento con datasets grandes

## Cómo Ejecutar las Pruebas

### Opción 1: Desde el documento Quarto
```r
# En el chunk 'run-tests', cambiar eval = FALSE a eval = TRUE
# Luego hacer knit del documento
```

### Opción 2: Desde la consola de R
```r
# Cargar las funciones primero
source("analizar_desempeno_perdidas.R")  # o ejecutar los chunks del documento

# Ejecutar todas las pruebas
library(testthat)
test_dir("tests/testthat")

# Ejecutar un archivo específico
test_file("tests/testthat/test-clase.R")
```

### Opción 3: Con RStudio
- Click derecho en la carpeta `tests/testthat`
- Seleccionar "Run Tests"

## Cobertura de Código

Para medir la cobertura:
```r
library(covr)

# Si las funciones están en un archivo .R
cov <- file_coverage("analizar_desempeno_perdidas.R", "tests/testthat")

# Generar reporte HTML
report(cov, file = "coverage-report.html")

# Ver resumen en consola
print(cov)
```

## Benchmarking

Para comparar rendimiento dplyr vs data.table:
```r
library(microbenchmark)

# Preparar datos de diferentes tamaños
datos_small <- crear_datos_prueba(1000)
datos_large <- crear_datos_prueba(60000)

# Benchmark
microbenchmark(
  dplyr_small = analizar_desempeno_perdidas(datos_small, usar_data_table = FALSE),
  dt_small = analizar_desempeno_perdidas(datos_small, usar_data_table = TRUE),
  dplyr_large = analizar_desempeno_perdidas(datos_large, usar_data_table = FALSE),
  dt_large = analizar_desempeno_perdidas(datos_large, usar_data_table = TRUE),
  times = 10
)
```

## Interpretación de Resultados

### Éxito Total
- ✅ Todas las pruebas pasan (PASSED)
- ✅ Cobertura > 92%
- ✅ Sin warnings inesperados

### Problemas Comunes
1. **Error: "función no encontrada"**
   - Asegurarse de cargar las funciones antes de ejecutar tests
   
2. **Error en pruebas de data.table**
   - Verificar que el paquete data.table esté instalado
   
3. **Timeouts en test-fuzz.R**
   - Normal con datasets muy grandes, considerar skip_on_cran()

## Mantenimiento

- Agregar nuevas pruebas cuando se añadan features
- Actualizar pruebas si cambia la estructura de salida
- Ejecutar pruebas antes de cada commit importante
- Mantener cobertura > 92%

## Dependencias

Las pruebas requieren:
- testthat (>= 3.0.0)
- covr (para cobertura)
- microbenchmark (para benchmarking)
- Todos los paquetes requeridos por la función principal
