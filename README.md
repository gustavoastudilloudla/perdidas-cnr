# Análisis de Desempeño en Detección de Pérdidas Eléctricas (CNR)

> **Versión:** v6 · **Autor:** Gustavo Astudillo P.
> **UDLA** (ACI1018EL) Programacíon en R.
> Quarto + R (tidyverse / data.table) · Cobertura 94 %

<div align="center">
  <img alt="dashboard preview" src="figs/dashboard_demo.png" width="650"/>
</div>

## Descripción

Este repositorio contiene el _workflow_ completo para analizar la eficacia de los técnicos en la detección de **Consumos No Registrados (CNR)**:

* Carga y limpieza de datos
* Función `analizar_desempeno_perdidas()` (vector · matriz · data frame)
* Detección de anomalías (Tukey / Z-score)
* Tests unitarios `testthat` + fuzz-testing (≈ 900 + cases)
* Cobertura de código > 92 % con **covr**
* Benchmark `microbenchmark` dplyr vs data.table  
* Documento **Quarto PDF/HTML** con dashboard y conclusiones   

## Arquitectura del análisis

```mermaid
flowchart TD
    A["Ingreso de datos<br>(CSV o Excel)"] --> B["Limpieza y normalización"]
    B --> C["Llamada a funcion&nbsp;principal"]
    C --> D["Creación de objeto S3"]
    D --> E["Visualizaciones<br>ggplot2 / patchwork"]
    C --> F["Pruebas unitarias<br>testthat"]
    C --> G["Benchmark & cobertura"]
    E --> H["Reporte Quarto"]
    F --> H
    G --> H
```
