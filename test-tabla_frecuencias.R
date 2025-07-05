library(testthat)

# Contexto para las pruebas de la función tabla_frecuencias
context("Pruebas para la función tabla_frecuencias")

# Crear un conjunto de datos de prueba
set.seed(123)
df_test <- data.frame(
  valores = c(1, 2, 2, 3, 3, 3, 4, 4, 5, NA, 10)
)

# Prueba 1: Validar que la función falla con un argumento 'data' no válido
test_that("Falla si 'data' no es un dataframe", {
  expect_error(
    tabla_frecuencias(data = c(1, 2, 3), column = "valores", silent = TRUE),
    "El argumento 'data' debe ser un dataframe."
  )
})

# Prueba 2: Validar que la función falla si 'column' no es una cadena válida
test_that("Falla si 'column' no es una columna válida", {
  expect_error(
    tabla_frecuencias(data = df_test, column = "no_existe", silent = TRUE),
    "El argumento 'column' debe ser el nombre de una columna válida en el dataframe."
  )
})

# Prueba 3: Validar que la función falla si la columna no es numérica
test_that("Falla si la columna no es numérica", {
  df_char <- data.frame(text = c("a", "b", "c"))
  expect_error(
    tabla_frecuencias(data = df_char, column = "text", silent = TRUE),
    "La columna especificada debe ser numérica."
  )
})

# Prueba 4: Validar que la función falla si no hay datos no nulos
test_that("Falla si no hay datos no nulos", {
  df_na <- data.frame(valores = c(NA, NA, NA))
  expect_error(
    tabla_frecuencias(data = df_na, column = "valores", silent = TRUE),
    "No hay datos no nulos en la columna especificada."
  )
})

# Prueba 5: Validar que la función falla si 'k' no es un entero positivo
test_that("Falla si 'k' no es un entero positivo", {
  expect_error(
    tabla_frecuencias(data = df_test, column = "valores", k = -1, silent = TRUE),
    "El argumento 'k' debe ser un número entero positivo."
  )
  expect_error(
    tabla_frecuencias(data = df_test, column = "valores", k = 2.5, silent = TRUE),
    "El argumento 'k' debe ser un número entero positivo."
  )
})

# Prueba 6: Validar que la función falla con un solo valor válido
test_that("Falla con un solo valor válido", {
  df_single <- data.frame(valores = c(1, NA, NA))
  expect_error(
    tabla_frecuencias(data = df_single, column = "valores", silent = TRUE),
    "No hay suficientes datos válidos para crear una tabla de frecuencias."
  )
})

# Prueba 7: Validar que la función falla con datos de rango cero
test_that("Falla con datos de rango cero", {
  df_same <- data.frame(valores = c(5, 5, 5))
  expect_error(
    tabla_frecuencias(data = df_same, column = "valores", silent = TRUE),
    "No se pueden crear intervalos: el rango de los datos es demasiado pequeño."
  )
})

# Prueba 8: Validar que la función falla si no hay datos válidos tras eliminar valores atípicos
test_that("Falla si no hay datos válidos tras eliminar valores atípicos", {
  df_outlier <- data.frame(valores = c(1000, 1000))
  expect_error(
    tabla_frecuencias(data = df_outlier, column = "valores", handle_outliers = TRUE, silent = TRUE),
    "No se pueden crear intervalos: el rango de los datos es demasiado pequeño."
  )
})

# Prueba 9: Validar manejo de valores atípicos
test_that("Maneja correctamente los valores atípicos", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", handle_outliers = TRUE, silent = TRUE)
  
  expect_true(!is.na(resultado@valores_atipicos))
  expect_equal(length(resultado@valores_atipicos), 1) # Debería detectar el valor 10 como atípico
})

# Prueba 10: Validar manejo de múltiples valores atípicos
test_that("Maneja correctamente múltiples valores atípicos", {
  df_multi_outliers <- data.frame(valores = c(-10, 1, 2, 2, 3, 3, 3, 4, 4, 5, 20))
  resultado <- tabla_frecuencias(data = df_multi_outliers, column = "valores", handle_outliers = TRUE, silent = TRUE)
  
  expect_equal(sort(resultado@valores_atipicos), c(-10, 20)) # Debería detectar -10 y 20 como atípicos
})

# Prueba 11: Validar cálculos de medidas descriptivas
test_that("Calcula correctamente las medidas descriptivas", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", silent = TRUE)
  
  expect_equal(round(resultado@media, 4), round(mean(df_test$valores, na.rm = TRUE), 4))
  expect_equal(round(resultado@cuartil1, 4), round(unname(quantile(df_test$valores, 0.25, na.rm = TRUE)), 4))
  expect_equal(round(resultado@mediana, 4), round(median(df_test$valores, na.rm = TRUE), 4))
  expect_equal(round(resultado@cuartil3, 4), round(unname(quantile(df_test$valores, 0.75, na.rm = TRUE)), 4))
  expect_equal(round(resultado@desviacion_estandar, 4), round(sd(df_test$valores, na.rm = TRUE), 4))
})

# Prueba 12: Validar cálculos de moda no agrupada
test_that("Calcula correctamente la moda no agrupada", {
  df_moda <- data.frame(valores = c(1, 1, 2, 2, 3)) # Dos modas: 1 y 2
  resultado <- tabla_frecuencias(data = df_moda, column = "valores", silent = TRUE)
  
  expect_equal(sort(resultado@moda_frecuencia), c(1, 2))
  
  df_no_moda <- data.frame(valores = c(1, 2, 3, 4)) # Sin moda definida
  resultado_no_moda <- tabla_frecuencias(data = df_no_moda, column = "valores", silent = TRUE)
  expect_true(is.na(resultado_no_moda@moda_frecuencia))
})

# Prueba 13: Validar moda agrupada
test_that("Calcula correctamente la moda agrupada", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", silent = TRUE)
  
  expect_true(is.numeric(resultado@moda_agrupada))
  expect_true(length(resultado@moda_agrupada) >= 1)
})

# Prueba 14: Validar otras medidas descriptivas
test_that("Calcula correctamente otras medidas descriptivas", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", silent = TRUE)
  
  expect_equal(round(resultado@varianza, 4), round(var(df_test$valores, na.rm = TRUE), 4))
  expect_equal(round(resultado@coef_variacion, 4), round((sd(df_test$valores, na.rm = TRUE) / mean(df_test$valores, na.rm = TRUE)) * 100, 4))
  expect_equal(resultado@minimo, min(df_test$valores, na.rm = TRUE))
  expect_equal(resultado@maximo, max(df_test$valores, na.rm = TRUE))
  expect_equal(resultado@rango, max(df_test$valores, na.rm = TRUE) - min(df_test$valores, na.rm = TRUE))
})

# Prueba 15: Validar que el gráfico se genera si plot = TRUE
test_that("Genera un gráfico si plot = TRUE", {
  skip_if_not_installed("ggplot2")
  
  # Ejecutar la función
  resultado <- tabla_frecuencias(data = df_test, column = "valores", plot = TRUE, silent = TRUE)
  
  # Verificar que resultado es un objeto S4 de clase TablaFrecuencias
  expect_s4_class(resultado, "TablaFrecuencias")
  
  # Verificar que el slot grafico no es NULL y es de clase ggplot
  expect_true(!is.null(resultado@grafico))
  expect_s3_class(resultado@grafico, "ggplot")
})

# Prueba 16: Validar advertencia cuando ggplot2 no está instalado
test_that("Lanza advertencia si ggplot2 no está instalado", {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    skip("ggplot2 está instalado, prueba omitida")
  }
  expect_warning(
    tabla_frecuencias(data = df_test, column = "valores", plot = TRUE, silent = TRUE),
    "El paquete 'ggplot2' no está instalado. Instálalo para generar gráficos."
  )
})

# Prueba 17: Validar comportamiento con decimales personalizados
test_that("Maneja correctamente el parámetro decimales", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", decimales = 2, silent = TRUE)
  
  expect_equal(resultado@decimales, 2)
  expect_true(all(sapply(resultado@tabla_frecuencias[, c("Frecuencia_Relativa", "Frecuencia_Relativa_Acumulada",
                                                         "Frecuencia_Porcentual", "Frecuencia_Porcentual_Acumulada")],
                         function(x) all(grepl("\\.[0-9]{2}$", as.character(x))))))
})

# Prueba 18: Validar exportación a CSV
test_that("Exporta correctamente a CSV", {
  archivo <- tempfile(fileext = ".csv")
  resultado <- tabla_frecuencias(data = df_test, column = "valores", export = archivo, silent = TRUE)
  
  expect_true(file.exists(archivo))
  tabla_leida <- read.csv(archivo)
  expect_true(all(c("Intervalo", "Frecuencia_Absoluta", "Frecuencia_Acumulada",
                    "Frecuencia_Relativa", "Frecuencia_Relativa_Acumulada",
                    "Frecuencia_Porcentual", "Frecuencia_Porcentual_Acumulada") %in%
                    names(tabla_leida)))
})

# Prueba 19: Validar exportación a Excel
test_that("Exporta correctamente a Excel", {
  skip_if_not_installed("writexl")
  archivo <- tempfile(fileext = ".xlsx")
  resultado <- tabla_frecuencias(data = df_test, column = "valores", export = archivo, silent = TRUE)
  
  expect_true(file.exists(archivo))
  tabla_leida <- readxl::read_excel(archivo)
  expect_true(all(c("Intervalo", "Frecuencia_Absoluta", "Frecuencia_Acumulada",
                    "Frecuencia_Relativa", "Frecuencia_Relativa_Acumulada",
                    "Frecuencia_Porcentual", "Frecuencia_Porcentual_Acumulada") %in%
                    names(tabla_leida)))
})

# Prueba 20: Validar advertencia para formato de archivo no soportado
test_that("Lanza advertencia para formato de archivo no soportado", {
  archivo <- tempfile(fileext = ".txt")
  expect_warning(
    tabla_frecuencias(data = df_test, column = "valores", export = archivo, silent = TRUE),
    "Formato de archivo no soportado. Usa '.csv' o '.xlsx'."
  )
})

# Prueba 21: Validar que la función crea correctamente el objeto S4
test_that("Crea correctamente el objeto TablaFrecuencias", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", silent = TRUE)
  
  expect_s4_class(resultado, "TablaFrecuencias")
  expect_true(is.data.frame(resultado@tabla_frecuencias))
  expect_true(all(c("Intervalo", "Frecuencia_Absoluta", "Frecuencia_Acumulada",
                    "Frecuencia_Relativa", "Frecuencia_Relativa_Acumulada",
                    "Frecuencia_Porcentual", "Frecuencia_Porcentual_Acumulada") %in%
                    names(resultado@tabla_frecuencias)))
  expect_equal(resultado@decimales, 2)
  expect_equal(resultado@column, "valores")
})

# Prueba 22: Validar validaciones del objeto S4
test_that("Valida correctamente el objeto S4", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", silent = TRUE)
  
  # Modificar tabla_frecuencias para que falle la validación
  invalid_object <- resultado
  invalid_object@tabla_frecuencias <- data.frame(x = 1)
  expect_error(
    validObject(invalid_object),
    "El slot 'tabla_frecuencias' debe contener todas las columnas requeridas."
  )
  
  # Modificar decimales para que falle la validación
  invalid_object <- resultado
  invalid_object@decimales <- -1
  expect_error(
    validObject(invalid_object),
    "El slot 'decimales' debe ser un entero no negativo."
  )
  
  # Modificar column para que falle la validación
  invalid_object <- resultado
  invalid_object@column <- c("valores", "otro")
  expect_error(
    validObject(invalid_object),
    "El slot 'column' debe ser una cadena de caracteres de longitud 1."
  )
})

# Prueba 23: Validar el método show
test_that("El método show imprime correctamente", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", silent = TRUE)
  
  expect_output(show(resultado), "Tabla de Frecuencias")
  expect_output(show(resultado), "Valores faltantes \\(NA\\): 1")
  expect_output(show(resultado), "Media:")
})

# Prueba 24: Validar el método plot
test_that("El método plot funciona correctamente", {
  resultado <- tabla_frecuencias(data = df_test, column = "valores", plot = TRUE, silent = TRUE)
  
  expect_silent(plot(resultado))
  
  resultado_no_plot <- tabla_frecuencias(data = df_test, column = "valores", plot = FALSE, silent = TRUE)
  expect_output(plot(resultado_no_plot), "No se generó un gráfico")
})

# Prueba 25: Validar salida en consola cuando silent = FALSE
test_that("Imprime mensajes en consola cuando silent = FALSE", {
  expect_output(
    tabla_frecuencias(data = df_test, column = "valores", handle_outliers = TRUE, silent = FALSE),
    "Valores atípicos detectados y excluidos: 1"
  )
  archivo <- tempfile(fileext = ".csv")
  expect_output(
    tabla_frecuencias(data = df_test, column = "valores", export = archivo, silent = FALSE),
    "Tabla exportada como CSV en:"
  )
})