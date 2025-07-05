# Definir la clase S4
setClass(
  "TablaFrecuencias",
  slots = list(
    tabla_frecuencias = "data.frame",
    media = "numeric",
    cuartil1 = "numeric",
    mediana = "numeric",
    cuartil3 = "numeric",
    moda_frecuencia = "numeric",
    moda_agrupada = "numeric",
    minimo = "numeric",
    maximo = "numeric",
    rango = "numeric",
    desviacion_estandar = "numeric",
    varianza = "numeric",
    coef_variacion = "numeric",
    valores_faltantes = "numeric",
    valores_atipicos = "numeric",
    grafico = "ANY",
    decimales = "numeric",
    column = "character"
  ),
  validity = function(object) {
    if (!is.data.frame(object@tabla_frecuencias)) {
      return("El slot 'tabla_frecuencias' debe ser un data.frame.")
    }
    if (!all(c("Intervalo", "Frecuencia_Absoluta", "Frecuencia_Acumulada",
               "Frecuencia_Relativa", "Frecuencia_Relativa_Acumulada",
               "Frecuencia_Porcentual", "Frecuencia_Porcentual_Acumulada") %in%
             names(object@tabla_frecuencias))) {
      return("El slot 'tabla_frecuencias' debe contener todas las columnas requeridas.")
    }
    if (object@decimales < 0 || object@decimales != floor(object@decimales)) {
      return("El slot 'decimales' debe ser un entero no negativo.")
    }
    if (length(object@column) != 1 || !is.character(object@column)) {
      return("El slot 'column' debe ser una cadena de caracteres de longitud 1.")
    }
    return(TRUE)
  }
)

# Función para generar una tabla de frecuencias y medidas descriptivas (Clase S4)
tabla_frecuencias <- function(data, column, k = NULL, decimales = 4, handle_outliers = FALSE, plot = FALSE, export = NULL, silent = FALSE) {
  
  # Validar que data sea un dataframe
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un dataframe.")
  }
  
  # Validar que column sea una cadena y exista en el dataframe
  if (!is.character(column) || !column %in% names(data)) {
    stop("El argumento 'column' debe ser el nombre de una columna válida en el dataframe.")
  }
  
  # Extraer la columna especificada
  t <- data[[column]]
  
  # Contar valores NA
  n_na <- sum(is.na(t))
  n <- sum(!is.na(t))
  if (n == 0) {
    stop("No hay datos no nulos en la columna especificada.")
  }
  
  # Validar que la columna sea numérica
  if (!is.numeric(t)) {
    stop("La columna especificada debe ser numérica.")
  }
  
  # Manejo de valores atípicos (si handle_outliers = TRUE)
  t_clean <- t
  outliers <- NULL
  if (handle_outliers) {
    t_no_na <- t[!is.na(t)]
    if (length(t_no_na) == 0) {
      stop("No hay datos no nulos en la columna especificada para manejar valores atípicos.")
    }
    q1 <- unname(as.numeric(quantile(t_no_na, 0.25, na.rm = TRUE)))
    q3 <- unname(as.numeric(quantile(t_no_na, 0.75, na.rm = TRUE)))
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    outliers <- t_no_na[t_no_na < lower_bound | t_no_na > upper_bound]
    t_clean <- t[t <= upper_bound & t >= lower_bound & !is.na(t)]
    if (length(t_clean) == 0) {
      stop("Después de eliminar valores atípicos, no quedan datos válidos.")
    }
    n <- sum(!is.na(t_clean))
    if (!silent) {
      cat("Valores atípicos detectados y excluidos:", length(outliers), "\n")
    }
  }
  
  # Filtrar valores no finitos
  t_clean <- t_clean[!is.na(t_clean) & is.finite(t_clean)]
  if (length(t_clean) == 0) {
    stop("No hay datos válidos después de filtrar valores no finitos.")
  }
  if (length(t_clean) < 2) {
    stop("No hay suficientes datos válidos para crear una tabla de frecuencias.")
  }
  
  # Calcular el número intervalos usando la regla de Sturges si k es NULL
  if (is.null(k)) {
    k <- ceiling(1 + 3.322 * log10(n))
  } else {
    if (!is.numeric(k) || k <= 0 || k != floor(k)) {
      stop("El argumento 'k' debe ser un número entero positivo.")
    }
  }
  
  # Calcular el rango de los datos
  rango <- max(t_clean, na.rm = TRUE) - min(t_clean, na.rm = TRUE)
  
  # Calcular el ancho de los intervalos
  amplitud <- rango / k
  
  # Crear los puntos de corte (breaks) para los intervalos
  breaks <- seq(min(t_clean, na.rm = TRUE), max(t_clean, na.rm = TRUE), by = amplitud)
  
  # Ajustar breaks para incluir el máximo si es necesario
  if (max(t_clean, na.rm = TRUE) > max(breaks)) {
    breaks <- c(breaks, max(breaks) + amplitud)
  }
  
  # Verificar que haya suficientes puntos de corte
  if (length(breaks) <= 1) {
    stop("No se pueden crear intervalos: el rango de los datos es demasiado pequeño.")
  }
  
  # Crear los intervalos
  intervalos <- cut(t_clean, breaks = breaks, right = TRUE, include.lowest = TRUE)
  
  # Calcular frecuencias absolutas
  absoluta <- table(intervalos)
  
  # Calcular frecuencias acumuladas
  acumulada <- cumsum(absoluta)
  
  # Calcular frecuencias relativas
  relativa <- prop.table(absoluta)
  
  # Calcular frecuencias relativas acumuladas
  r_acumulada <- cumsum(relativa)
  
  # Calcular frecuencias porcentuales
  porcentual <- relativa * 100
  
  # Calcular frecuencias porcentuales acumuladas
  p_acumulada <- cumsum(porcentual)
  
  # Crear la tabla de frecuencias con redondeo configurable
  tabla_frecuencias <- data.frame(
    Intervalo = names(absoluta),
    Frecuencia_Absoluta = as.vector(absoluta),
    Frecuencia_Acumulada = as.vector(acumulada),
    Frecuencia_Relativa = sprintf(paste0("%.", decimales, "f"), as.vector(relativa)),
    Frecuencia_Relativa_Acumulada = sprintf(paste0("%.", decimales, "f"), as.vector(r_acumulada)),
    Frecuencia_Porcentual = sprintf(paste0("%.", decimales, "f"), as.vector(porcentual)),
    Frecuencia_Porcentual_Acumulada = sprintf(paste0("%.", decimales, "f"), as.vector(p_acumulada))
  )
  
  # Calcular medidas descriptivas
  m <- mean(t_clean, na.rm = TRUE)
  q1 <- unname(as.numeric(quantile(t_clean, 0.25, na.rm = TRUE)))
  me <- median(t_clean, na.rm = TRUE)
  q3 <- unname(as.numeric(quantile(t_clean, 0.75, na.rm = TRUE)))
  
  # Moda para datos no agrupados
  obtener_moda <- function(x) {
    ux <- unique(x[!is.na(x)])
    tab <- tabulate(match(x, ux))
    max_freq <- max(tab)
    if (max_freq == 0) return(NA_real_)
    modas <- ux[tab == max_freq]
    if (length(modas) == length(ux)) return(NA_real_)
    return(modas)
  }
  moda <- obtener_moda(t_clean)
  
  # Moda agrupada
  max_freq <- max(tabla_frecuencias$Frecuencia_Absoluta)
  idx_modales <- which(tabla_frecuencias$Frecuencia_Absoluta == max_freq)
  modas_agrupadas <- numeric(length(idx_modales))
  for (i in seq_along(idx_modales)) {
    idx_modal <- idx_modales[i]
    inter_modal <- tabla_frecuencias$Intervalo[idx_modal]
    inter_limpio <- gsub("\\[|\\]|\\(|\\)", "", inter_modal)
    valores <- as.numeric(unlist(strsplit(inter_limpio, ",")))
    limite_inf_modal <- valores[1]
    fmodal <- tabla_frecuencias$Frecuencia_Absoluta[idx_modal]
    fanterior <- ifelse(idx_modal > 1, tabla_frecuencias$Frecuencia_Absoluta[idx_modal - 1], 0)
    fposterior <- ifelse(idx_modal < nrow(tabla_frecuencias),
                         tabla_frecuencias$Frecuencia_Absoluta[idx_modal + 1], 0)
    denominator <- (fmodal - fanterior) + (fmodal - fposterior)
    if (denominator == 0 || is.na(denominator)) {
      modas_agrupadas[i] <- NA
    } else {
      mo <- limite_inf_modal + ((fmodal - fanterior) / denominator) * amplitud
      modas_agrupadas[i] <- round(mo, decimales)
    }
  }
  
  # Rango de valores
  rango_valores <- range(t_clean, na.rm = TRUE)
  rango_dif <- diff(rango_valores)
  
  # Desviación estándar
  desvest <- sd(t_clean, na.rm = TRUE)
  
  # Varianza
  varianza <- var(t_clean, na.rm = TRUE)
  
  # Coeficiente de variación
  cv <- (desvest / m) * 100
  
  # Generar gráfico con ggplot2 si plot = TRUE
  grafico <- NULL
  if (plot) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      warning("El paquete 'ggplot2' no está instalado. Instálalo para generar gráficos.")
    } else {
      midpoints <- (breaks[-length(breaks)] + breaks[-1]) / 2
      grafico <- ggplot2::ggplot(data.frame(x = t_clean), ggplot2::aes(x = x)) +
        ggplot2::geom_histogram(breaks = breaks, fill = "lightblue", color = "black") +
        ggplot2::scale_x_continuous(
          breaks = midpoints,
          labels = tabla_frecuencias$Intervalo
        ) +
        ggplot2::labs(title = paste("Histograma de", column), x = column, y = "Frecuencia") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  }
  
  # Exportar tabla si se especifica un archivo
  if (!is.null(export)) {
    if (!is.character(export)) {
      warning("El argumento 'export' debe ser una cadena con la ruta del archivo.")
    } else {
      if (grepl("\\.csv$", export)) {
        write.csv(tabla_frecuencias, file = export, row.names = FALSE)
        if (!silent) {
          cat("Tabla exportada como CSV en:", export, "\n")
        }
      } else if (grepl("\\.xlsx$", export)) {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          warning("El paquete 'writexl' no está instalado. Instálalo para exportar a Excel.")
        } else {
          writexl::write_xlsx(tabla_frecuencias, path = export)
          if (!silent) {
            cat("Tabla exportada como Excel en:", export, "\n")
          }
        }
      } else {
        warning("Formato de archivo no soportado. Usa '.csv' o '.xlsx'.")
      }
    }
  }
  
  # Crear objeto de clase S4
  resultado <- new("TablaFrecuencias",
                   tabla_frecuencias = tabla_frecuencias,
                   media = m,
                   cuartil1 = q1,
                   mediana = me,
                   cuartil3 = q3,
                   moda_frecuencia = if (length(moda) == 0) NA_real_ else moda,
                   moda_agrupada = if (length(modas_agrupadas) == 0) NA_real_ else modas_agrupadas,
                   minimo = min(t_clean, na.rm = TRUE),
                   maximo = max(t_clean, na.rm = TRUE),
                   rango = rango_dif,
                   desviacion_estandar = desvest,
                   varianza = varianza,
                   coef_variacion = cv,
                   valores_faltantes = n_na,
                   valores_atipicos = if (length(outliers) == 0) NA_real_ else outliers,
                   grafico = grafico,
                   decimales = decimales,
                   column = column
  )
  
  # Imprimir resultados si silent = FALSE
  if (!silent) {
    # Imprimir el objeto completo usando el método show
    show(resultado)
  }
  
  # Visualización en entorno interactivo
  if (interactive() && !silent) {
    View(tabla_frecuencias)
  }
  
  return(invisible(resultado))
}

# Método show para la clase TablaFrecuencias
setMethod("show", "TablaFrecuencias", function(object) {
  cat("\nTabla de Frecuencias, Medidas de Tendencia Central y Dispersión para la variable", object@column, "\n")
  if (object@valores_faltantes > 0) {
    cat("Valores faltantes (NA):", object@valores_faltantes, "\n")
  }
  cat("\n")
  print(object@tabla_frecuencias, row.names = FALSE)
  cat("\nMedidas de tendencia central:\n")
  cat("\n")
  cat("Media:", round(object@media, object@decimales), "\n")
  cat("Cuartil 1:", round(object@cuartil1, object@decimales), "\n")
  cat("Mediana:", round(object@mediana, object@decimales), "\n")
  cat("Cuartil 3:", round(object@cuartil3, object@decimales), "\n")
  cat("Moda (frecuencia):", 
      if (length(object@moda_frecuencia) > 1) paste(round(object@moda_frecuencia, object@decimales), collapse = ", ") 
      else if (is.na(object@moda_frecuencia)) "No definida" 
      else round(object@moda_frecuencia, object@decimales), "\n")
  cat("Moda (datos agrupados):", 
      if (length(object@moda_agrupada) > 1) paste(round(object@moda_agrupada, object@decimales), collapse = ", ") 
      else if (all(is.na(object@moda_agrupada))) "No definida" 
      else round(object@moda_agrupada, object@decimales), "\n")
  cat("\nMedidas de variabilidad:\n")
  cat("\n")
  cat("Mínimo:", round(object@minimo, object@decimales), "\n")
  cat("Máximo:", round(object@maximo, object@decimales), "\n")
  cat("Rango:", round(object@rango, object@decimales), "\n")
  cat("Desviación Estándar:", round(object@desviacion_estandar, object@decimales), "\n")
  cat("Varianza:", round(object@varianza, object@decimales), "\n")
  cat("Coeficiente de Variación (%):", round(object@coef_variacion, object@decimales), "\n")
})

# Método plot para la clase TablaFrecuencias
setMethod("plot", "TablaFrecuencias", function(x, y, ...) {
  if (is.null(x@grafico)) {
    cat("No se generó un gráfico. Usa plot = TRUE al crear el objeto.\n")
  } else {
    print(x@grafico)
  }
})