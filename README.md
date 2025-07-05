# Funcion `tabla_frecuencia` 
Esta funcion calcula una tabla de frecuencias y estadisticas descriptivas (media, mediana, cuartiles, moda, etc.) para una columna numerica de un data frame. Permite manejar valores atipicos, generar graficos y exportar resultados a CSV o Excel.

# bjetivo General 
Esta función construye un objeto S4 que encapsula una tabla de frecuencias para variables numéricas, junto con medidas de tendencia central, dispersión, detección de valores atípicos y, opcionalmente, un gráfico.

# Estructura de la Clase S4 TablaFrecuencias 
# Contiene los siguientes slots (atributos):

* **`tabla_frecuencias`:** tabla con frecuencias absoluta, relativa, acumuladas y porcentuales.

* **medidas de tendencia central.**
  * media, mediana, cuartil1, cuartil3:
  * moda_frecuencia: moda no agrupada.
  * moda_agrupada: moda calculada a partir de intervalos.
    
* **medidas de dispersión.**
  * minimo, maximo, rango: resumen del rango de los datos.
  * desviacion_estandar, varianza, coef_variacion: 

**valores_faltantes:** cantidad de NA’s en la columna original.

**valores_atipicos:** vector de valores considerados outliers (si se solicita).

**grafico:** histograma generado con ggplot2 (opcional).

**decimales:** número de decimales para redondeo.

**column:** nombre de la columna analizada.

# Características Principales de la Función `tabla_frecuencias`()

**1. Validación de Entradas**
Verifica que data sea un data.frame.

Verifica que column sea una cadena válida y que exista en el dataframe.

Comprueba que los datos de la columna sean numéricos y no estén vacíos.

**2. Manejo de Datos Faltantes y Atípicos**
Cuenta NAs.

Si handle_outliers = TRUE, aplica la regla del IQR (1.5 * rango intercuartílico) para detectar y excluir outliers.

**3. Cálculo de Intervalos**
Usa la regla de Sturges si no se especifica k (número de intervalos).

Calcula el rango y amplitud para generar los cortes (breaks).

**Crea los intervalos con cut() y genera:**

* Frecuencia absoluta

* Frecuencia acumulada

* Frecuencia relativa

* Frecuencia relativa acumulada

* Frecuencia porcentual

* Frecuencia porcentual acumulada

**4. Medidas Descriptivas**
   
**Tendencia central:** media, mediana, cuartiles.

**Moda:** tanto para datos originales como para intervalos (moda agrupada).

**Dispersión:** desviación estándar, varianza, coeficiente de variación, mínimo, máximo y rango.

**5. Gráfico Opcional**
Si plot = TRUE y se encuentra ggplot2, se genera un histograma personalizado usando los intervalos.

**6. Exportación**
Permite exportar la tabla de frecuencias a .csv o .xlsx si se especifica el argumento export.

**7. Creación del Objeto S4**
Crea un objeto de clase TablaFrecuencias con todos los datos calculados.

Muestra los resultados si silent = FALSE.

**8. Métodos Asociados**
show(): muestra la tabla y las medidas de forma amigable.

plot(): imprime el gráfico almacenado (si existe).

# Ventajas de Usar esta Implementación

* **Encapsulación:** agrupa los datos y estadísticas en un solo objeto estructurado.

* **Validación integrada:** asegura consistencia de los slots mediante el método validity.

-Visualización: opción de histograma con ggplot2.

-Exportación: permite guardar resultados directamente desde la función.

-Flexibilidad: configurable en número de intervalos, decimales, gráfico, exportación y detección de atípicos.

