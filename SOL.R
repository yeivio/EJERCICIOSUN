# Función para leer los números desde el archivo
leer_numeros <- function(archivo) {
  # Verificar si el archivo existe
  if (!file.exists(archivo)) {
    stop("El archivo no existe. Deteniendo la ejecución.")
  }
  
  # Leer los datos y convertirlos a un vector de enteros
  numeros <- as.integer(readLines(archivo))
  
  return(numeros)
}

# Función para calcular y escribir los resultados en el archivo
procesar_resultados <- function(numeros) {
  # Calcular la media, mediana y desviación estándar
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  
  # Verificar si hay alta variabilidad
  if (desviacion_estandar > 10) {
    mensaje_variabilidad <- "Alta variabilidad en los datos (desviación estándar > 10)."
  } else {
    mensaje_variabilidad <- "Variabilidad normal en los datos."
  }
  
  # Usar sapply() para calcular el cuadrado de cada número
  cuadrados <- sapply(numeros, function(x) x^2)
  
  # Crear el texto para escribir en el archivo de resultados
  resultados <- paste(
    "Estadísticos:\n",
    "Media: ", media, "\n",
    "Mediana: ", mediana, "\n",
    "Desviación Estándar: ", desviacion_estandar, "\n",
    mensaje_variabilidad, "\n\n",
    "Cuadrados de los números:\n", 
    paste(cuadrados, collapse = ", "), "\n"
  )
  
  # Escribir los resultados en el archivo de salida
  writeLines(resultados, "resultados.txt")
}

# Función principal que coordina todo el proceso
procesar_datos_numeros <- function() {
  # Leer los números desde el archivo
  numeros <- leer_numeros("numeros.txt")
  
  # Procesar los resultados y escribirlos en el archivo de salida
  procesar_resultados(numeros)
}

# Ejecutar la función principal
procesar_datos_numeros()
