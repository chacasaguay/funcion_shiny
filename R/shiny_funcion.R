# Definimos la función para crear la tabla de frecuencia y calcular estadísticas
tabla_frecuencia<- function(data, variable) {
  # Tabla de frecuencia
  freq_table <- data %>%
    count(!!sym(variable)) %>%
    mutate(Frequency = n / sum(n))

  # Calcular media (solo para variables numéricas)
  if (is.numeric(data[[variable]])) {
    mean_val <- mean(data[[variable]], na.rm = TRUE)
  } else {
    mean_val <- NA
  }

  # Calcular moda
  mode_val <- names(sort(table(data[[variable]]), decreasing = TRUE))[1]

  # Combinar resultados
  list(
    freq_table = freq_table,
    mean = mean_val,
    mode = mode_val
  )
}
