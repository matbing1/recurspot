# Definición de la función potencia
potencia <- function(base, exponente) {
  if (exponente == 0) {
    return(1)
  } else if (exponente < 0) {
    return(1 / potencia(base, -exponente))
  } else {
    resultado_parcial <- potencia(base, exponente %/% 2)
    resultado <- resultado_parcial * resultado_parcial
    if (exponente %% 2 != 0) {
      resultado <- resultado * base
    }
    return(resultado)
  }
}

# Definición de la función para calcular potencia compuesta
calcular_potencia_compuesta <- function(base, exponente1, exponente2) {
  resultado_intermedio <- potencia(base, exponente1)
  return(potencia(resultado_intermedio, exponente2))
}

# Función para interactuar con el usuario
interactuar_con_usuario <- function() {
  cat("Ingrese la base:\n")
  base <- as.numeric(readline())

  cat("Ingrese el primer exponente:\n")
  exponente1 <- as.numeric(readline())

  cat("Ingrese el segundo exponente:\n")
  exponente2 <- as.numeric(readline())

  # Validar entradas
  if (is.na(base) || is.na(exponente1) || is.na(exponente2)) {
    cat("Error: Todos los valores deben ser números válidos.\n")
    return()
  }

  # Calcular el resultado
  resultado <- calcular_potencia_compuesta(base, exponente1, exponente2)

  # Mostrar el resultado
  cat("El resultado de", base, "^", exponente1, "^", exponente2, "es:", resultado, "\n")
}

# Llamar a la función para interactuar con el usuario
interactuar_con_usuario()

