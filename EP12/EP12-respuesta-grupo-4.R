# Ejercicio Práctico 12
# Grupo 4

# Integrantes:
# Aarón Ibáñez
# 
# 
# 

library(ggpubr)

################################## PREGUNTA 1 ##################################
# 1. En el trabajo de título de un estudiante del DIINF se reportan los siguientes
# tiempos de ejecución (en milisegundos) medidos para dos versiones de un
# algoritmo genético para resolver instancias del problema del vendedor viajero
# disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que
# el otro?

# Definición de Hipótesis
# Hipótesis Nula: En promedio, los tiempos de ejecución para ambas versiones del
# algortimo genético aplicado a la resolución del problema del vendedor viajero 
# son iguales 
# uA = uB

# Hipótesis Alternativa: En promedio, los tiempos de ejecución para ambas versiones del algortimo genético aplicado a
# la resolución del problema del vendedor viajero son diferentes.
# uA != uB

# Se definen los datos
tiempoA <- c(48667, 783108, 4428151, 842079, 8576989, 885722, 834565, 210041,
             48705, 251843)
tiempoB <- c(6684497, 5743260, 92932, 48408, 994830, 2196277, 35974, 4629726,
             180141, 1174562)
algoritmo <- factor(c(rep("A", length(tiempoA)), rep("B", length(tiempoB))))
tiempo <- c(tiempoA, tiempoB)
datos1 <- data.frame(algoritmo, tiempo)

# Histograma de los datos
g1 <- ggplot(datos1, aes(x = tiempo)) +
  geom_histogram(fill = "white", colour = "black", bins = 5) +
  facet_grid(algoritmo ~ .)
print(g1)

# Se ve que no existe una distribución normal. Transformamos los datos usando el
# logaritmo de los valores de tiempo en ms.
datos1$logTiempo = log(datos1$tiempo)

# Histograma los datos transformados.
g2 <- ggplot(datos1, aes(x = logTiempo)) +
  geom_histogram(fill = "white", colour = "black", bins = 5) +
  facet_grid(algoritmo ~ .)
print(g2)

# Se ve ahora un comportamiento de los datos más cercano a una distribución
# normal. Se aplica entonces una prueba de normalidad.
normalidad <- shapiro.test(datos1$logTiempo)
cat("\nPrueba de normalidad para datos transformados:\n")
print(normalidad)

# Se tiene que p > 0.05 (p = 0.1189), lo que nos indica que tenemos una
# distribución normal. Se puede entonces continuar con una prueba paramétrica,
# en este caso una prueba t de Student.
prueba1 <- t.test(logTiempo ~ algoritmo, data = datos1)
cat("\nPrueba t de Student para datos transformados:\n")
print(prueba1)

# Se obtuvo un p valor de 0.8365 el cual es mayor que el nivel de significación
# definido en 0.05, por lo que fallamos en rechazar la hipótesis nula. En
# conclusión, no hay sufieciente evidencia para considerar que existe una
# diferencia significativa entre las medias geométricas de los tiempos de
# ejecución requeridos por los algoritmos.

################################## PREGUNTA 2 ##################################
# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.



################################## PREGUNTA 3 ##################################
# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.



