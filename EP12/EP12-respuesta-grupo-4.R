# Ejercicio Práctico 12
# Grupo 4

# Integrantes:
# Aarón Ibáñez
# 
# 
# 

library(ggpubr)
library(WRS2)

################################## PREGUNTA 1 ##################################
# 1. En el trabajo de título de un estudiante del DIINF se reportan los siguientes
# tiempos de ejecución (en milisegundos) medidos para dos versiones de un
# algoritmo genético para resolver instancias del problema del vendedor viajero
# disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido que
# el otro?

# ---- Definición de Hipótesis ----
# Hipótesis Nula: En promedio, los tiempos de ejecución
# para ambas versiones del algortimo genético aplicado a
# la resolución del problema del vendedor viajero 
# son iguales 
# uA = uB

# Hipótesis Alternativa: En promedio, los tiempos de ejecución
# para ambas versiones del algortimo genético aplicado a
# la resolución del problema del vendedor viajero 
# son diferentes.
# uA != uB

# Cargar datos.
tiempoA <- c(48667, 783108, 4428151, 842079, 8576989, 885722, 834565, 210041,
             48705, 251843)

tiempoB <- c(6684497, 5743260, 92932, 48408, 994830, 2196277, 35974, 4629726,
             180141, 1174562)

algoritmo <- factor(c(rep("A", length(tiempoA)), rep("B", length(tiempoB))))
tiempo <- c(tiempoA, tiempoB)
datos1 <- data.frame(algoritmo, tiempo)


# Construimos el histograma de los datos.
g1 <- gghistogram(datos1, x = "tiempo", xlab = "algoritmo", color = "algoritmo",
                  fill = "algoritmo", bins = 5)

g1 <- g1 + facet_grid(~ algoritmo)
print(g1)


# Prueba de Normalidad 


################################## PREGUNTA 2 ##################################
# 2. Analice la primera pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.



################################## PREGUNTA 3 ##################################
# 3. Analice la segunda pregunta abordada en el ejercicio práctico 11, con los
# mismos datos, utilizando un método robusto adecuado.



