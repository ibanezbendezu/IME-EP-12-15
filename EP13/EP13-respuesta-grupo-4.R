library(tidyverse)

################################## ENUNCIADO ###################################

# Se pide construir un modelo de regresión lineal simple y otro de regresión
# lineal múltiple para predecir la variable Peso, de acuerdo con las siguientes
# instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos
#    del RUN (sin considerar el dígito verificador) del integrante de menor edad
#    del equipo.
# 2. Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 50
#    hombres (si la semilla es impar).
# 3. Seleccionar de forma aleatoria ocho posibles variables predictoras.
# 4. Seleccionar, entre las variables que no fueron escogidas en el punto
#    anterior, una que el equipo considere que podría ser útil para predecir la
#    variable Peso, justificando bien esta selección.
# 5. Usando el entorno R, construir un modelo de regresión lineal simple con el
#    predictor seleccionado en el paso anterior.
# 6. Usando herramientas para la exploración de modelos del entorno R, escoger
#    entre dos y cinco predictores de entre las variables seleccionadas en los
#    puntos 3 y 4 (9 en total) para construir un modelo de regresión lineal
#    múltiple.
# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema
#    con las condiciones que deben cumplir.
# 8. Evaluar el poder predictivo del modelo en datos no utilizados para
#    construirlo (o utilizando validación cruzada).


################################## DESARROLLO ##################################
data <- read.csv2(file.choose(), check.names = F, encoding = "latin1")

# (1) Definimos una semilla.
set.seed(7754)


# (2) Seleccionamos una muestra de 50 mujeres.
datos <- data %>% filter(Gender == 0)
datos[["Gender"]] <- NULL
datos <- sample_n(datos, 50, replace = FALSE)

# Guardamos la variable respuesta.
peso <- datos[["Weight"]]
datos[["Weight"]] <- NULL


# (3) Seleccionamos de forma aleatoria ocho posibles variables predictoras.
nombresPredictores <- colnames(datos)
predictores <- sample(nombresPredictores, 8, replace = FALSE)
cat("\nVariables predictoras escogidas al azar:\n")
print(predictores)

# Aprovechamos de guardar los datos para la regresion lineal múltiple.
datosRLM <- datos %>% select(predictores)
datosRLM <- cbind(peso, datosRLM)


# (4) De entre las variables que no fueron escogidas en el punto anterior,
# seleccionamos una que el equipo considere que podría ser útil para predecir
# la variable Peso.
datos <- datos %>% select(!predictores)
# Matriz de correlación de la variable de respuesta y los datos 
correlacion <- cor(datos, y = peso)
cat("\nMatriz de correlación:\n")
print(correlacion)

util <- which(correlacion == max(abs(correlacion)))
predictor <- rownames(correlacion)[util]

# Guardamos los datos para la regresion lineal simple.
datosRLS <- datos %>% select(util)
datosRLS <- cbind(peso, datosRLS)
cat("\nSe escoge como predictor util a la variable:", predictor)


# (5) Construimos un modelo de regresión lineal simple con el predictor Waist.Girth.



