
library(tidyverse)
library(caret)

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

# Condiciones:
# 1. Los datos deben presentar una relación lineal.
# 2. La distribución de los residuos debe ser cercana a la normal.
# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados debe
#    ser aproximadamente constante.
# 4. Las observaciones deben ser independientes entre sí.


# Ajustamos modelo de RLS usando validación cruzada de 5 pliegues.

RLS <- train (peso ~ Waist.Girth, data = datosRLS, method = "lm",
              trControl = trainControl(method = "cv", number = 5))

cat("\n\n\n\nModelo de regresión lineal simple\n")
print(summary(RLS))

# Evaluación modelo.
# Obtener residuos y estadísticas de influencia de los casos.
evalRLS <- data.frame(predicted.probabilities = fitted(RLS[["finalModel"]]))
evalRLS[["standardized.residuals"]] <- rstandard(RLS[["finalModel"]])
evalRLS[["studentized.residuals"]] <-rstudent(RLS[["finalModel"]])
evalRLS[["cooks.distance"]] <- cooks.distance(RLS[["finalModel"]])
evalRLS[["dfbeta"]] <- dfbeta(RLS[["finalModel"]])
evalRLS[["dffit"]] <- dffits(RLS[["finalModel"]])
evalRLS[["leverage"]] <- hatvalues(RLS[["finalModel"]])
evalRLS[["covariance.ratios"]] <- covratio(RLS[["finalModel"]])

cat("\nInfluencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(evalRLS[["standardized.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(evalRLS[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- ncol(datosRLS) / nrow(datosRLS)
sospechosos3 <- which(evalRLS[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(evalRLS[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio

sospechosos5 <- which(evalRLS[["covariance.ratios"]] < CVRi.lower |
                        evalRLS[["covariance.ratios"]] > CVRi.upper)

cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4, sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(evalRLS[sospechosos, c("cooks.distance", "leverage", "covariance.ratios")], 4))

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

