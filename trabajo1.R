# Instalación de paquetes necesarios
install.packages("MASS")
install.packages("psych")
install.packages("lavaan")

# Cargar las librerías
library(MASS)
library(psych)
library(lavaan)

# Generar datos simulados con correlaciones específicas
set.seed(123)
n <- 200  # número de observaciones

# Matriz de correlaciones deseada
cor_matrix <- matrix(c(
  1.0, 0.6, 0.6, 0.3,
  0.6, 1.0, 0.6, 0.3,
  0.6, 0.6, 1.0, 0.3,
  0.3, 0.3, 0.3, 1.0
), nrow=4, ncol=4)

# Generar datos simulados
data <- mvrnorm(n, mu = c(50, 50, 50, 50), Sigma = cor_matrix, empirical = TRUE)
colnames(data) <- c("N_Ansiedad1", "NAnsiedad2", "N_Ansiedad3", "Depresion")

# Convertir a data frame
data <- as.data.frame(data)

# Agregar una variable de validación externa para ansiedad
data$Ansiedad_Validado <- rnorm(n, mean = 50, sd = 10) + 0.7 * rowMeans(data[, 1:3])  # combinación lineal para mayor correlación

# Guardar el conjunto de datos como CSV
write.csv(data, "ansiedad.csv", row.names = FALSE)

# Leer los datos (opcional, si no se continúan desde el script anterior)
data <- read.csv("ansiedad.csv")

# Inspeccionar los datos
head(data)

# Análisis de validez convergente
# Calcular la correlación entre el nuevo cuestionario y el cuestionario validado
cor_convergente <- cor(data[, 1:3], data$Ansiedad_Validado, method = "pearson")
print("Correlaciones convergentes:")
print(cor_convergente)

# Análisis de validez discriminante
# Calcular la correlación entre el nuevo cuestionario y la depresión
cor_discriminante <- cor(data[, 1:3], data$Depresion, method = "pearson")
print("Correlaciones discriminantes:")
print(cor_discriminante)

# Análisis factorial confirmatorio
model <- '
  Ansiedad =~ N_Ansiedad1 + N_Ansiedad2 + N_Ansiedad3
  Depresion =~ Depresion
'

fit <- cfa(model, data = data)
summary(fit, fit.measures = TRUE)

