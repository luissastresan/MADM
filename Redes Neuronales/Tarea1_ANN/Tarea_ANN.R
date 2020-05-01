# Redes Neuronales Artificiales (ANN)

# Cargo librerias necesrarias
library(caTools)
library(h2o)
library(tidyverse)
library(ROCR)
# Importo el dataset
data = read.csv('sonar.all-data.csv')

# Conozcamos un poco el Dataset
head(data)
colnames(data)
str(data)

table(data$R)

data %>%
  group_by(R) %>%
  summarise(
    count = n()
  ) %>%
  ggplot(aes(R, count, fill = R)) +
  geom_bar(stat = "identity")

# Codifico la variable dependiente (R o M) en (0, 1)
data$R = ifelse(data$R == "R", 0, 1)
class(data$R) # Ya lo tenemos en tipo numerico

# Divido en conjunto de entrenamiento y test (75% y 25% respectivamente)
set.seed(333)
split = sample.split(data$R, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
testing_set = subset(data, split == FALSE)

# Escalo solo las variables independientes
training_set[, -61] = scale(training_set[, -61])
testing_set[, -61] = scale(testing_set[, -61])

# Creo la red Neuronal
h2o.init(nthreads = -1)
classifier = h2o.deeplearning(y = "R", # variable dependiente, la que quiero clasificar
                              training_frame = as.h2o(training_set),
                              activation = "Rectifier", # función de activacion
                              hidden = c(30, 30, 30), # 3 capas ocultas de 30 neuronas cada una
                              epochs = 50, # 200 iteraciones para ajustar los pesos
                              train_samples_per_iteration = -2,
                              adaptive_rate = TRUE) # Significa que el aprendizaje se adapta automaticamente. Por defecto ADADELTA

classifier1 = h2o.deeplearning(y = "R",
                               training_frame = as.h2o(training_set),
                               activation = "Tanh",
                               hidden = c(500, 500),
                               epochs = 100,
                               train_samples_per_iteration = -2,
                               adaptive_rate = TRUE) 

classifier2 = h2o.deeplearning(y = "R",
                               training_frame = as.h2o(training_set),
                               activation = "Rectifier",
                               hidden = c(500, 500, 500),
                               epochs = 500,
                               train_samples_per_iteration = -2,
                               adaptive_rate = FALSE,
                               rate = 0.00001, # Lo rápido que aprende por iteración
                               nesterov_accelerated_gradient = TRUE) 

# Prediccion de los resultados con el conjunto de testing
prob_pred = h2o.predict(classifier, 
                        newdata = as.h2o(testing_set[, -61]))
y_pred = (prob_pred > 0.8)
y_pred = as.vector(y_pred)

prob_pred1 = h2o.predict(classifier1, 
                        newdata = as.h2o(testing_set[, -61]))
y_pred1 = (prob_pred1 > 0.8)
y_pred1 = as.vector(y_pred1)

prob_pred2 = h2o.predict(classifier2, 
                         newdata = as.h2o(testing_set[, -61]))
y_pred2 = (prob_pred2 > 0.8)
y_pred2 = as.vector(y_pred2)

# Creo una matriz de confusion para comparar los resultados
cm = table(testing_set[, 61], y_pred)
cm1 = table(testing_set[, 61], y_pred1)
cm2 = table(testing_set[, 61], y_pred2)

# Visualización de los Falsos Positivos y los Verdaderos Positivos
pred <- prediction(as.numeric(y_pred), as.numeric(testing_set[, 61]))
perf <- performance(pred, "tpr", "fpr")
plot(perf)

pred1 <- prediction(as.numeric(y_pred1), as.numeric(testing_set[, 61]))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)

pred2 <- prediction(as.numeric(y_pred2), as.numeric(testing_set[, 61]))
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2)

# Cerrar la sesion h20
h2o.shutdown()






