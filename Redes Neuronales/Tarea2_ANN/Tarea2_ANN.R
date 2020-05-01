# Redes Neuronales Artificiales (ANN)

# Cargo librerias necesrarias
library(neuralnet)
library(caTools)
library(rpart)
library(e1071)
library(plyr)

# Importo el dataset
data = read.csv("haberman.csv")

# Identifico el nombre de cada variable
colnames(data)

# Renombro las variables para que me sea más facil trabajar
names(data) = c("Age", "Year", "Nodes", "Status")

# Miro si hay NAs en el dataset
apply(data,2,function(x) sum(is.na(x))) # No hay na´s

# Creo la funcion para normalizar mis variables
normalize.minmaxbinary = function(v){
  return((v-min(v))/(max(v)-min(v)))
}
normalize.minmaxbipolar = function(v){
  return((v-min(v))/(max(v)-min(v))*(1-(-1))+(-1))
}

# Normalizo el dataset
data_n = as.data.frame(apply(data, 2, normalize.minmaxbinary)) 

# Divido en conjunto de entrenamiento y test (75% y 25% respectivamente), uno para los datos sin escalar y el
# otro con los datos escalados

# Sin escalar
set.seed(333)
split = sample.split(data$Status, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
testing_set = subset(data, split == FALSE)

# Escalados
set.seed(3333)
split = sample.split(data_n$Status, SplitRatio = 0.75)
training_set_n = subset(data_n, split == TRUE)
testing_set_n = subset(data_n, split == FALSE)

# ANN

# Creamos el clasificador de la red neuronal con el conjunto de trainning
regressor_ann = neuralnet(Status ~ Age + Year + Nodes,
                        training_set_n, hidden = c(3,3),
                        linear.output = T, threshold = 0.1)
plot(regressor_ann)

# Vamos a predecir el conjunto de testing con nuestro regressor que ya esta entrenado
y_pred_an = predict(regressor_ann, testing_set_n[, -4])

# Lo desnormalizamos para poder intrepetar el resultado
y_pred_ann = y_pred_an*
  (max(data$Status)-min(data$Status))+min(data$Status)

#Vamos a evaluar el modelo, para ver si nos ha dado una predicción acertada
actual = testing_set$Status
cbind(actual,y_pred_ann)

# Calculamos el MSE
n = nrow(testing_set)
MSE.ann = sum((actual - y_pred_ann)^2)/n
MSE.ann

# Linear Regression

# Creamos el regresor de la regresión con el conjunto de trainning sin escalar
regressor_lm = lm(formula = Status ~ Nodes, 
                  data = training_set_n)

summary(regressor_lm)

# Vamos a predecir el conjunto de testing con nuestro regressor que ya esta entrenado
y_pred_lm = predict(regressor_lm, testing_set_n)

# Calculamos el MSE
MSE_lm <- sum((actual - y_pred_lm)^2)/nrow(training_set)
MSE_lm

# Multiple Regression

# Creamos el regresor de la regresión con el conjunto de trainning sin escalar
regressor_mm = lm(formula = Status ~ ., 
                  data = training_set_n)

summary(regressor_mm)

# Vamos a predecir el conjunto de testing con nuestro regressor que ya esta entrenado
y_pred_mm = predict(regressor_lm, testing_set_n)

# Calculamos el MSE
MSE_mm <- sum((actual - y_pred_mm)^2)/nrow(training_set)
MSE_mm

# k-fold CROSS VALIDATION

error.cv <- NULL
k <- 10
## to control the process we create a progress bar to know the status of CV
progress.bar <- create_progress_bar('text')
progress.bar$init(k)
## the loop to implement cross validation 
folds <- sample(rep(1:10,length = nrow(data_n)))
## the loop to implement cross validation
for(i in 1:k){
  ## select rows with index=1 from Boston_n to create test set
  ## select remaining rows i to create training set
  trainset.cv <- data_n[folds!=i,]
  testset.cv <- data_n[folds==i,]
  ## the neural net
  ANN.cv <- neuralnet(Status ~ Age + Year + Nodes,
                      training_set_n, hidden = c(3,3),
                      linear.output = T, threshold = 0.1)
  ## propagating the input
  pr.ANN.cv <- predict(ANN.cv, testset.cv[,1:3])
  ## denormalize data for prediction
  pr.ANN.cvf<-pr.ANN.cv*(max(data$Status)-min(data$Status))+ min(data$Status)
  ## actual data
  test.cvf<-testset.cv$Status*(max(data$Status)-min(data$Status))+min(data$Status)
  n<-length(test.cvf)
  error.cv[i] <- sum((test.cvf - pr.ANN.cvf)^2)/n
  progress.bar$step()
}

cbind(test.cvf,pr.ANN.cvf)
error.cv
mean(error.cv)   ## Average MSE
## Plot of the MSE errors
plot(error.cv, pch=19, type="b")
## Boxplot of the MSE error
boxplot(error.cv,xlab='MSE in cross validation',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for ANN',horizontal=TRUE)

mean(error.cv); MSE.ann; MSE_lm
#-------------------------------------------------------------------------------

# ANN

# Creamos el regresor de la red neuronal con el conjunto de trainning
regressor_ann2 = neuralnet(Status ~ Age + Year + Nodes,
                           training_set_n, hidden = c(3,3,2),
                           linear.output = T, threshold = 0.01)
plot(regressor_ann2)

# Vamos a predecir el conjunto de testing con nuestro regresor que ya esta entrenado
y_pred2 = predict(regressor_ann2, testing_set_n[, -4])

# Lo desnormalizamos para poder intrepetar el resultado
y_pred_ann2 = y_pred2*
  (max(data$Status)-min(data$Status))+min(data$Status)

#Vamos a evaluar el modelo, para ver si nos ha dado una predicción acertada
actual2 = testing_set$Status
cbind(actual2,y_pred_ann2)

# Calculamos el MSE
n2 = nrow(testing_set)
MSE.ann2 <- sum((actual2 - y_pred_ann2)^2)/n2
MSE.ann2

# Arbol de Decision

# Creamos el regresor para el Arbol de Decisión
regressor_tree = rpart(formula = Status ~ .,
                   data = training_set_n,
                   control = rpart.control(minsplit = 1))

y_pred_tree = predict(regressor_tree, testing_set_n[, -4])

# Calculamos el MSE
MSE_tree <- sum((actual2 - y_pred_tree)^2)/nrow(training_set)
MSE_tree

# k-fold CROSS VALIDATION

error.cv <- NULL
k <- 10
## to control the process we create a progress bar to know the status of CV
progress.bar <- create_progress_bar('text')
progress.bar$init(k)
## the loop to implement cross validation 
folds <- sample(rep(1:10,length = nrow(data_n)))
## the loop to implement cross validation
for(i in 1:k){
  ## select rows with index=1 from Boston_n to create test set
  ## select remaining rows i to create training set
  trainset.cv <- data_n[folds!=i,]
  testset.cv <- data_n[folds==i,]
  ## the neural net
  ANN2.cv <- neuralnet(Status ~ Age + Year + Nodes,
                       training_set_n, hidden = c(3,3,2),
                       linear.output = T, threshold = 0.01)
  ## propagating the input
  pr.ANN2.cv <- predict(ANN2.cv, testset.cv[,1:3])
  ## denormalize data for prediction
  pr.ANN2.cvf<-pr.ANN2.cv*(max(data$Status)-min(data$Status))+ min(data$Status)
  ## actual data
  test.cvf<-testset.cv$Status*(max(data$Status)-min(data$Status))+min(data$Status)
  n<-length(test.cvf)
  error.cv[i] <- sum((test.cvf - pr.ANN2.cvf)^2)/n
  progress.bar$step()
}

cbind(test.cvf,pr.ANN2.cvf)
error.cv
mean(error.cv)   ## Average MSE
## Plot of the MSE errors
plot(error.cv, pch=19, type="b")
## Boxplot of the MSE error
boxplot(error.cv,xlab='MSE in cross validation',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for ANN',horizontal=TRUE)

mean(error.cv); MSE.ann2; MSE_tree

#-------------------------------------------------------------------------------

# ANN

# Creamos el clasificador de la red neuronal con el conjunto de trainning
regressor_ann3 = neuralnet(Status ~ Age + Year + Nodes,
                            training_set_n, hidden = c(3,3,2,2),
                            linear.output = T, threshold = 0.1)
plot(regressor_ann3)

# Vamos a predecir el conjunto de testing con nuestro classifier que ya esta entrenado
y_pred3 = predict(regressor_ann3, testing_set_n[, -4])

# Lo desnormalizamos para poder intrepetar el resultado
y_pred_ann3 = y_pred3*
  (max(data$Status)-min(data$Status))+min(data$Status)

#Vamos a evaluar el modelo, para ver si nos ha dado una predicción acertada
actual3 = testing_set$Status
cbind(actual3,y_pred_ann3)

# Calculamos el MSE
n3 = nrow(testing_set)
MSE.ann3 <- sum((actual3 - y_pred_ann3)^2)/n
MSE.ann3

# SVM

# Creamos el regresor para el SVM
regressor_svr = svm(formula = Status ~ ., 
                 data = training_set_n, 
                 type = "eps-regression", 
                 kernel = "radial")

y_pred_svr = predict(regressor_svr, testing_set_n[, -4])


# Calculamos el MSE
MSE_svr<- sum((actual - y_pred_svr)^2)/nrow(training_set)
MSE_svr

# k-fold CROSS VALIDATION

error.cv <- NULL
k <- 10
## to control the process we create a progress bar to know the status of CV
progress.bar <- create_progress_bar('text')
progress.bar$init(k)
## the loop to implement cross validation 
folds <- sample(rep(1:10,length = nrow(data_n)))
## the loop to implement cross validation
for(i in 1:k){
  ## select rows with index=1 from Boston_n to create test set
  ## select remaining rows i to create training set
  trainset.cv <- data_n[folds!=i,]
  testset.cv <- data_n[folds==i,]
  ## the neural net
  ANN3.cv <- neuralnet(Status ~ Age + Year + Nodes,
                       training_set_n, hidden = c(3,3,2),
                       linear.output = T, threshold = 0.01)
  ## propagating the input
  pr.ANN3.cv <- predict(ANN3.cv, testset.cv[,1:3])
  ## denormalize data for prediction
  pr.ANN3.cvf<-pr.ANN3.cv*(max(data$Status)-min(data$Status))+ min(data$Status)
  ## actual data
  test.cvf<-testset.cv$Status*(max(data$Status)-min(data$Status))+min(data$Status)
  n<-length(test.cvf)
  error.cv[i] <- sum((test.cvf - pr.ANN3.cvf)^2)/n
  progress.bar$step()
}

cbind(test.cvf,pr.ANN3.cvf)
error.cv
mean(error.cv)   ## Average MSE
## Plot of the MSE errors
plot(error.cv, pch=19, type="b")
## Boxplot of the MSE error
boxplot(error.cv,xlab='MSE in cross validation',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for ANN',horizontal=TRUE)

mean(error.cv); MSE.ann3; MSE_svr


