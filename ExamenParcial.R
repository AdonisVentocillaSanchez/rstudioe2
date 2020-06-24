############################
##Conjunto de datos
dbdiabetes=read.csv("https://raw.githubusercontent.com/luisatp/ExamenParcial/master/diabetes.csv", sep = ";")
head(dbdiabetes)

#Objetivo: Construir un modelo de clasificación de minería de datos que
#permita clasificar si una mujer no menor de 21 años es diabetica o no

##########################
#ENTENDIMIENTO DE LA DATA
##########################
#Data inicial

#Embarazos: Representa los embarazos de cada uno de los usuarios
#Glucosa: Representa los Glucosa de cada uno de los usuarios
#Presión.arterial: Representa los Presión.arterial de cada uno de los usuarios
#Insulina: la Insulinade cada usuario
#Índice.de.masa.corporal: Representa el Índice.de.masa.corporal de cada uno de los usuarios
#Función.de.pedigrí.de.la.diabetes: Nivel de pedigrí.de.la.diabetes del usuario
#Edad: Edad de los usuarios
#Resultado: Si un cliente es o no diabetico (1 = moroso y 0= no moroso)


#Observamos el tipo de datos
str(dbdiabetes)
# Se cuenta con un conjunto de datos de 768 observaciones(fila) y 8 variables (columnas)
# La variable objetivo es "Resultado" por lo que 1 representa si el cliente es diabetico y 0 si el cliente no lo es
# podemos apreciar que hay 500 diabetico y 268 no diabetico

#Extraemos un resumen de los datos
summary(dbdiabetes)

######################
#Coerción

dbdiabetes$Resultado=as.factor(dbdiabetes$Resultado)
summary(dbdiabetes)

########################
#Exploración de datos
hist(dbdiabetes$Edad)

install.packages("ggplot2")
library(ggplot2)
ggplot(data=dbdiabetes)+
  geom_histogram(mapping = aes(x=Edad))

#Según el gráfico observamos que la mayoría de clientes tiene edades entre
#25 y 30 años

boxplot(dbdiabetes$Edad)
#Según el gráfico se observa pocos valores atípicos en la variable edad

hist(dbdiabetes$Insulina)
#Según el gráfico observamos que la mayoría de clientes tiene edades entre
#0 y 100
boxplot(dbdiabetes$Insulina)
#Según el gráfico se observa valores atípicos en la variable edad

plot(dbdiabetes$Edad, dbdiabetes$Insulina)

install.packages("VIM")
library(VIM)
nulos=aggr(dbdiabetes)
summary(nulos)


########################
#calidad de datos

#En la variable que contamos en esta base de datos no estariamos contando con valores nulos, negativos o Na,
#por lo que se ve en este grafico 0 Nulos
###################################################

######################################################
# Del conjunto de datos, solo no seleccionaremos el indice de masa corporal por ser unica en cada registro
head(dbdiabetes)
dbdiabetes=dbdiabetes[,c(1:3,5:9)]
head(dbdiabetes)

#Arboles de decisión
#Construir la data train y test
install.packages("caret")
library(caret)
set.seed(123)
particion=createDataPartition(y = dbdiabetes$Resultado, p = 0.70, list = FALSE, times = 1)
train=dbdiabetes[particion,]
test=dbdiabetes[-particion,]
dim(train)
dim(test)

#Modelo árbol por defecto
library(rpart)
modelo1=rpart(Resultado~.,data=train,method = "class", minsplit=0, cp=0.0016)
modelo1
summary(modelo1)

library(partykit)
plot(as.party(modelo1))
##################################
#Evaluación del modelo
head(test)
#Predecir en data de prueba utilizando el modelo
predichos=predict(modelo1, test, type = "class")
predichos
comparacion=cbind(test$Resultado, predichos)
head(comparacion)
fix(comparacion)
#Matriz de confusión
matriz_confusion=table(predichos, test$Resultado)
matriz_confusion
table(train$Resultado)
table(test$Resultado)
#################
#Medidas de rendimiento del modelo
library(caret)
medidas=confusionMatrix(predichos, test$Resultado)
medidas
#################################
##################################
#curva roc
str(test)
pred_prob<-predict(modelo1, test, type = "prob")[,2]
pred_prob
library(ROCR)
predR1 <- prediction(pred_prob, test$Resultado)
predR2<-performance(predR1, "tpr", "fpr")
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)

#Indice GINI
ROCRN <- round(performance(predR1, measure = "auc")@y.values[[1]]*100, 2)
giniRN <- (2*ROCRN - 100)
giniRN
#########################################################
#######################################################
#KNN
str(train)
cov(train[,1:7])
install.packages("DMwR")
library(VIM)
library(DMwR)
library(e1071)
library(class)
modelo_knn=knn(train, test, train$Resultado, k=2)
modelo_knn
table(modelo_knn, test$Resultado)
library(ggplot2)
library(caret)
medidas_knn=confusionMatrix(modelo_knn, test$Resultado)
medidas_knn
#Curva ROC

pred_prob<-predict(medidas_knn, test, type = "prob")[,2]
pred_prob
library(ROCR)
predR1 <- prediction(pred_prob, test$Resultado)
predR2<-performance(predR1, "tpr", "fpr")
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)

#Indice GINI
ROCRN <- round(performance(predR1, measure = "auc")@y.values[[1]]*100, 2)
giniRN <- (2*ROCRN - 100)
giniRN
##################################
###############################
#redes neuronales

install.packages("nnet")
library(nnet)
head(train)
str(train)
modelo_nnet=nnet(Resultado~., data=train,size = 70, trace=F,method = "class",maxit=1000)
summary(modelo_nnet)
predichos_nnet=predict(modelo_nnet,test,type = "class")
predichos_nnet=as.factor(predichos_nnet)
indicadores_nnet=confusionMatrix(predichos_nnet,test$Resultado)
indicadores_nnet

#Curva ROC

pred_prob2<-predict(modelo_nnet, test, type = "prob")[,2]
pred_prob2
library(ROCR)
predR1 <- prediction(pred_prob, test$Resultado)
predR2<-performance(predR1, "tpr", "fpr")
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)

#Indice GINI
ROCRN <- round(performance(predR1, measure = "auc")@y.values[[1]]*100, 2)
giniRN <- (2*ROCRN - 100)
giniRN
##################################
#################################
#Naive bayes

head(train)
table(train$Embarazos, train$Resultado)

library(e1071)
modelo_bayes=naiveBayes(Resultado~.,data = train,method="class")
modelo_bayes
summary(modelo_bayes)
predichos_bayes=predict(modelo_bayes, test, type = "class")
predichos_bayes
table(predichos_bayes, test$Resultado)

library(caret)
indicadores=confusionMatrix(predichos_bayes, test$Resultado)
indicadores

#Curva ROC

pred_prob2<-predict(modelo_bayes, test, type = "prob")[,2]
pred_prob2
library(ROCR)
predR1 <- prediction(pred_prob, test$Resultado)
predR2<-performance(predR1, "tpr", "fpr")
plot(predR2, colorize = T)
lines(x=c(0, 1), y=c(0, 1), col=" blue", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="red", lwd=1, lty=4)

#Indice GINI
ROCRN <- round(performance(predR1, measure = "auc")@y.values[[1]]*100, 2)
giniRN <- (2*ROCRN - 100)
giniRN