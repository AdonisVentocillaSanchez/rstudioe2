############################
##Conjunto de datos
dbdiabetes=read.csv("https://raw.githubusercontent.com/luisatp/ExamenParcial/master/diabetes.csv", sep = ";")
head(dbdiabetes)

#Observamos el tipo de datos
str(dbdiabetes)

#Extraemos un resumen de los datos
summary(dbdiabetes)

######################
#Coerción

dbdiabetes$Resultado=as.factor(dbdiabetes$Resultado)
summary(dbdiabetes)

########################
#Exploración de datos
hist(dbdiabetes$Glucosa)

install.packages("ggplot2")
library(ggplot2)
ggplot(data=dbdiabetes)+
  geom_histogram(mapping = aes(x=Glucosa))

boxplot(dbdiabetes$Glucosa)

hist(dbdiabetes$Insulina)
boxplot(dbdiabetes$Insulina)

plot(dbdiabetes$Glucosa, dbdiabetes$Insulina)
install.packages("VIM")
library(VIM)
nulos=aggr(dbdiabetes)
summary(nulos)

######################################################
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
