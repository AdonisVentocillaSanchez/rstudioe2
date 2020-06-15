#####################
#DATA FRAME

datos=read.csv("https://raw.githubusercontent.com/luisatp/ExamenE2/master/master.csv", sep = ";")
head(datos)
summary(datos)


#Coerción
datos$sex=as.factor(datos$sex)
datos$age=as.factor(datos$age)
datos$ï..country=as.factor(datos$ï..country)
