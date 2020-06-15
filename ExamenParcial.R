#####################
#DATA FRAME

datos=read.csv("https://raw.githubusercontent.com/luisatp/ExamenE2/master/master.csv", sep = ";")
head(datos)


#Coerción
datos$sex=as.factor(datos$sex)
datos$ï..country=as.factor(datos$ï..country)
datos$country.year=as.factor(datos$country.year)
datos$age=as.factor(datos$age)
datos$generation=as.factor(datos$generation)
datos$year=as.factor(datos$year)

#Retiro de variable

datos=datos[,c(1:8,12)]


summary(datos)


str(datos)
########################
#Exploración de datos
hist(datos$year)

install.packages("ggplot2")
library(ggplot2)
ggplot(data=datos)+
  geom_histogram(mapping = aes(x=age, color=Mora))
