#####################
#DATA FRAME

datos=read.csv("https://raw.githubusercontent.com/luisatp/ExamenE2/master/master.csv", sep = ";")
head(datos)


#Coerción
datos$sex=as.factor(datos$sex)
datos$ï..country=as.factor(datos$ï..country)
datos$country.year=as.factor(datos$country.year)
datos$age=as.integer(datos$age)
datos$generation=as.factor(datos$generation)
datos$year=as.factor(datos$year)

#Retiro de variable

datos=datos[,c(1:8,12)]


summary(datos)
#ï..country.-
#year.-
#sex.-
#age.-
#sucides_numero.-
#poblation.-
#tasa de suicidiio.-
#pais por año.-
#generacion.-

str(datos)
########################
#Exploración de datos
hist(datos$suicides.100k.pop)

install.packages("ggplot2")
library(ggplot2)
ggplot(data=datos)+
  geom_histogram(mapping = aes(x=suicides.100k.pop))


boxplot(datos$suicides.100k.pop)
