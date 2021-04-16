#########################################################
# DEA 1                                     #
#########################################################


library(Hmisc)
datosmark <- read.table(file = 'D://Maestría en Estadística/Minería de datos/Base de datos/DirectMarketing.csv',sep = ',',header = TRUE)
names(datosmark)
describe(datosmark$Age)
describe(datosmark$Gender)
describe(datosmark$OwnHome)
describe(datosmark$Married)
describe(datosmark$Location)
describe(datosmark$Salary)
describe(datosmark$Children)
describe(datosmark$History)
describe(datosmark$Catalogs)
describe(datosmark$AmountSpent)
boxplot(datosmark$Salary,horizontal = T)
boxplot(datosmark$AmountSpent,horizontal = T)
install.packages("fBasics")
hist(datosmark$Salary)
str(datosmark)
summary(datosmark[,c(4,5,6)])
summary(datosmark)
barplot(table((datosmark['Gender'])))
barplot(table((datosmark['Married'])))
barplot(table((datosmark['Age'])))
library (rattle)
summary(datosmark['Age'])
hist(datosmark['Age'])
library(fBasics)
hist(datosmark$Salary)
basicStats(datosmark$Salary)
basicStats(datosmark$AmountSpent)

library('mice')
md.pattern(datosmark[,c(7,8,9,10)])
md.pattern(datosmark)
barplot(table((datosmark['History'])))
boxplot(datosmark$Salary~datosmark$AmountSpent, horizontal = TRUE)
dim(datosmark)
names(datosmark)
head(datosmark)
str(datosmark)

install.packages('gmodels')
library(gmodels)
CrossTable(datosmark$Age, format="SAS")
CrossTable(datosmark$Age, format="SPSS")

#Gráfico de barras
par(mfrow=c(2,2))
barplot(table(datosmark$Age), main="Distribución de la Edad de los Clientes", 
        col=1,xlab="Edad",ylab="# de Clientes")
barplot(prop.table(table(datosmark$Age))*100,
        main="Distribución de la Edad de los Clientes", 
        col=3,xlab="Edad",ylab="% de Clientes")
barplot(table(datosmark$Location), main="Distribución de la Ubicación de los Clientes", 
        col=7,horiz=TRUE,xlab="Ubicación",ylab="# de Clientes")
barplot(table(datosmark$Location)/length(datosmark$Location)*100,
        main="Distribución de la Ubicación de los Clientes", 
        col=7,horiz=TRUE,xlab="Ubicación",ylab="% de Clientes")
#par(mfrow=c(1,1))

#install.packages('ggplot2')
#install.packages('scales')
#install.packages('grid')
#install.packages('gridExtra')
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

## no funciona
p1<-ggplot(data=datosmark, aes(x=Age))+ geom_bar(colour="black",fill="#DD8888", width=.7, stat="bin") + xlab("Edad") + ylab("# de Clientes") +
  ggtitle("Distribución de la Edad de los Clientes")

p2<-ggplot(data=datosmark, aes(x=Age))+ geom_bar(aes(y = (..count..)/sum(..count..)*100),colour="black", fill="#DD8888", width=.7, stat="bin") +
  xlab("Edad") + ylab("% de Clientes") + ggtitle("Distribuci{on de la Edad de los Clientes")

p3<-ggplot(data=datosmark, aes(x=Location))+  geom_bar(colour="black",fill="#E69F00", width=.7, stat="bin") + xlab("Ubicaci?n") + ylab("# de Clientes") + coord_flip() +
  ggtitle("Distribución de la Ubicación de los Clientes")

p4<-ggplot(data=datosmark, aes(x=Location))+  geom_bar(aes(y = (..count..)/sum(..count..)*100),colour="black", fill="#E69F00", width=.7, stat="bin") +
  xlab("Ubicaci?n") + ylab("% de Clientes") + coord_flip() + ggtitle("Distribuci?n de la Ubicación de los Clientes")

#grid.arrange(p1, p2, p3, p4,ncol = 4)

####

lbls1 <- paste(names(table(datosmark$Age)), "\n",prop.table(table(datosmark$Age))*100,"%", sep="")
par(mfrow=c(1,1))
pie(table(datosmark$Age), labels = lbls1,   main="Distribución de la Edad de los Clientes")


#install.packages('plotrix')
library(plotrix)
pie3D(table(datosmark$Age),labels=lbls1,explode=0.1, main="Distribución de la Edad de los Clientes")

ggplot(datosmark, aes(x = factor(""), fill = Age) ) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_polar(theta = "y") +
  scale_x_discrete("percent") +
  scale_y_continuous(labels = percent_format())

####################################################################
#  Representación de Datos Cuantitativos Discretos                 #
####################################################################
#Tabla de Frecuencias
table(datosmark$Children)
prop.table(table(datosmark$Children))

plot(table(datosmark$Children), type="h")
plot(prop.table(table(datosmark$Children)), type="h", lwd=5, ylab="Prop.")

stem(datosmark$Salary)

stem(datosmark$Salary[datosmark$Age=="Young"])
stem(datosmark$Salary[datosmark$Age=="Middle"])
stem(datosmark$Salary[datosmark$Age=="Old"])
summary(datosmark)

stripchart(datosmark$Salary)
stripchart(datosmark$Salary,method = "stack")
stripchart(datosmark$Salary,method = "jitter")

install.packages('barcode')
library(barcode)
barcode(datosmark$Salary)

stripchart(datosmark$Salary~datosmark$Age)

h<-hist(datosmark$Salary) #Frecuencias Observadas
hist(datosmark$Salary,freq = FALSE)

#install.packages('agricolae')
library(agricolae)

(table.freq(hist(datosmark$Salary,breaks = "Sturges"))) #Regla Sturges
(table.freq(hist(datosmark$Salary,breaks = "Scott")))   #Regla de Scott
(table.freq(hist(datosmark$Salary,breaks = "FD")))      #Regla de Friedman-Diaconis
(table.freq(graph.freq(datosmark$Salary,plot=FALSE)))   #Regla Sturges (Agricolae)

par(mfrow=c(1,3))
hist(datosmark$Salary[datosmark$Age=="Young"],ylim=c(0,130))
hist(datosmark$Salary[datosmark$Age=="Middle"],ylim=c(0,130))
hist(datosmark$Salary[datosmark$Age=="Old"],ylim=c(0,130))
par(mfrow=c(1,1))

h1<-hist(datosmark$Salary,border=FALSE)
polygon.freq(h1,frequency=1,col="red")

h<-graph.freq(datosmark$Salary,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE)
print(points)

hist(datosmark$Salary,prob=TRUE)
lines(density(datosmark$Salary))
plot(density(datosmark$Salary))

library(Hmisc)

Ecdf(datosmark$Salary)

salary1<-datosmark$Salary[datosmark$Age=="Young"]
salary2<-datosmark$Salary[datosmark$Age=="Middle"]
salary3<-datosmark$Salary[datosmark$Age=="Old"]
Ecdf(salary1,col="red",xlab="Salario",subtitles=FALSE)
Ecdf(salary2,col="blue", lty=2,subtitles=FALSE, add=TRUE)
Ecdf(salary3,col="green", lty=3,subtitles=FALSE, add=TRUE)

boxplot(datosmark$Salary)

boxplot(datosmark$Salary~datosmark$Age)

#####################
## Tablas de contingencia
#####################

# Existe asociación entre la historia de compra anterior y la locación o la tenencia de casa del cliente?

tt1 = table(datosmark$Location,datosmark$History)
tt2 = table(datosmark$OwnHome,datosmark$History)
margin.table(tt1,1)
margin.table(tt1,2)
margin.table(tt1)
addmargins(tt1)
addmargins(prop.table(tt1,2))
addmargins(prop.table(tt2,1))

# Gráficos de barras agrupadas y componentes
barplot(tt1,xlab='Historia de compras',main='Grafico de barras componentes localización por historia')
# Como se aprecia el default es un gráfico de barras componentes sin leyendas

barplot(tt1,xlab='Historia de compras',main='Gráfico de barras componentes localización por historia',legend.text=TRUE,col=c("darkblue","red"))

barplot(t(tt1),xlab='Localización',main='Gráfico de barras componentes historia por localización',legend.text=TRUE)
# Gr?fico de barras agrupadas

barplot(tt1,xlab='Historia de compras',main='Gráfico de barras agrupadas localización por historia',legend.text=TRUE, beside=TRUE)

# Existe aparente asociaci?n entre la historia de compras y la tenencia de casa?
#library(ggplot2)
ggplot(datosmark, aes(History, fill = Location)) + geom_bar(position = "dodge") +  xlab("Historia") + ylab("Locaci?n")
DMark2 <- datosmark[!is.na(datosmark$History),]
ggplot(DMark2, aes(History, fill = Location)) + geom_bar(position = "dodge") +  xlab("Historia") + ylab("Locación")
ggplot(DMark2, aes(History, fill = Location)) + geom_bar(position = "fill") +  xlab("Historia") + ylab("Locación")
ggplot(DMark2, aes(History, fill = Location)) + geom_bar(position = "dodge") +  xlab("Historia") + ylab("Locación")




#########################################################
#  Proceso de limpieza de datos                        #
#########################################################


library(Hmisc)
library(rattle)
describe(airquality)
str(airquality)
summary(airquality)
which(colSums(is.na(airquality))!=0)
rmiss=which(rowSums(is.na(airquality))!=0,arr.ind=T)
length(rmiss)*100/dim(airquality)[1]
colmiss=c(1,2)
per.miss.col=100*colSums(is.na(airquality[,colmiss]))/dim(airquality)[1]
per.miss.col
library(VIM)
a=aggr(airquality,numbers=T)
a
summary(a)
matrixplot(airquality)
airquality.cl<-na.omit(airquality)
summary(airquality.cl)

#########################################################
#  Datos Faltantes                                      #
#########################################################

#install.packages('cluster')
library(cluster)
USArrests
data(USArrests)
head(USArrests)

USArresto.usa=daisy(scale(USArrests))
par(mfrow=c(1,3))
for(h in 2:4){
  res=kmeans(scale(USArrests),h)
  plot(silhouette(res$cluster,USArresto.usa))
}


#########################################################
#  Imputación                                   #
#########################################################

airquality.cl<-na.omit(airquality)
summary(airquality.cl)

a1<-aggr(airquality.cl,numbers=T)
a1
summary(a1)
matrixplot(airquality.cl)
airquality


#install.packages('DMwR2')
library(DMwR2)

#Imputación central

airquality.imp<-centralImputation(airquality)
matrixplot(airquality.imp)

#Imputación mediana

airquality.impmed<-initialise(airquality,method="median")
matrixplot(airquality.impmed)

#imputación k vecinos

census.k<-knnImputation(census)

bupa<-read.table(file = 'D://Maestría en Estadística/Minería de datos/Base de datos/bupa.txt',sep = ',',header = TRUE)
bupa

install.packages('mvoutlier')
library(mvoutlier)
aq.plot(bupa[bupa$V7==1,1:6],alpha=0.01)

library(reshape)
zbupa<-rescaler(x=bupa[,-7],type="sd")
summary(zbupa)

source('D://Maestría en Estadística/Minería de datos/Base de datos/dprep.r')

mmbupa<-mmnorm(bupa,minval=0,maxval=1 )[,-7]
summary(mmbupa)

dsbupa<-decscale(bupa)[,-7]
summary(dsbupa)

sigbupa<-signorm(bupa)[,-7]
summary(sigbupa)

par(mfrow=c(1,2))
plot(sort(bupa$V1))
plot(sort(sigbupa$V1))

library(DMwR2)
softbupa<-SoftMax(bupa[,-7],lambda=2*pi)
summary(softbupa)

par(mfrow=c(2,3))
boxplot(bupa[,1:6],main="bupa")
boxplot(zbupa,main="znorm bupa")
boxplot(mmbupa,main="min-max bupa")
boxplot(dsbupa,main="dec scale bupa")
boxplot(sigbupa,main="signorm bupa")
boxplot(softbupa,main="softmax bupa")


install.packages('AppliedPredictiveModeling')


################################################################################
### Caso de Estudio: Segmentación celular en Screening de alto contenido
################################################################################

library(AppliedPredictiveModeling)
data(segmentationOriginal)

## Retener el conjunto original de entrenamiento 
segTrain <- subset(segmentationOriginal, Case == "Train")

## Remover las tres primeras columnas (identificadores)
segTrainX <- segTrain[, -(1:3)]
segTrainClass <- segTrain$Class

################################################################################
### Transformación de Predictores Individuales
################################################################################

## La columna VarIntenCh3 mide la desviación estándar de la intensidad
## de los pixels en los filamentos de actinina

max(segTrainX$VarIntenCh3)/min(segTrainX$VarIntenCh3)

install.packages('e1071')
install.packages('caret')

library(e1071)
skewness(segTrainX$VarIntenCh3)

library(caret)

## Se usa la función preProcess de la librería caret para transformar debido al sesgo
segPP <- preProcess(segTrainX, method = "BoxCox")

## Aplicación de la transformación
segTrainTrans <- predict(segPP, segTrainX)

## Resultados para un predictor 
segPP$bc$VarIntenCh3

par(mfrow=c(1,2))
histogram(~segTrainX$VarIntenCh3,xlab = "Unidades Originales",type = "count")
histogram(~log(segTrainX$VarIntenCh3),xlab = "Unidades en Logaritmos",ylab = " ",type = "count")

segPP$bc$PerimCh1

histogram(~segTrainX$PerimCh1,xlab = "Unidades Originales",type = "count")

histogram(~segTrainTrans$PerimCh1,xlab = "Data Transformada",ylab = " ",type = "count")


################################################################################
### Reglas de asociación
################################################################################

install.packages('arules')
library(arules)

data("Groceries")
summary(Groceries)
labels(Groceries)
rules <- apriori(Groceries, parameter =list(supp = 0.01, conf = 0.5, target = "rules"))

inspect(rules)

install.packages('arulesViz')
library(arulesViz)
plot(rules)

subrules <- head(sort(rules, by="lift"), 10)
plot(subrules,method="graph",control=list(alpha=1))

plot(rules,method="matrix",measure="support")

plot(rules,method="matrix3D",measure="confidence")
