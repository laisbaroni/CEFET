#intstalação de pacotes
pre_install_libraries <- function()  
{
  install.packages("ggplot2")
  install.packages("caret")
  
  install.packages("glmnet")
  install.packages("leaps")
  
  install.packages("rJava")
  install.packages("RWeka")
  install.packages("RWekajars")
  install.packages("FSelector")
  
  install.packages("doBy")
  install.packages("corrplot")
  install.packages("tidyverse")
  install.packages("e1071")
  install.packages("arules")
  install.packages("naivebayes")
}

pre_load_libraries <- function() 
{
  library(caret)
  
  library(glmnet)
  library(leaps)
  
  library(rJava)
  library(RWeka)
  library(RWekajars)
  library(FSelector)
  
  library(doBy)  
}
#Abrindo o dataset
mydata <- read.csv("covtype.csv", header= TRUE)

#Análise exploratória
#sumário dos dados
summary(mydata$Elevation)
summary(mydata$Aspect)
summary(mydata$Slope)
summary(mydata$Horizontal_Distance_To_Hydrology)
summary(mydata$Hillshade_Noon)
summary(mydata$Cover_Type)

#Legenda dos Plots
covertype <- c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz")
color_covertype <- c("orangered", "lightblue", "pink", "grey", "green", "yellow", "purple")
legend("topright", covertype, fill=color_covertype, title="Cobertura Florestal")

#Gráficos
#Gráfico em Barras de frequências para a "cover"
#barplot(mydata$Cover_Type, main= "Histograma da Cobertura Florestal", col={"gray"}, xlab="Tipo de Cobertura Florestal", ylab="Frequência", labels=TRUE, breaks=8, ylim=c(0, 530000))
#barplot(table(mydata$Cover_Type), main= "Frequência da Cobertura Florestal", col=color_covertype, xlab="Tipo de Cobertura Florestal", ylab="Frequência", ylim=c(0,300000), names.arg=covertype, srt=45)
barplot(table(mydata$Cover_Type), main= "Frequência da Cobertura Florestal", col=color_covertype, xlab="Tipo de Cobertura Florestal", ylab="Frequência", ylim=c(0,300000), axisnames=FALSE)

#histograma combinado das variáveis
par(mfrow=c(2,3))
hist(mydata$Elevation, main= "Histograma da Elevação", col={"gray"}, xlab="Elevação (m)", ylab="Frequência", ylim=c(0, 100000))
hist(mydata$Aspect,main= "Histograma do Aspecto", col={"gray"}, xlab="Aspecto (°)", ylab="Frequência")
hist(mydata$Slope, main= "Histograma da Declividade", col={"gray"}, xlab="Declividade (°)", ylab="Frequência", ylim=c(0, 200000))
hist(mydata$Horizontal_Distance_To_Hydrology, main= "Histograma da Distância p/ Hidrologia", col={"gray"}, xlab="Distância (m)", ylab="Frequência", ylim=c(0, 80000))
hist(mydata$Hillshade_Noon, main= "Histograma do Relevo Sombreado", col={"gray"}, xlab="ND", ylab="Frequência", xlim=c(100, 300), ylim=c(0, 140000))
hist(mydata$Cover_Type, main= "Histograma da Cobertura", col={"gray"}, xlab="Cobertura Florestal", ylab="Frequência", ylim=c(0, 300000))

#Gráfico pareado (comparação de variáveis 2 a 2)
#Subset do dataset (apenas um amostra de 100 valores para os atributos de interesse)
mydata2<-mydata[,c(1, 2, 3, 4)]
mydata2<-mydata2[sample(1:nrow(mydata2),100,replace=F),]
pairs(mydata2, main = "Gráfico de Dispersão entre Variáveis", pch = 21, bg = c("orangered", "lightblue", "pink", "grey", "green", "yellow", "purple"))

#Boxplot
par(mfrow=c(1,3))
boxplot(mydata$Elevation)
boxplot(mydata$Aspect)
boxplot(mydata$Slope)

par(mfrow=c(1,4))
boxplot(mydata$Horizontal_Distance_To_Roadways)
boxplot(mydata$Horizontal_Distance_To_Fire_Points)
boxplot(mydata$Horizontal_Distance_To_Hydrology)
boxplot(mydata$Vertical_Distance_To_Hydrology)


#Mapa de Calor
library(corrplot)
M <- cor(mydata[,c(1, 2, 3, 7, 8, 9, 55)]) # get correlations
corrplot(M, method = "number", main="Correlation between variables") #plot matrix