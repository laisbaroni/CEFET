##Abrindo DataSet
mydata <- read.csv("covtype.csv", header= TRUE)
##Categorização de Variáveis usando fç "discretize"##
#Obs.: ou uso o primeiro ou o segundo.
# Categorização com iguais intervalos de frequência
library(arules)
mydata$Elevation <- discretize(mydata$Elevation,categories = 10, method="frequency")
mydata$Aspect <- discretize(mydata$Aspect,categories = 10, method="frequency")
mydata$Slope <- discretize(mydata$Slope,categories = 10, method="frequency")
mydata$Horizontal_Distance_To_Hydrology <- discretize(mydata$Horizontal_Distance_To_Hydrology,categories = 10, method="frequency")
mydata$Vertical_Distance_To_Hydrology <- discretize(mydata$Vertical_Distance_To_Hydrology,categories = 10, method="frequency")
mydata$Horizontal_Distance_To_Roadways <- discretize(mydata$Horizontal_Distance_To_Roadways,categories = 10, method="frequency")
mydata$Hillshade_Noon <- discretize(mydata$Hillshade_Noon,categories = 10, method="frequency")
mydata$Horizontal_Distance_To_Fire_Points <- discretize(mydata$Horizontal_Distance_To_Fire_Points,categories = 10, method="frequency")

# Categorização com intervalos equidistantes
mydata$Elevation <- discretize(mydata$Elevation,categories = 10)
mydata$Aspect <- discretize(mydata$Aspect,categories = 10)
mydata$Slope <- discretize(mydata$Slope,categories = 10)
mydata$Horizontal_Distance_To_Hydrology <- discretize(mydata$Horizontal_Distance_To_Hydrology,categories = 10)
mydata$Vertical_Distance_To_Hydrology <- discretize(mydata$Vertical_Distance_To_Hydrology,categories = 10)
mydata$Horizontal_Distance_To_Roadways <- discretize(mydata$Horizontal_Distance_To_Roadways,categories = 10)
mydata$Hillshade_Noon <- discretize(mydata$Hillshade_Noon,categories = 10)
mydata$Horizontal_Distance_To_Fire_Points <- discretize(mydata$Horizontal_Distance_To_Fire_Points,categories = 10)


##Gráficos para a Categorização##

##Gráficos antes da Categorização
par(mfrow=c(2,4))
hist(mydata$Elevation, main= "Elevação Antes", col={"gray"}, xlab="Elevação (m)", ylab="Frequência", ylim=c(0, 100000))
hist(mydata$Aspect, main= "Aspecto Antes", col={"gray"}, xlab="Aspecto (º)", ylab="Frequência", ylim=c(0, 58000))
hist(mydata$Slope, main= "Declividade Antes", col={"gray"}, xlab="Declividade (º)", ylab="Frequência", ylim=c(0, 200000))
hist(mydata$Horizontal_Distance_To_Hydrology, main= "Dh Hidrologia Antes", col={"gray"}, xlab="Dh Hidrologia (m)", ylab="Frequência", ylim=c(0, 80000))
hist(mydata$Horizontal_Distance_To_Roadways, main= "Dh Rodovia Antes", col={"gray"}, xlab="Dh Rodovia (m)", ylab="Frequência", ylim=c(0, 100000))
hist(mydata$Horizontal_Distance_To_Fire_Points, main= "Dh Pt de Fogo Antes", col={"gray"}, xlab="Dh Pt de Fogo (m)", ylab="Frequência", ylim=c(0, 120000))
hist(mydata$Vertical_Distance_To_Hydrology, main= "Dv Hidrologia Antes", col={"gray"}, xlab="Dv Hidrologia (m)", ylab="Frequência", ylim=c(0, 300000))
hist(mydata$Hillshade_Noon, main= "Hillshade Antes", col={"gray"}, xlab="Hillshade", ylab="Frequência", ylim=c(0, 150000))

##Gráficos depois da Categorização
#Intervalos mesma frequência
par(mfrow=c(2,4))
barplot(table(mydata$Elevation),  main= "Elevação Depois", col={"gray"}, xlab="Intervalo de Elevação (m)", ylab="Frequência", ylim=c(0, 70000), cex.names = 0.8, las=2)
barplot(table(mydata$Aspect),  main= "Aspecto Depois", col={"gray"}, xlab="Direção do Aspecto", ylab="Frequência", ylim=c(0, 70000), cex.names = 0.8, las=2)
barplot(table(mydata$Slope),  main= "Declividade Depois", col={"gray"}, xlab="Intervalo de Declividade", ylab="Frequência", ylim=c(0, 100000), cex.names = 0.8, las=2)
barplot(table(mydata$Horizontal_Distance_To_Hydrology),  main= "Dh Hidrologia Depois", col={"gray"}, xlab="Intervalo de Dh Hidrologia", ylab="Frequência", ylim=c(0, 70000), cex.names = 0.8, las=2)
barplot(table(mydata$Horizontal_Distance_To_Roadways),  main= "Dh Rodovias Depois", col={"gray"}, xlab="Intervalo de Dh Rodovia", ylab="Frequência", ylim=c(0, 60000), cex.names = 0.8, las=2)
barplot(table(mydata$Horizontal_Distance_To_Fire_Points),  main= "Dh Pt de Fogo Depois", col={"gray"}, xlab="Intervalo de Dh Pt de Fogo", ylab="Frequência", ylim=c(0, 70000), cex.names = 0.8, las=2)
barplot(table(mydata$Vertical_Distance_To_Hydrology),  main= "Dv Hidrologia Depois", col={"gray"}, xlab="Intervalo de Dv Hidrologia", ylab="Frequência", ylim=c(0, 100000), cex.names = 0.8, las=2)
barplot(table(mydata$Hillshade_Noon),  main= "Hillshade Depois", col={"gray"}, xlab="Intervalo de Hillshade", ylab="Frequência", ylim=c(0, 70000), cex.names = 0.8, las=2)
#Intervalos equidistantes
barplot(table(mydata$Elevation),  main= "Elevação Depois", col={"gray"}, xlab="Intervalo de Elevação (m)", ylab="Frequência", ylim=c(0, 200000), cex.names = 0.8, las=2)
barplot(table(mydata$Aspect),  main= "Aspecto Depois", col={"gray"}, xlab="Direção do Aspecto", ylab="Frequência", ylim=c(0, 100000), cex.names = 0.8, las=2)
barplot(table(mydata$Slope),  main= "Declividade Depois", col={"gray"}, xlab="Intervalo de Declividade", ylab="Frequência", ylim=c(0, 250000), cex.names = 0.8, las=2)
barplot(table(mydata$Horizontal_Distance_To_Hydrology),  main= "Dh Hidrologia Depois", col={"gray"}, xlab="Intervalo de Dh Hidrologia", ylab="Frequência", ylim=c(0, 200000), cex.names = 0.8, las=2)
barplot(table(mydata$Horizontal_Distance_To_Roadways),  main= "Dh Rodovias Depois", col={"gray"}, xlab="Intervalo de Dh Rodovia", ylab="Frequência", ylim=c(0, 150000), cex.names = 0.8, las=2)
barplot(table(mydata$Horizontal_Distance_To_Fire_Points),  main= "Dh Pt de Fogo Depois", col={"gray"}, xlab="Intervalo de Dh Pt de Fogo", ylab="Frequência", ylim=c(0, 200000), cex.names = 0.8, las=2)
barplot(table(mydata$Vertical_Distance_To_Hydrology),  main= "Dv Hidrologia Depois", col={"gray"}, xlab="Intervalo de Dv Hidrologia", ylab="Frequência", ylim=c(0, 400000), cex.names = 0.8, las=2)
barplot(table(mydata$Hillshade_Noon),  main= "Hillshade Depois", col={"gray"}, xlab="Intervalo de Hillshade", ylab="Frequência", ylim=c(0, 300000), , cex.names = 0.8, las=2)


##Construção de Atributos##
#as tabelas soil_type de 1 a 40 precisam ser juntadas, assim como as tabelas wildeness_area de 1 a 4
library(plyr)
library(dplyr)
library(tidyr)

#fazendo nova coluna com as wilderness areas juntas
mydata <- mydata %>% 
  gather(key=wilderness_area, value=area.indicator, Wilderness_Area1:Wilderness_Area4) %>%
  filter(area.indicator == 1 ) %>%
  select(-area.indicator)
# Renomeando as áreas
mydata$wilderness_area <- ifelse(mydata$wilderness_area=="Wilderness_Area1","Rawah",
                                 ifelse(mydata$wilderness_area=="Wilderness_Area2","Neota",
                                        ifelse(mydata$wilderness_area=="Wilderness_Area3","Comanche Peak",
                                               "Cache la Poudre")))

#fazendo nova coluna com as soil type juntas
mydata <- mydata %>% 
  gather(key=Soil, value=soil.indicator, Soil_Type1:Soil_Type40) %>% 
  filter(soil.indicator == 1 ) %>%
  select(-soil.indicator)

##Gráficos dos Novos Atributos##
par(mfrow=c(1,1))
# Gráfico de Frequências dos Tipos de Solo
barplot(table(mydata$Soil), ylim=c(0,130000), space=.8, width=c(.2,.2), main="Número de árvores por tipo de solo", xlab="Tipo de Solo", ylab="Frequência",  cex.names = 0.8, las=2)
# Gráfico de Frequências da area de preservação
barplot(table(mydata$wilderness_area), ylim=c(0,300000), space=.8, width=c(.2,.2), main="Número de árvores por área florestal", xlab="Área Florestal",  cex.names = 0.8, ylab="Frequência")

#Escolha das colunas de interesse (descartando hillshade 9am e 3pm)
mydata <- mydata[,c(1,2,3,4,5,6,8,10,11,12,13)]

##Salvando Novo Dataset
write.csv(mydata, file = "mynewdata.csv",row.names=TRUE)