##Carregando algumas bibliotecas
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
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(arules)
  library(ggplot2)
  }

##Abrindo DataSet
mydata <- read.csv("covtype.csv", header= TRUE)

# Categorização com intervalos equidistantes
mydata$Elevation <- discretize(mydata$Elevation,categories = 15)
mydata$Aspect <- discretize(mydata$Aspect,categories = 15)
mydata$Slope <- discretize(mydata$Slope,categories = 15)
mydata$Horizontal_Distance_To_Hydrology <- discretize(mydata$Horizontal_Distance_To_Hydrology,categories = 15)
mydata$Vertical_Distance_To_Hydrology <- discretize(mydata$Vertical_Distance_To_Hydrology,categories = 15)
mydata$Horizontal_Distance_To_Roadways <- discretize(mydata$Horizontal_Distance_To_Roadways,categories = 15)
mydata$Hillshade_Noon <- discretize(mydata$Hillshade_Noon,categories = 15)
mydata$Horizontal_Distance_To_Fire_Points <- discretize(mydata$Horizontal_Distance_To_Fire_Points,categories = 15)

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

#Escolha das colunas de interesse (descartando hillshade 9am e 3pm)
mydata <- mydata[,c(1:6,8,10:13)]

##Salvando Novo Dataset
write.csv(mydata, file = "best.csv",row.names=TRUE)

#Separando as parcelas para teste (20%) e treinamento (80%)
#Dividindo dataset para treinamento e teste
n <- 581012  #total de linhas no dataset
m <- trunc(n*0.2) #20%
a <- sample(1:n,m)  #escolhendo aleatoriamente 20%
test <- mydata[a,] #parte para teste
nb <- mydata[-a,] # parte para treinamento

#Removendo as colunas índice e rótulo
rotulo<-nb$Cover_Type
nb<- nb[,c(1:8,10,11)]

#transformando rótulo em "character"
rotulo<-as.character(rotulo)
is.character(rotulo)

#Naive Bayes
library(naivebayes)
nbmodel<-naive_bayes(nb, rotulo, prior = NULL)
plot(nbmodel)

#Removendo rótulo da camada test
test2<-test[,c(1:8,10,11)]
gabarito<-test$Cover_Type

#Classificando a camada test
pred <- predict(nbmodel, test2, c("class","prob"))

#Verificando o resultado  - matriz de confusão
matriz.confusao<-table(gabarito,pred)
perc.acerto <- 100*(matriz.confusao[1,1]+matriz.confusao[2,2]+matriz.confusao[3,3]+matriz.confusao[4,4]+matriz.confusao[5,5]+matriz.confusao[6,6]+ matriz.confusao[7,7])/116202
perc.erro <- 100-perc.acerto
matriz.confusao
perc.acerto
perc.erro
