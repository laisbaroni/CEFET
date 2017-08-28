##Abrindo DataSet
mydata <- read.csv("covtype.csv", header= TRUE)

# Categorização com intervalos equidistantes
library(arules)
mydata$Elevation <- discretize(mydata$Elevation,categories = 15)
mydata$Aspect <- discretize(mydata$Aspect,categories = 15)
mydata$Slope <- discretize(mydata$Slope,categories = 15)
mydata$Horizontal_Distance_To_Hydrology <- discretize(mydata$Horizontal_Distance_To_Hydrology,categories = 15)
mydata$Vertical_Distance_To_Hydrology <- discretize(mydata$Vertical_Distance_To_Hydrology,categories = 15)
mydata$Horizontal_Distance_To_Roadways <- discretize(mydata$Horizontal_Distance_To_Roadways,categories = 15)
mydata$Hillshade_Noon <- discretize(mydata$Hillshade_Noon,categories = 15)
mydata$Horizontal_Distance_To_Fire_Points <- discretize(mydata$Horizontal_Distance_To_Fire_Points,categories = 15)

#fazendo nova coluna com as wilderness areas juntas
library(tidyr)
library(plyr)
library(dplyr)
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

#Separando as parcelas para teste (20%) e treinamento (80%)
#Dividindo dataset para treinamento e teste
n <- 581012  #total de linhas no dataset
m <- trunc(n*0.2) #20%
a <- sample(1:n,m)  #escolhendo aleatoriamente 20%
test <- mydata[a,] #parte para teste
nb <- mydata[-a,] # parte para treinamento Naive Bayes
rf <- mydata[-a,] # parte para treinamento Random Forest

#Separando gabarito da camada test
gabarito <- test$Cover_Type

##Naive Bayes
#Separando o rótulo do treinamento e transformando-o em "character"
rotulo <- nb$Cover_Type
rotulo <- as.character(rotulo)

#Criando o modelo Naive Bayes
library(naivebayes)
nbmodel <- naive_bayes(nb[,-9], rotulo, laplace=c(0.3646, 0.4875, 0.06160, 0.00477, 0.0164, 0.0298, 0.03530))

#Classificando a camada teste
nb_pred <- predict(nbmodel, test[,-9])

#Verificando o resultado  - matriz de confusão
nb_matriz.confusao <- table(gabarito, nb_pred)
nb_perc.acerto <- 100*(nb_matriz.confusao[1,1]+nb_matriz.confusao[2,2]+nb_matriz.confusao[3,3]+nb_matriz.confusao[4,4]+nb_matriz.confusao[5,5]+nb_matriz.confusao[6,6]+ nb_matriz.confusao[7,7])/116202
nb_perc.erro <- 100-nb_perc.acerto
nb_matriz.confusao
nb_perc.acerto
nb_perc.erro

#Random Forest
#transformando as variáveis para "factor"
rf$wilderness_area<-as.factor(rf$wilderness_area)
rf$Cover_Type<-as.factor(rf$Cover_Type)
rf$Soil<-as.factor(rf$Soil)
test$wilderness_area<-as.factor(test$wilderness_area)
test$Cover_Type<-as.factor(test$Cover_Type)
test$Soil<-as.factor(test$Soil)
sapply(rf, class)
sapply(test, class)

#Igualando o "level" de teste com o treino (para rodar o predict)
levels(test$Cover_Type) <- levels(rf$Cover_Type)
levels(test$Elevation) <- levels(rf$Elevation)
levels(test$Aspect) <- levels(rf$Aspect)
levels(test$Slope) <- levels(rf$Slope)
levels(test$Horizontal_Distance_To_Hydrology) <- levels(rf$Horizontal_Distance_To_Hydrology)
levels(test$Horizontal_Distance_To_Roadways) <- levels(rf$Horizontal_Distance_To_Roadways)
levels(test$Horizontal_Distance_To_Fire_Points) <- levels(rf$Horizontal_Distance_To_Fire_Points)
levels(test$Vertical_Distance_To_Hydrology) <- levels(rf$Vertical_Distance_To_Hydrology)
levels(test$Soil) <- levels(rf$Soil)
levels(test$wilderness_area) <- levels(rf$wilderness_area)
levels(test$Hillshade_Noon) <- levels(rf$Hillshade_Noon)

#Criando o modelo Random Forest
library(randomForest)
rfmodel <- randomForest(as.factor(Cover_Type) ~., data=rf, importance=TRUE, ntree=100)
varImpPlot(rfmodel)
is.object(rfmodel)

#Classificando a camada teste
rf_pred <- predict(rfmodel, test)

#Verificando o resultado  - matriz de confusão
rf_matriz.confusao <- table(gabarito, rf_pred)
rf_perc.acerto <- 100*(rf_matriz.confusao[1,1]+rf_matriz.confusao[2,2]+rf_matriz.confusao[3,3]+rf_matriz.confusao[4,4]+rf_matriz.confusao[5,5]+rf_matriz.confusao[6,6]+ rf_matriz.confusao[7,7])/116202
rf_perc.erro <- 100-rf_perc.acerto
rf_matriz.confusao
rf_perc.acerto
rf_perc.erro
