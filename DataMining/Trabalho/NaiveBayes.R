##Executando Naive Bayes##

##Abrindo novo dataset
mynewdata <- read.csv("mynewdata.csv", header= TRUE)

#Separando as parcelas para teste (20%) e treinamento (80%)
#Dividindo dataset para treinamento e teste
n <- 581012  #total de linhas no dataset
m <- trunc(n*0.2) #20%
a <- sample(1:n,m)  #escolhendo aleatoriamente 20%
test <- mynewdata[a,] #parte para teste
nb <- mynewdata[-a,] # parte para treinamento

#Removendo as colunas índice e rótulo
rotulo<-nb$Cover_Type
nb<- nb[,c(2:9,11,12)]

#transformando rótulo em "character"
rotulo<-as.character(rotulo)
is.character(rotulo)

#Naive Bayes
library(naivebayes)
nbmodel<-naive_bayes(nb, rotulo, prior = NULL)
plot(nbmodel)

#Removendo rótulo da camada test
test2<-test[,c(2:9,11,12)]
gabarito<-test$Cover_Type

#Classificando a camada test
pred <- predict(nbmodel, test2, c("class","prob")) #ainda não entendi pra que serve isso: c("class","prob")

#Tabela com todas as probabilidades individuais com a camada rótulo
tables(nbmodel, which = NULL)

#####Verificando o resultado  - matriz de confusão
matriz.confusao<-table(gabarito,pred)
perc.acerto <- 100*(matriz.confusao[1,1]+matriz.confusao[2,2]+matriz.confusao[3,3]+matriz.confusao[4,4]+matriz.confusao[5,5]+matriz.confusao[6,6]+ matriz.confusao[7,7])/116202
perc.erro <- 100-perc.acerto
matriz.confusao
perc.acerto
perc.erro

