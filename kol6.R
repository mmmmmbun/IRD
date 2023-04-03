# Kolokwium IRD
################################################################################
# 09.06.2021 
pacman::p_load(FSAdata,party,dplyr,datasets,ggplot2,caret,rpart,mlbench,randomForest,ROCR,prediction,readr,data.table,tidyverse,rpart.plot)
################################################################################
# Zadanie 1
################################################################################t
transformuj_macierz <- function(macierz){
   mac1 <- macierz
   mac2 <- matrix(NA,ncol=ncol(mac1),nrow=nrow(mac1))
   for (i in 1:nrow(mac2)){
      for (j in 1:ncol(mac2)){
         if (i==j){
            if (mac1[i,j] %% 2 == 0){
            mac2[i,j] <- 0
         }
             else{
            mac2[i,j] <- 1}}
         else if (i<j){
            if (mac1[i,j]>mean(mac1[i,])){
               mac2[i,j] <- 1}
            else{
               mac2[i,j] <- -1}}
         else if (i>j){
            if (mac1[i,j]>mean(mac1[,j])){
               mac2[i,j] <- 1}
            else{
               mac2[i,j] <-0}
            } }}
            return(mac2)   
            }

transformuj_macierz(matrix(1:16,4,4))

dowolna <- matrix(sample(-500:500,25),5,5)
dowolna
transformuj_macierz(dowolna)
################################################################################
# Zadanie 2
################################################################################
library("nycflights13")
data("flights")


loty <- flights %>% group_by(origin,dest) %>%
   dplyr::summarise(Liczba_polaczen=n(),
             Unikalne_polaczenia=n_distinct(tailnum),
             ¦redni_czas_wpowietrzu=mean(air_time,na.rm=T),
             ¦redni_Dystans=mean(distance,na.rm=T))
loty
loty2 <- loty %>% mutate(Unikalne_loty=Unikalne_polaczenia/Liczba_polaczen)
loty2
                         
ggplot(subset(flights,carrier==c("B6","UA","YV")),
       aes(x=carrier,y=distance,colour=carrier))+
  geom_boxplot()+
  xlab("Przewoznik (carrier)")+
   ylab("Dystans (distance")+
   labs(color="Przewoznik")+
   scale_fill_discrete(name = "Przewoznik")

################################################################################
# Zadanie 3
################################################################################

library(mlbench)
data("PimaIndiansDiabetes")

set.seed(12345)

train_proportion <- 0.70
train_index <- runif(nrow(PimaIndiansDiabetes)) < train_proportion
train <- PimaIndiansDiabetes[train_index,]
test <- PimaIndiansDiabetes

rf <- randomForest(diabetes ~., data = train,ntree=50)
dtree <- rpart(diabetes ~., data = train,  method = "class",maxdepth=5)

dtree$variable.importance
#Glukoza ma najwiêkszy wp³Yw na zmienn± prognozowan± w drzewie decyzyjnym


CM <- list()
CM[["drzewo"]] <- table(predict(dtree, new = test, type = "class"), test$diabetes)
CM[["forest"]]<- table(predict(rf, new = test, type = "class"), test$diabetes)
CM


Ewal <- function(macierz){
   true_positive <- macierz[2,2]
   true_negative <- macierz[1,1]
   false_positive <- macierz[1,2]
   false_negative <- macierz[2,1]
   MER <- (false_positive+false_negative)/sum(macierz)
   Fallout <- false_positive/(false_positive+true_negative)
   return(list( MER = MER,
               Fallout=Fallout))}

tree <- Ewal(CM[["drzewo"]])
forest <- Ewal(CM[["forest"]])
cbind(forest,tree)

#Lepszym modelem jest forest bo mniej siê myli (mniejsze missclassifiaction) i rzadziej przyporzadkowuje klase pozytywna prawdziwej klasie negatywnej

forecast <- predict(rf, newdata = test, type = "prob")[,2]
plottingData <- ROCR::prediction(forecast, test$diabetes)


AUC <- performance(plottingData,"auc")@y.values[[1]]
cat("Warto¶æ AUC:",AUC)

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 

#Interpretacja
#Przy 20% wlasciwych predykcji nasz model jest lepszy o ok 2,8 raza niz losowy

################################################################################
# Zadanie 4
################################################################################

library(ISLR)
data("Carseats")


set.seed(12345)

train_proportion <- 0.70
train_index <- runif(nrow(Carseats)) < train_proportion
train <- Carseats[train_index,]
test <- Carseats


reglin <- lm(Sales~.,data=train)
drzreg <- rpart(Sales~.,cp=0.01,data=train)

rpart.plot(drzreg, under=FALSE, fallen.leaves = FALSE, cex = 0.7)

#Regu³a
#Dla shelving location jakosci bad/medium i ceny mniejszej niz 107 oraz wieku mniejszego 
#niz 35 sprzedano srednio 11 tys fotelikow dla dzieci



modele <- list("drzewo" = drzreg, "regresja" = reglin)

OcenaModeli <- function(modele, dane, predicted_col_name) {
   
   print("Pierwiastek b³êdu ¶redniokwadratowego RMSE")
   print(sapply(modele, function(x) sqrt(sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) ))
   
   print("Wzglêdny b³±d  absolutny RAE")
   print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/sum(abs(dane[[predicted_col_name]] - mean(dane[[predicted_col_name]]))) ))
   
}

OcenaModeli(modele, test, 'Sales')

#Regresja jest zdecydowanie lepsza, bo ma oba bledy nizsze co oznacza precyzyjniejsze oszacowania

