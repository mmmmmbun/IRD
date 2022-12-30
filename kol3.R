pacman::p_load(dplyr,datasets,ggplot2,caret,rpart,mlbench,ROCR,randomforest)

#ZADANIE1======================================================================
siema <- function(x){
   if (x>=10){
     return(x^(1/3))
   }
   else if (x>-10 && x<10){
     return(1/2*abs(x))
   }
   else if (x<=-10){
     return(log(abs(x),base=10))
   }
   return(x)
 }

x  <- seq(-15,15)
y <- sapply(x,siema)
plot(x=x,y=y,type="l")
#ZADANIE2======================================================================
library(datasets)
data(airquality)

airquality %>%
  group_by(Month) %>%
  summarise(
    Liczebno¶æ=n(),
    Minimum=min(Wind),
    Maksimum=max(Wind)
  )

wiatr <- airquality %>%
  group_by(Month) %>%
  summarise(
    Liczebno¶æ=n(),
    Maksimum=max(Wind))

ggplot(wiatr,
       aes(x=Month,y=Maksimum))+
  geom_line()+
  labs(title="Maksymalna prêdko¶æ wiatru w zale¿no¶ci od miesi±ca")+
       xlab("Miesi±c")+
       ylab("Maksymalna prêdko¶æ wiatru")

#ZADANIE3======================================================================
dane <- read.csv(file="HR_comma_sep.csv")

dane$left <- as.factor(dane$left)

set.seed(76424)


train_proportion <- 0.75
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane

dtree1 <- rpart(left~satisfaction_level+salary, data = train,  method = "class")
dtree2 <- rpart(left~., data = train,  method = "class")


CM <- list()
CM[["drzewo1"]] <- table(predict(dtree1, new = test, type = "class"), test$left)
CM[["drzewo2"]]<- table(predict(dtree2, new = test, type = "class"), test$left)
CM

Ewal <- function(macierz){
  ACC <- sum(diag(macierz))/sum(macierz)
  return(list("Accuracy" = ACC))}
              

cbind("drzewo2"=Ewal(CM[["drzewo2"]]),"drzewo1"=Ewal(CM[["drzewo1"]]))

#Drzewo 2 jest lepsze, wyzsza accuracy czyli lepiej klasyfikuje obserwacje do wlasciwych klas
#ZADANIE4======================================================================
dane <- read.csv(file="HR_comma_sep.csv")

library(dplyr)
dane %>% summarise(
  ¦rednia_godzin_pracy = average_montly_hours/21,
  Placa=salary,
  sprzedaz=sales) %>%
  group_by(Placa,sprzedaz) %>%  arrange(¦rednia_godzin_pracy)

  

#ZADANIE5======================================================================
dane <- read.csv(file="HR_comma_sep.csv")
dane$left <- as.factor(dane$left)


set.seed(76424)

train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane

rpart.plot(drzewo, under=FALSE, fallen.leaves = FALSE, cex = 0.7)
rf <- randomForest(left ~., data = train)

#REGU£A DECYZYJNA
library(randomForest)
macierz_bledu <- table(predict(rf, new = test, type = "class"), test$left)
macierz_bledu

#LIFT
forecast <- predict(rf, newdata = test, type = "prob")[,2]
plottingData <- prediction(forecast, test$left)

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(plottingData,"tpr","fpr"),lwd=2, colorize=T) 

#AUC (Area Under Curve) 
performance(plottingData,"auc")@y.values[[1]]

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 

