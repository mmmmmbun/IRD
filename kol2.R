pacman::p_load(dplyr,datasets,ggplot2,caret,rpart,mlbench,randomForest,ROCR,prediction)

#ZADANIE1======================================================================
suma_kumul <- function(wektor){
  vektor <- rep(NA, length(wektor))
  for (i in 1:length(wektor)){
    vektor[1]=wektor[1]
   vektor[i]=sum(vektor[i-1]+wektor[i])
  } 
  return(vektor)
}
suma_kumul(c(7,10,20))


set.seed(76424)
wektor <- c(rnorm(10,0,4))
wektor
suma_kumul(wektor)
#ZADANIE2======================================================================
library(ISLR)
Auto <- Auto
help(Auto)
Auto %>%
  group_by(year) %>%
  summarise(¦rednia_moc_silnika=mean(horsepower),
            Maksimum_przyspieszenie=max(acceleration),
            Minimum_waga= min(weight))
            

#¦rednia moc silnika dla 78' -> 99.7 koni mechanicznych

ggplot(Auto,
       aes(x=weight,y=mpg,colour =acceleration))+
  geom_point()+
  labs(title="Wykres zu¿ycia paliwa w zale¿no¶ci od wagi samochodu",
       x="Waga",
       y="Zuzycie paliwa (mpg)")+
  geom_smooth(method="lm")+
  labs(colour="Przyspieszenie")

#ZADANIE3======================================================================
library("mlbench")
data(BreastCancer)
help(BreastCancer)
si1 <- na.omit(BreastCancer)

set.seed(76424)

train_proportion <- 0.8
train_index <- runif(nrow(si1)) < train_proportion
train <- si1[train_index,]
test <- si1

dtree1 <- rpart(Class ~Cell.shape+Mitoses, data = train,  method = "class")
dtree2 <- rpart(Class ~., data = subset(train, select=c(-Id)),  method = "class")
               
CM <- list()
CM[["drzewo1"]] <- table(predict(dtree1, new = test, type = "class"), test$Class)
CM[["drzewo2"]]<- table(predict(dtree2, new = test, type = "class"), test$Class)
CM

modele <- c("DRZ1" = dtree1, "DRZ2"=dtree2)

Ewal <- function(macierz){
  FNR <- macierz[1,2]/sum(macierz[1,1]+macierz[1,2])
  ACC <- sum(diag(macierz))/sum(macierz)
  RCL <- macierz[1,1]/(macierz[1,1]+macierz[2,2])
  return(list("False Negative Rate"=FNR,
              "Accuracy" = ACC,
              "Recall"=RCL))
}

drzewo1 <- Ewal(CM[["drzewo1"]])
drzewo2 <- Ewal(CM[["drzewo2"]])
cbind(drzewo1,drzewo2)

#Drzewo 1 jest lepsze bo mniejsze FNR

#ZADANIE4======================================================================
library(mlbench)
data("Vehicle")

levels(Vehicle$Class)[4] <- "big"
levels(Vehicle$Class)[3] <- "small"
levels(Vehicle$Class)[1] <- "big"
levels(Vehicle$Class)[2] <- "small"


set.seed(76424)



train_proportion <- 0.60
train_index <- runif(nrow(Vehicle)) < train_proportion
train <- Vehicle[train_index,]
test <- Vehicle

dtree <- rpart(Class ~., data = train,  method = "class",minbucket=50)

rpart.plot(dtree, under=FALSE, fallen.leaves = FALSE, cex = 0.7)


#REGU£A DECYZYJNA
#Jest 67% prawdopodobieñstwa na to, ¿e samochod bedzie maly jezeli jego sc var maxis jest wieksza od 186,
#max l ra jest mniejsze od 8 i elong jest wieksze od 37


CM2 <- list()
CM2[["Macierz pomy³ek"]] <- table(predict(dtree, new = test, type = "class"), test$Class)
CM2
prognoza_ciagla <- predict(dtree, newdata = test)
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])

library(ROCR)
plot(performance(ROCR::prediction(prognoza_ciagla,test$Class),"tpr","fpr"),lwd=2, colorize=T) 

# AUC (Area Under Curve) - pole pod krzywa ROC
performance(ROCR::prediction(prognoza_ciagla, test$Class),"auc")
performance(ROCR::prediction(prognoza_ciagla, test$Class),"auc")
# Lift chart
plot(performance(ROCR::prediction(prognoza_ciagla,test$Class),"lift","rpp"),lwd=2, col = "darkblue") 

