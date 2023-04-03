pacman::p_load(randomForest,dplyr,datasets,ggplot2,caret,rpart,mlbench,ROCR,rpart.plot)

#ZADANIE1======================================================================
foobar <- function(n){
  lista <- list()
  for (i in 1:n){
    if (i %% 7 == 0){
      if (i  %% 3 == 0){ 
        lista[i] <- "dzielnik"
      }
      else{
        lista[i] <- "dziel"
      }}
 else if (i %% 3 == 0){
      lista[i] <- "nik"
    }
   else {
    lista[i] <- i
  }}
return(lista)
}
foobar(100)
#ZADANIE2======================================================================
 data("chickwts")

chicks2<- chickwts %>%
  group_by(feed) %>%
   summarize(Liczebno¶æ  = n(),
          ¦rednia = mean(weight),
          Mediana = median(weight))

 
 ggplot(chicks2,
   aes(x=feed,y=Mediana))+
     geom_boxplot()+
     labs(title="Mediana wagi kurczaka w zale¿no¶ci od karmy")+
     xlab("Typ karmy")+
     ylab("Mediana wagi kurczaka")
#ZADANIE3======================================================================

library("caret")
help(data("GermanCredit"))


set.seed(76424)
train_proportion <- 0.7
train_index <- runif(nrow(GermanCredit)) < train_proportion
train <- GermanCredit[train_index,]
test <- GermanCredit

reglin <- lm(Duration~.,data=train)
drzreg <- rpart(Duration~.,cp=0.04,data=train)

rpart.plot(drzreg, under=FALSE, fallen.leaves = FALSE, cex = 0.7)

#31% osbo bioracych kredyt ktoory byl wiekszy niz 3595 bralo go na srednio 31 dni
all(as.vector(residuals(drzreg)) == drzreg$Duration - predict(drzreg, test))

modele <- list("reglin" = reglin, "drzreg" = drzreg)

OcenaModeli <- function(modele, dane, predicted_col_name) {
  
  print("¦redni b³±d absolutny MAE")
  print(sapply(modele, function(x) sum(abs((dane[[predicted_col_name]] - predict(x, dane))))/nrow(dane) ))
  
  print("B³±d ¶redniokwadratowy MSE")
  print(sapply(modele, function(x) sum((dane[[predicted_col_name]] - predict(x, dane))^2)/nrow(dane)) )}
  
OcenaModeli(modele, test, 'Duration')

#REGLIN BÊDZIE LEPSZA -> MA MNIEJSZE B£EDY OBA
#ZADANIE4======================================================================
library(mlbench)
help(data("HouseVotes84"))

sie <- table(is.na(HouseVotes84))[2]

cat("Braki danych w zbiorze:",sie)

sie2 <- na.omit(HouseVotes84)

set.seed(76424)

train_proportion <- 0.75
train_index <- runif(nrow(sie2)) < train_proportion
train <- sie2[train_index,]
test <- sie2


rf <- randomForest(Class ~., data = train)
dtree <- rpart(Class ~V1+V2+V3+V4, data = train,  method = "class")


rpart.plot(dtree, under=FALSE, fallen.leaves = FALSE, cex = 0.9)


#REGU£A DECYZYJNA
#Je¿eli zag³osowa³e¶ przeciwko przyjeciu budzetu to jestes na 47% republikaniem

CM <- list()
CM[["drzewo"]] <- table(predict(rf, new = test, type = "class"), test$Class)
CM[["forest"]]<- table(predict(dtree, new = test, type = "class"), test$Class)
CM
sprawdzanko <- function(matrix){
  list()
Accuracy <- sum(diag(matrix))/sum(matrix)
Precision <- matrix[1,1]/sum(matrix)
return(list(Accuracy,Precision))}

 sprawdzanko(CM[["drzewo"]])
 sprawdzanko(CM[["forest"]])
 #Accuracy i precision s± wiêksze dla drzewa, wiêc wybieramy je
