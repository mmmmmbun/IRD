#=========================================================================ZADANIE1
LiczSumyKolumn <- function(macierz){
  #col_sums <- c()
  col_sum <- rep(NA, ncol(macierz))
  #miejsce gdzie zapisujemy wyniki
  for(i in 1:ncol(macierz)){
    col_sum[i] <- sum(macierz[,i])
  }
   cat("Suma w kolumnach: ",col_sum, "\n")
  return(col_sum)
}

set.seed(76424)
macierz <- matrix(round(rnorm(25),3),5,5)
LiczSumyKolumn(macierz)

wektor_sum_kolmn <- LiczSumyKolumn(macierz)

wektor_sum_kolmn[wektor_sum_kolmn <0]
#===================================================================ZADANIE2
setwd("C:/Users/Maciej/OneDrive - Szko³a G³ówna Handlowa w Warszawie/SGH/VI SEMESTR/IRD/ÆWICZENIA/exam")
siema <- read.csv(file="FITNESS.csv")

cat("Liczba obserwacji: ",nrow(siema))
cat("Liczba zmiennych: ",ncol(siema))

siema %>% 
 summarise(mediana=median(Age),
           dominanta=names(sort(-table(siema$Age)))[1],
           minimum=min(Age),
           maksimum=max(Age))
           
siema$bin <- as.factor(ifelse(siema$RunTime>=10.58, 1, 0))
siema$Diff <- siema$MaxPulse-siema$RunPulse

#==============================================================================ZADANIE3
library("ISLR")
library("dplyr")
data("Hitters")

table(is.na(Hitters))
cat("Liczba braków danych:",table(is.na(Hitters))[2])
cat("59 obserwacji pustych")
sum(apply(Hitters,1,anyNA))
czyszcz <- na.omit(Hitters)


czyszcz %>%
  select(Hits,Years,Salary,League,Division) %>%
  group_by(League, Division) %>%
  summarise_all(mean)

library(ggplot2)
ggplot(czyszcz, aes(x=Hits, y=Salary, col=League)) +
  geom_point()+
  geom_smooth()+
  ggtitle("Salary vs hits chart")


jpg("salaryvshits.jpg", width = 10, height = 8)

#=====================================================================ZADANIE4
 data("College")
set.seed(76424)
na.omit(College)
summary(College)

train_proportion <- 0.6
train_index <- runif(nrow(College)) < train_proportion
train <- College[train_index,]
test <- College[!train_index,]


drzewo <- rpart(Private~., data = train, method = "class")
las <- randomForest(Private ~., data = train)


rpart.plot(drzewo, under=FALSE, fallen.leaves = FALSE, cex = 0.7)

#przyk³adowa regu³a: 
#Jezeli idziesz do kolledzu poza swoj stan to jest 96% na to ze to bedzie koledz prywatny


CM <- list()
CM[["drzewo"]] <- table(predict(drzewo, new = test, type = "class"), test$Private)
CM[["las"]] <- table(predict(las, new = test, type = "class"), test$Private)

evaluate_model <- function(macierz){
  #accuracy <- macierz[1,1] + macierz[2,2]
  accuracy <- sum(diag(macierz))/sum(macierz)
  MER <- 1 - accuracy
  return(list(Accuracy=accuracy,
              MER=MER))}

evaluate_model(CM[["drzewo"]])
evaluate_model(CM[["las"]])

