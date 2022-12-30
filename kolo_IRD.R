pacman::p_load(dplyr,datasets,ggplot2,caret,rpart,mlbench,randomForest,ROCR,prediction)
"Napisz funkcje, ktora dla dowolnej numerycznej wartosci x bedzie zwracala:
  jezeli x>1: pierwiastek kwadratowy z x
jezeli x>=0 i x<=1: 0
jezeli x<0: logarytm naturalny z wartosci bezwzglednej x
Oblicz wartosc funkcji z punktu a) dla wszystkich liczb calkowitych z przedzialu <-100,100>, pokaz przebieg tej funkcji na wykresie"

#ZADANIE1======================================================================
wartosc <- function(x){
   if (x>1){
     return(sqrt(x))
   
     else (x>=0 | x<=1){
     return(0)}}
    else if (x<0){
     return(log(abs(x)))
   }
   return(x)
}

wartosc <- function(x){
    if (x>1){
          return(sqrt(x))}
        
      else if (x>=0 & x<=1){
          return(0)}
     else if (x<0){
         return(log(abs(x)))}
      
   
        return(x)
     
     }



x <- seq(-100, 100)
y <- sapply(x, wartosc)
plot(x = x, y = y, type = "p")
#ZADANIE2======================================================================
foo <- function(x){
  if (x>1){
    return(x %% 3)
  }
  else if (x>=0 & x<=1)
    {
    return(0)}
  else if (x<0){
    return(log(abs(x)))
  }
  return(x)
}
x <- seq(-100,100)
y <- sapply(x,wartosc)
plot(x=x,y=y,type="p")
