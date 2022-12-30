#=====================================================
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




#======================================================





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



#======================================================



si <- function(n){
  if (n<4 || is.finite(n)==F||is.na(n)==T){
    return(x=NULL)}
  else if (n>=4){
    a <- lower.tri(matrix(1,ncol=n,nrow=n))
    b <- matrix(0,ncol=n,nrow=n)
    c <- matrix(ncol=n,nrow=n)
    for (i in 1:ncol(c)){
      for (j in 1:nrow(c))
        c[i,j] <- i*j}
    diag(c) <- 0   
    c[lower.tri(c)] <- 0
    x <- a+b+c}
  return(x)}


#======================================================




aa <- function(x) {
  if(is.numeric(x) == FALSE) {print("x nie jest wartoœci¹ numeryczn¹")}
  else if(x>=10){return(x^(1/3))}
  else if(x< 10 && x> -10) {return(1/2 * abs(x))}
  else if(x<= -10) {return(log10(abs(x)))} 
}



#======================================================



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



#======================================================


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




#======================================================

mnozenie <- function(n, m){
  if (dim(n)[1]==dim(m)[2]){
    macierz <- matrix(0 , dim(n)[1] , dim(m)[2])    
    for (i in 1:dim(n)[1]) {
      for (j in 1:dim(m)[2]) {
        for (k in 1:dim(n)[2] ) {
          macierz[i,j] = macierz[i,j] + n[i,k]*m[k,j]
        }
      }
    }}
  else{
    macierz <- 76424}
  return (macierz)
}



mnozenie(mat1,mat2)













