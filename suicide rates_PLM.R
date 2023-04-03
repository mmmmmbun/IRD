pacman::p_load(plyr,usmap,pwt,xtable,devtools,urbnmapr,ggplot2,maps,data.table,ggsn,ggrepel,viridis,RColorBrewer,moments,VIF,apaTables,WordR,plm,texreg,readxl,stargazer,tidyverse,ggthemes,AER,lmtest)
#=====================================================================================================================================
getwd()
rm(list=ls())
#=====================================================================================================================================
data1 <- read.csv("DANE2.csv",sep=";"
                       ,fileEncoding="UTF-8-BOM",dec=",")
data1[,7:19] <- sapply(data1[,7:19],as.numeric)

#=====================================================================================================================================
#FIXED EFFECTS (BINARY DLA KA?DEGO ZE STAN?W) - WSZYSCY

samob <- lm(log(SUI)~WAG+BEZ+BIR+GES+ROZ+ALK+FER, data=data1)
samob

samob <- lm(log(MAL)~WAG+BEZ+BIR+GES+ROZ+ALK+FER, data=data1)


summary(samob)
#KOBIETY
samobF <- lm(log(FEM)~WAG+UKP+BEZ+PLO+GES+ROZ+ALK+KRA-1, data=data1)

#MʯCZY?NI
samobM <- lm(log(MAL)~WAG+UKP+BEZ+PLO+GES+ROZ+ALK+KRA-1, data=data1)

stargazer(samob,samobF, samobM, type="text")



#=====================================================================================================================================
#PLM PACKAGE
sima <- plm(MODI,data=USA1,model="pooling")
summary(sima)
table(index(USA1),useNA="ifany")
"WITH PLM" 
MODI <- log(SUI)~WAG+BEZ+BIR+GES+MAR+ALK+ROZ

ols <- lm(log(SUI)~WAG+BEZ+BIR+GES+MAR+ALK+ROZ,data=data1)
FEFE2  <- plm(MODI,
    data = data1,
    index = c("STAN", "ROK"), 
    model = "random")
FEFE33  <- plm(MODI,
              data = data1,
              index = c("STAN", "ROK"), 
              model = "within")

phtest(MODEL=c(FEFE2,FEFE33)
summary(FEFE)

phtest(MODI,data=data1,method="aux",vcov=vcovHC)
pFtest(fixed,ols)

plmtest(FEFE33)

fixef(FEFE33)
pFtest(FEFE33,ols)
pcdtest(FEFE33, test = c("lm"))

stargazer(FEFE2, FEFE33, type="text")
#=====================================================================================================================================
#lm
siema1 <- lm(SUI~WAG+BEZ+BIR+GES+ROZ+ALK+FEM+MAL,data=data1)
vif(siema1)
summary(siema1)
summary(data1)
sd(data1$BIR)
#TIME MODEL
M1 <- plm(log(SUI)~WAG+BEZ+FER+GES+ROZ+ALK,data=data1,
            index = c("STAN", "ROK"), 
            model = "within", 
            effect="time")
M2 <- plm(log(MAL)~WAG+BEZ+FER+GES+ROZ+ALK,data=data1,
                 index = c("STAN", "ROK"), 
                 model = "within", 
                 effect="time")
M3 <- plm(log(FEM)~WAG+BEZ+FER+GES+ROZ+ALK,data=data1,
                 index = c("STAN", "ROK"), 
                 model = "within", 
                 effect="time")

stargazer(M1,M2,M3, type="text")

#INDIVIDUAL
M4 <- plm(log(SUI)~WAG+BEZ+FER+GES+ROZ+ALK+GIN,data=data1,
          index = c("STAN", "ROK"), 
          model = "within", 
          effect="individual")
M5 <- plm(log(MAL)~WAG+BEZ+FER+GES+ROZ+ALK+GIN,data=data1,
          index = c("STAN", "ROK"), 
          model = "within", 
          effect="individual")
M6 <- plm(log(FEM)~WAG+BEZ+FER+GES+ROZ+ALK+GIN,data=data1,
          index = c("STAN", "ROK"), 
          model = "within", 
          effect="individual")

stargazer(M4,M5,M6, type="text")

data1$WAG2 <- data1$WAG/10000
#TWOWAYS
M7 <- plm(log(SUI)~BEZ+GES+ALK+ROZ+WAG+FER+GIN,data=data1,
                index = c("STAN", "ROK"), 
                model = "within", 
                effect="twoways")
M8 <- plm(log(MAL)~BEZ+GES+ALK+ROZ+WAG+FER+GIN,data=data1,
          index = c("STAN", "ROK"), 
          model = "within", 
          effect="twoways")
M9 <- plm(log(FEM)~BEZ+GES+ALK+ROZ+WAG+FER+GIN,data=data1,
          index = c("STAN", "ROK"), 
          model = "within", 
          effect="twoways")

stargazer(M7,M8,M9, type="text")
library(lmtest)
coeftest(M9, vcov=vcovHC(M9,type="HC0", cluster="group"))
> coeftest(M7, vcov=vcovHC(M7, cluster="group"))

install.packages("multiwayvcov")
library(multiwayvcov)
vcov_both <- cluster.vcov(M9, cbind(data1$STAN, data1$ROK))
coeftest(m1, vcov_both)

summary(data1)
sd(data1$FER)
M7 <- plm(log(SUI)~BEZ+GES+ALK+ROZ+WAG2^2+BIR+FER,data=data1,
          index = c("STAN", "ROK"), 
          model = "random", 
          effect="twoways")
M8 <- plm(log(MAL)~BEZ+GES+ALK+ROZ+WAG2^2+BIR+FER,data=data1,
          index = c("STAN", "ROK"), 
          model = "random", 
          effect="twoways")
M9 <- plm(log(FEM)~BEZ+GES+ALK+ROZ+WAG2^2+BIR+FER,data=data1,
          index = c("STAN", "ROK"), 
          model = "random", 
          effect="twoways")

stargazer(M7,M8,M9, type="text")
#=====================================================================================================================================
#=====================================================================================================================================
#=====================================================================================================================================
#=====================================================================================================================================
#=====================================================================================================================================
#=====================================================================================================================================

#HETEROSKEDASTICITY STANDARD ERRORS
coeftest(FEFE2_PLM, vcov = vcovHC, type = "HC1")[1, ]
coeftest(samob, vcov = vcovHC, type = "HC1")[1,]
#=====================================================================================================================================
#KORELACJA ZMIENNYCH
numeryczny <- data1[,c(10,11,12,13,14,15,17)]
cor(numeryczny)
numeryczny2 <- cor(USA1[,c(11,12,13,14,16,17,18)],use = "complete.obs")
numeryczny2
apa.cor.table(numeryczny,show.conf.interval=F, filename="Table2_APA.doc", table.number=1)
#=====================================================================================================================================
#?REDNIA
USA_M <- data.frame(USA1$STAN,USA1$SUI,USA1$FEM,USA1$MAL)
colnames(USA_M) <- c("STAN","SUI","FEM","MAL")

pwt.ma <- ddply(USA_M, .(STAN), numcolwise(mean))
colnames(pwt.ma) <- c("Stan","Ca?kowity","Kobiety","M??czy?ni")
pwt.ma
SREDNIA[order(SREDNIA$Ca?kowity),]

print(xtable(pwt.ma[order(pwt.ma$Ca?kowity,decreasing=F),]))DDS
#=====================================================================================================================================
#US MAPA - WSPӣCZYNNIK SAMOB?JSTW
USM <- subset(USA1, ROK>=2018 )


USS3 <- data.frame(USM$KOD,USM$SUI)
colnames(USS3) <- c("state","X")
USS3$brk <- cut(USS3$X,
                breaks = c(0,10,13,16,19,22,25,50),
                labels=c("<10","10 - 13", "13 - 16","16 - 19","19 - 22", "22 - 25", ">25"))

USS4 <- data.frame(USS3$state,USS3$brk)
colnames(USS4) <- c("state","X")

plot_usmap(regions="states",data=USS4,values="X",labels=F) +
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position = "right",plot.title = element_text(hjust = 1)) +
  labs(fill="Liczba samob?jstw \n na 100 000 mieszka?c?w")+
  guides(fill = guide_legend(reverse = TRUE))

#USMAPA - MEDIANA ZAROBK?W
USZ3 <- data.frame(USM$KOD, USM$ZAR)
colnames(USZ3) <- c("state","X")

USZ3$brk <- cut(USZ3$X, 
                breaks = c(0,50000,55000,60000,65000,70000,75000,100000),
                labels=c("<50000", "50001 - 55000","55001 - 60000" ,"60001 - 65000", "65001 - 70000", "70001 - 75000", ">75000"))
USZ4 <- data.frame(USZ3$state,USZ3$brk)
colnames(USZ4) <- c("state","X")
plot_usmap(regions="states",data=USZ4,values="X",labels=F) +
  scale_fill_brewer(palette = "PuBu") +
  theme(legend.position = "right",plot.title = element_text(hjust =1))+
  labs(fill="Mediana dochod?w\ngospodarstwa domowego\n(w dolarach) ") +
  guides(fill = guide_legend(reverse = TRUE))

#MAPA USA - UB?STWO 2018
USM2 <- data.frame(USM$KOD,USM$POV)
colnames(USM2) <-  c("state","X")

USM2$brks <- cut(USM2$X, 
                 breaks = c(0, 8, 10, 12, 14,16,18,25),
                 labels = c("<8%", "8% - 10%", "10% - 12%", "12% - 14%","14% - 16%","16% - 18%" ,">18%"),
                 include.lowest = TRUE)

USM3 <- data.frame(USM2$state,USM2$brks)
colnames(USM3) <- c("state","X")

plot_usmap(regions="states",data=USM3,values="X",labels=F) +
  scale_fill_brewer(palette = "YlOrRd")+ 
  theme(legend.position = "right",plot.title = element_text(hjust =1))+
  labs(fill="Stopa ub?stwa")+
  guides(fill = guide_legend(reverse = TRUE))

