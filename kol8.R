#Zadanie 2

data <- read.csv("ships.csv")
head(data)

data <- select(data, -(1))
head(data)

data$ship <- factor(data$ship, labels = c("pierwszy","drugi","trzeci","czwarty","piaty"))
head(data)

p <- rep(c(1:40),length.out = nrow(data))
p

data <- mutate(data, p)
head(data)

f = function(file){
  data <- read.csv(file)
  p <- c(1:nrow(data))
  data <- mutate(data, p)
  return(
    print(select(data, ship, p))
  )
}

f("ships.csv")

#Zadanie 3

data <- txhousing
head(data)

data <- na.omit(data)
head(data)

test <- sample_frac(data, 0.4)
head(test)

test$year <- factor(test$year)
test$month <- factor(test$month)
head(test)

test <- arrange(test, year, month)
head(test)

sumtest <- test %>%
  group_by(year, month) %>%
  select(salse, volume) %>%
  summarize(
    avs = mean(sales),
    avv = mean(volume)
  )
sumtest

salesyear <- test %>%
  group_by(year) %>%
  summarize(
    avs = mean(sales)
  )
salesyear

pdf("wykres.pdf", width = 10, height = 8)
print(
  ggplot(salesyear) + 
    geom_col(
      aes( x = year, y = avs)
    ) +
    ggtitle("Sprzedaz w latach")
)
dev.off()

volumeyear <- test %>%
  group_by(year, month) %>%
  summarize(
    avv = mean(volume)
  )
volumeyear

ggplot(volumeyear) +
  geom_point(
    aes( x = year, y = avv, col = month)
  ) +
  theme(axis.text.x = element_text(angle = 90))

#Zadanie 4

set.seed(1)

data("GermanCredit")
head(GermanCredit)
train_frac <- 0.2

training.set.index <- (runif(nrow(GermanCredit)) < train_frac)
train.set <- GermanCredit[training.set.index, ]
test.set <- GermanCredit[!training.set.index, ]

ctree.model <- ctree(factor(Class) ~ ., data = train.set,
                     controls = ctree_control(mincriterion = 0.99,
                                              minsplit = 20))
plot(ctree.model, tnex = 2, type = "extended")

rpart.model <- rpart(Class ~ ., train.set, cp = 0.00001, minsplit = 2)
plotcp(rpart.model)

minimum.error <- which.min(rpart.model$cptable[, "xerror"])
optimal.complexity <- rpart.model$cptable[minimum.error, "CP"]
points(minimum.error, rpart.model$cptable[minimum.error, "xerror"],
       col = "red", pch = 19)
pruned.tree <- prune(rpart.model, cp = optimal.complexity)
plot(pruned.tree, compress = T, uniform = T, margin = 0.1,
     branch = 0.3, nspace = 2)
text(pruned.tree, use.n = TRUE, pretty = 0)

confusion.matrix <- list()
print(confusion.matrix[[1]] <- table(predict(ctree.model, new = test.set),
                                     test.set$Class))
print(confusion.matrix[[2]] <- table(predict(rpart.model, type = "class",
                                             newdata = test.set),
                                     test.set$Class))
print(confusion.matrix[[3]] <- table(predict(pruned.tree, type = "class",
                                             newdata = test.set),
                                     test.set$Class))

CalculateAccuracy <- function(confusion.matrix) {
  return(sum(diag(confusion.matrix)) / sum(confusion.matrix))
}

print(data.frame(model = c("ctree", "rpart","rpart przyciety"),
                 dokladnosc = sapply(confusion.matrix, CalculateAccuracy)),
      row.names = FALSE)

#Zadanie 5

data <- read_csv2("bank.csv")
head(data)

data <- mutate(data,
  job = factor(job),
  marital = factor(marital),
  education = factor(education),
  default = factor(default),
  housing = factor(housing),
  loan = factor(loan),
  contact = factor(contact),
  month = factor(month),
  poutcome = factor(poutcome),
  y = factor(y)
)
head(data)

test <- runif(nrow(data), 0,1)

rf <- randomForest(y ~. , data = data[test <= 0.8,],
                   mtre = 500,
                   mtry = 4)
varImpPlot(rf)

forecast <- predict(rf, newdata = data[test > 0.8, ], type = "prob")[,2]

plottingData <- prediction(forecast,data[test > 0.8, ]$y)

# krzywa ROC - potrzebuje "ciągłej" prognozy
plot(performance(plottingData,"tpr","fpr"),lwd=2, colorize=T) 

#AUC (Area Under Curve) - pole pod krzywą ROC
performance(plottingData,"auc")

# Sensitivity/specificity plots ~ trade-off
plot(performance(plottingData ,"sens","spec"),lwd=2) 

# Lift chart
plot(performance(plottingData ,"lift","rpp"),lwd=2, col = "darkblue") 






