
# wczytanie uzywanych bibliotek
library(dplyr)
library(readr)
library(rjson)


# wczytanie danych
test_data1 <- read_csv("dota2Test.csv", col_names = FALSE)
train_data1 <- read_csv("dota2Train.csv", col_names = FALSE)
heroes_raw <- fromJSON(file = "hero_coding.json")

#dodawanie nazw kolumn
heroes <- tibble(name=NA, id=NA, localized_name=NA)
for (i in 1:length(heroes_raw[[1]])){
  heroes <- union(heroes, as.data.frame(heroes_raw[[1]][i]))
}
hero_names <- heroes$name[-1]
id <- heroes$id[-1]

for (i in 1:length(id)){
  colnames(test_data1)[4+id[i]] <- hero_names[i]
  colnames(train_data1)[4+id[i]] <- hero_names[i]
}


# transformacja danych
test_data2 <- test_data1 %>%
  mutate(across(-c("X2", "X3", "X4"), function(x) -x))

train_data2 <- train_data1 %>%
  mutate(across(-c("X2", "X3", "X4"), function(x) -x))


test_data <- test_data1 %>%
  union(test_data2) %>%
  rename(
    win = X1,
    cluster_id = X2,
    game_mode = X3,
    game_type = X4) %>%
  select(!"X28") %>% 
  mutate_all(as.factor)

train_data <- train_data1 %>%
  union(train_data2) %>%
  rename(
    win = X1,
    cluster_id = X2,
    game_mode = X3,
    game_type = X4) %>%
  select(!"X28") %>% 
  mutate_all(as.factor)

install.packages("vcd")
library(vcd)
install.packages("zoo")
catcorrm <- function(train_train_dataa, train_data) sapply(train_train_dataa, function(y) sapply(train_train_dataa, function(x) assocstats(table(train_data[,x], train_data[,y]))$cramer))

