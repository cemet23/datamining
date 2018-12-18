# Muhammad Luthfi 15.01.53.0026
# Fery Andrika 15.01.53.0027

# Libraries
install.packages("naivebayes")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("magrittr")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(magrittr)

# Data
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data'
data <- read.csv(urlfile, header = FALSE, sep = "", quote = "\"'")
str(data)
xtabs(~V1+V8, data = data)
data$origin <- as.factor(data$V8)
data$acceleration <- as.factor(data$V6)
data$weight <- as.factor(data$V5)
data$cylinders <- as.factor(data$V2)

# Visualization
pairs.panels(data[-1])
data %>%
  ggplot(aes(x=origin, y=acceleration, fill = weight)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>% ggplot(aes(x=acceleration, fill = weight)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(origin ~ ., data = train, usekernel = T)
model

train %>%
  filter(origin == "1") %>%
  summarise(mean(V2), sd(V2))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$cylinders))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$cylinders))
1 - sum(diag(tab2)) / sum(tab2)
