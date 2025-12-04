## 1: Very simple Bayesian Network
## https://en.wikipedia.org/wiki/Bayesian_network#/media/File:SimpleBayesNet.svg

## Construct data from this model
df <- data.frame(rain=runif(1000) < .2)
df$sprinkler <- ifelse(df$rain, runif(1000) < 0.01, runif(1000) < .4)
df$grasswet <- ifelse(df$sprinkler & df$rain, runif(1000) < .99,
               ifelse(df$sprinkler & !df$rain, runif(1000) < .9,
               ifelse(!df$sprinkler & df$rain, runif(1000) < .8, F)))
df$rain <- factor(df$rain)
df$sprinkler <- factor(df$sprinkler)
df$grasswet <- factor(df$grasswet)

install.packages("bnlearn")
library(bnlearn)

network <- hc(df)
plot(network)

library(readr)
text = read_file("~/Dropbox/Complex Systems Analysis 2025/week14/catinthehat.txt")
text2 <- gsub("[^a-z ]", "", gsub("\n", " ", tolower(text))) # "stopwords"

chars = factor(strsplit(text2, split="")[[1]])

df <- data.frame(lag1=chars[1:(length(chars)-1)], lag0=chars[2:length(chars)])
network <- hc(df)
plot(network)

df <- data.frame(lag5=chars[1:(length(chars)-5)], lag4=chars[2:(length(chars)-4)],
                 lag3=chars[3:(length(chars)-3)], lag2=chars[4:(length(chars)-2)],
                 lag1=chars[5:(length(chars)-1)], lag0=chars[6:length(chars)])
network <- hc(df)
plot(network)

param <- bn.fit(network, data=df)
samp <- rbn(param, n=20)

## Check out Natural Language Toolkit (NLTK): https://www.nltk.org/
## install.packages(c('neuralnet','keras','tensorflow', 'caret'),dependencies = T)

library(tidyverse)
library(neuralnet)

## First, translate into simple numbers
chars = utf8ToInt(text2)
df <- data.frame(lag5=chars[1:(length(chars)-5)], lag4=chars[2:(length(chars)-4)],
                 lag3=chars[3:(length(chars)-3)], lag2=chars[4:(length(chars)-2)],
                 lag1=chars[5:(length(chars)-1)], lag0=sapply(chars[6:length(chars)], intToUtf8))

train.df <- df[1:500,]
test.df <- df[501:nrow(df),]

model = neuralnet(lag0 ~ lag1 + lag2 + lag3 + lag4 + lag5,
                  data=train.df,
                  hidden=c(5,5),
                  linear.output = FALSE,
                  lifesign='full')
plot(model)

library(caret)

dummies <- dummyVars(~ lag1 + lag2 + lag3 + lag4 + lag5, data=df)
onehot.mat <- predict(dummies, df)
onehot.df <- data.frame(onehot.mat, lag0=df$lag0)

train.df <- onehot.df[1:5000,]
test.df <- onehot.df[5001:nrow(onehot.df),]

nn.formula <- as.formula(paste("lag0 ~", paste(names(onehot.df)[-ncol(onehot.df)], collapse = " + ")))
model = neuralnet(nn.formula,
                  data=train.df,
                  hidden=c(126,126),
                  linear.output = FALSE)

## Simple example

iris <- iris %>% mutate_if(is.character, as.factor)
data_rows <- floor(0.80 * nrow(iris))
train_indices <- sample(c(1:nrow(iris)), data_rows)
train_data <- iris[train_indices,]
test_data <- iris[-train_indices,]

model2 = neuralnet(
    Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
data=train_data,
hidden=c(4,2),
linear.output = FALSE
)

plot(model2,rep = "best")








