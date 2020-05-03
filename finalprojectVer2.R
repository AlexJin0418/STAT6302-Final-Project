library(RSQLite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(pROC)

con <- dbConnect(RSQLite::SQLite(), "D:/SMU/STAT 6302/Final Project/database.sqlite")
dbListTables(con)

league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
country <- tbl_df(dbGetQuery(con,"SELECT * FROM Country"))
team <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
match <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))

#Country Information
str(country)
View(country)

#League Information
str(league)
View(league)

#Team Information
str(team)
View(team)

missingteam <- team[is.na(team$team_fifa_api_id),]

length(unique(team$team_fifa_api_id))
length(unique(team$team_api_id))
length(unique(team$team_long_name))
length(unique(team$team_short_name))

freq <- as.data.frame(table(team$team_fifa_api_id))
freq1 <- freq[freq$Freq >= 2,]
dup <- team[team$team_fifa_api_id %in% freq1$Var1, ]

#Germany
str(match)
matchgermany = match[match$country_id == 7809, ]

matchgermany1 = matchgermany[, c(1:11)]
matchgermany1$Country = "Germany"
matchgermany1$League = "Germany 1. Bundesliga"
matchgermany1$goal_difference = matchgermany1$home_team_goal - matchgermany1$away_team_goal
matchgermany1$total_goal = matchgermany1$home_team_goal + matchgermany1$away_team_goal

sum(matchgermany1$home_team_goal)/2448
sum(matchgermany1$away_team_goal)/2448
sum(matchgermany1$total_goal)/2448
sum(matchgermany1$goal_difference)/2448

#Analysis
#Home Team Goal
lambdahome = mean(matchgermany1$home_team_goal)

g1 = ggplot(matchgermany1, aes(x=home_team_goal)) + 
  geom_histogram(binwidth=0.1, color="red", fill="yellow") +
  theme_bw() +
  xlab("Goal") +
  ylab("Number of Match") +
  ggtitle("Home Team Goal Distribution (Bundesliga)") +
  scale_x_continuous(breaks = seq(0, 9, 1))
print(g1)

g2 = ggplot(matchgermany1, aes(x=home_team_goal)) +
  stat_function(geom = "line", n = 10, fun = dpois, args = list(lambda = lambdahome)) +
  theme_bw() +
  xlab("Goal") +
  ylab("Poisson") +
  ggtitle("Home Team Goal Distribution (Bundesliga)") +
  scale_x_continuous(breaks = seq(0, 9, 1)) +
  annotate(geom = "text", x = 7.5, y = 0.3, label = "lambda = 1.627")
print(g2)

#Away Team Goal
lambdaaway = mean(matchgermany1$away_team_goal)

g3 = ggplot(matchgermany1, aes(x=away_team_goal)) + 
  geom_histogram(binwidth=0.1, color="red", fill="yellow") +
  theme_bw() +
  xlab("Goal") +
  ylab("Number of Match") +
  ggtitle("Away Team Goal Distribution (Bundesliga)") +
  scale_x_continuous(breaks = seq(0, 8, 1))
print(g3)

g4 = ggplot(matchgermany1, aes(x=away_team_goal)) +
  stat_function(geom = "line", n = 9, fun = dpois, args = list(lambda = lambdaaway)) +
  theme_bw() +
  xlab("Goal") +
  ylab("Poisson") +
  ggtitle("Away Team Goal Distribution (Bundesliga)") +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  annotate(geom = "text", x = 6.5, y = 0.3, label = "lambda = 1.275")
print(g4)

#Total Goal
lambdatotal = mean(matchgermany1$total_goal)

g5 = ggplot(matchgermany1, aes(x=total_goal)) + 
  geom_histogram(binwidth=0.1, color="red", fill="yellow") +
  theme_bw() +
  xlab("Goal") +
  ylab("Number of Match") +
  ggtitle("Total Goal Distribution (Bundesliga)") +
  scale_x_continuous(breaks = seq(0, 11, 1))
print(g5)

g6 = ggplot(matchgermany1, aes(x=total_goal)) +
  stat_function(geom = "line", n = 12, fun = dpois, args = list(lambda = lambdatotal)) +
  theme_bw() +
  xlab("Goal") +
  ylab("Poisson") +
  ggtitle("Total Goal Distribution (Bundesliga)") +
  scale_x_continuous(breaks = seq(0, 11, 1)) +
  annotate(geom = "text", x = 9, y = 0.20, label = "lambda = 2.902")
print(g6)

#Data Pre-Processing
matchodds = matchgermany[, c(86:97,101:115)]

df = cbind(matchgermany1, matchodds)
df = df[, -c(2:3,6:7)]
df = df[, c(8:9, 1:7, 10:26)]
df1 = na.omit(df)
df1$result = ifelse(df1$goal_difference > 0, "Win",
                    ifelse(df1$goal_difference == 0, "Draw", "Lose"))
df1$result = factor(df1$result)
levels(df1$result)

df1415 <- df[df$season == "2014/2015", ]
df1415fcb <- df1415[df1415$home_team_api_id == 9823,]
df1415bvb <- df1415[df1415$away_team_api_id == 9789,]
mean(df1415fcb$home_team_goal)
mean(df1415bvb$away_team_goal)

poissonscore <- function(x, mu){mu^x/factorial(x) * exp(1)^(-mu)}

for (x in 0:5){
  print(poissonscore(x, mean(df1415fcb$home_team_goal)))
}

for (x in 0:5){
  print(poissonscore(x, mean(df1415bvb$away_team_goal)))
}

# Regression Tree

ctrl <- trainControl(method = "repeatedcv",
                     classProbs = TRUE,
                     repeats=5,
                     savePredictions = TRUE)

set.seed(717)
train = sample(1:nrow(df1), 1/2*nrow(df1))
dfrpart <- train(x = as.matrix(df1[train, 12:26]), 
                  y = df1[train, 27],
                  method = "rpart",
                  tuneLength = 30,
                  metric = "Accuracy",
                  trControl = ctrl)
saveRDS(dfrpart, 'D:/SMU/STAT 6302/Final Project/rpart1.rds')

rpartpre <- predict(dfrpart, df1[-train,])
rpartpre1 <- predict(dfrpart, df1[-train,], type = "prob")
confusionMatrix(rpartpre, df1[-train, 27])

# Random Forest

mtryValues <- c(2, 4, 6, 8, 10)

set.seed(717)
train = sample(1:nrow(df1), 1/2*nrow(df1))
dfrf <- train(x = as.matrix(df1[train, 12:26]), 
              y = df1[train, 27],
              method = "rf",
              ntree = 500,
              tuneGrid = data.frame(mtry = mtryValues),
              importance = TRUE,
              metric = "Accuracy",
              trControl = ctrl)
saveRDS(dfrf, 'D:/SMU/STAT 6302/Final Project/rf1.rds')

rfpre <- predict(dfrf, df1[-train,])
rfpre1 <- predict(dfrf, df1[-train,], type = "prob")
confusionMatrix(rfpre, df1[-train, 27])

rfroc <- multiclass.roc(df1[-train, 27], rfpre1)
drawlose <- rfroc$rocs$`Draw/Lose`
drawwin <- rfroc$rocs$`Draw/Win`
losewin <- rfroc$rocs$`Lose/Win`

rfroc$auc

plot.roc(drawlose[[2]])
plot.roc(drawwin[[2]])
plot.roc(losewin[[2]])

# XGBoosting Tree

set.seed(717)
train = sample(1:nrow(df1), 1/2*nrow(df1))
dfxgb <- train(x = as.matrix(df1[train, 12:26]), 
               y = df1[train, 27], 
               method = "xgbTree", 
               trControl = ctrl,
               importance = TRUE,
               metric = "Accuracy",
               tuneLength = 3)
saveRDS(dfxgb, 'D:/SMU/STAT 6302/Final Project/xgb1.rds')

xgbpre <- predict(dfxgb, df1[-train,])
xgbpre1 <- predict(dfxgb, df1[-train,], type = "prob")
confusionMatrix(xgbpre, df1[-train, 27])

xgbroc <- multiclass.roc(df1[-train, 27], xgbpre1)
drawlose1 <- xgbroc$rocs$`Draw/Lose`
drawwin1 <- xgbroc$rocs$`Draw/Win`
losewin1 <- xgbroc$rocs$`Lose/Win`

xgbroc$auc

plot.roc(drawlose1[[2]])
plot.roc(drawwin1[[2]])
plot.roc(losewin1[[2]])

# Bagging Tree

set.seed(717)
train = sample(1:nrow(df1), 1/2*nrow(df1))
dfbag <- train(x = as.matrix(df1[train, 12:26]), 
               y = df1[train, 27], 
               method = "treebag", 
               trControl = ctrl,
               importance = TRUE,
               metric = "Accuracy")
saveRDS(dfbag, 'D:/SMU/STAT 6302/Final Project/treebag1.rds')

bagpre <- predict(dfbag, df1[-train,])
bagpre1 <- predict(dfbag, df1[-train,], type = "prob")
confusionMatrix(bagpre, df1[-train, 27])

bagroc <- multiclass.roc(df1[-train, 27], bagpre1)
drawlose2 <- bagroc$rocs$`Draw/Lose`
drawwin2 <- bagroc$rocs$`Draw/Win`
losewin2 <- bagroc$rocs$`Lose/Win`

bagroc$auc

plot.roc(drawlose2[[2]])
plot.roc(drawwin2[[2]])
plot.roc(losewin2[[2]])

# Ladbrokes Home
lbh <- df1[, c(21,27)]
lbh1 <- subset(lbh, lbh$LBH < 1.90)
lbh2 <- subset(lbh1, lbh1$LBH >= 1.80)
table(lbh2)
