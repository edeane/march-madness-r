
# sources
# https://www.kaggle.com/c/march-machine-learning-mania-2017
# https://www.kaggle.com/ajniggles/logistic-regression-and-game-round-calculator
# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# todo
#    add functions for joining train and score data
#    move score data join steps up to train data steps
#    move cross val, train models, and scoring to bottom

rm(list=ls())

setwd("W:/March_Madness")

require(dplyr)
require(tidyr)
require(stringr)
require(glmnet)
require(xgboost)
require(caret)

#read in data
tourney.seeds <- read.csv('data/TourneySeeds.csv')
tourney.compact.results <- read.csv('data/TourneyCompactResults.csv')
regular.season.compact.results <- read.csv('data/RegularSeasonCompactResults.csv')
regular.season.detailed.results <- read.csv('data/RegularSeasonDetailedResults.csv')
sample.submission <- read.csv('data/SampleSubmission.csv')

#create train data with only data >= to 2003 because we have detailed results from 2003 to today
train <- tourney.compact.results[tourney.compact.results$Season >= 2003, c('Season', 'Wteam', 'Lteam')]
train$first.team <- pmin(train$Wteam, train$Lteam)
train$second.team <- pmax(train$Wteam, train$Lteam)
train$first.team.win <- 0
train$first.team.win[train$Wteam==train$first.team] <- 1

# create win loss data frame by season with percent win
# season.wins <- regular.season.compact.results %>% group_by(Season, Wteam) %>% tally()
# season.losses <- regular.season.compact.results %>% group_by(Season, Lteam) %>% tally()
# season.wins.losses <- full_join(season.wins, season.losses, by=c('Season', 'Wteam' = 'Lteam'))
# colnames(season.wins.losses)[2:4] <- c('team', 'wins', 'loses')
# season.wins.losses[is.na(season.wins.losses)] <- 0
# season.wins.losses$percent.win <- season.wins.losses$wins / (season.wins.losses$wins+season.wins.losses$loses)
# head(season.wins.losses)

# create win average margin and loss average margin
# regular.season.compact.results$win.margin <- regular.season.compact.results$Wscore - regular.season.compact.results$Lscore
# mean.win.margin <- regular.season.compact.results %>% group_by(Season, Wteam) %>% summarize(mean.win.margin = mean(win.margin, na.rm=T))
# mean.loss.margin <- regular.season.compact.results %>% group_by(Season, Lteam) %>% summarize(mean.loss.margin = mean(win.margin, na.rm=T))
# mean.win.loss.margin <- full_join(mean.win.margin, mean.loss.margin, by=c('Season', 'Wteam' = 'Lteam'))
# mean.win.loss.margin[is.na(mean.win.loss.margin)] <- 0
# colnames(mean.win.loss.margin)[2:4] <- c('team', 'mean.win.margin', 'mean.loss.margin')
# head(mean.win.loss.margin)

# joining wins and losses percentages
# train <- left_join(left_join(train, season.wins.losses[,c('Season', 'team', 'percent.win')], by=c('Season', 'first.team' = 'team')), season.wins.losses[,c('Season', 'team', 'percent.win')], by=c('Season', 'second.team' = 'team'), suffix=c('.first.team', '.second.team'))
# train$percent.win.difference <- train$percent.win.second.team - train$percent.win.first.team
# head(train, 10)

# joining win and los margins
# train <- left_join(left_join(train, mean.win.loss.margin, by=c('Season', 'first.team' = 'team')), mean.win.loss.margin, by=c('Season', 'second.team' = 'team'), suffix=c('.first.team', '.second.team'))
# train$mean.win.margin.difference <-  train$mean.win.margin.second.team - train$mean.win.margin.first.team
# train$mean.loss.margin.difference <- train$mean.loss.margin.second.team - train$mean.loss.margin.first.team
# head(train)

# joining seed data
train <- left_join(left_join(train, tourney.seeds, by=c('Season', 'first.team' = 'Team')), tourney.seeds, by=c('Season', 'second.team' = 'Team'), suffix=c('.first.team', '.second.team'))
train$Seed.first.team <- as.numeric(str_extract(train$Seed.first.team, '[:digit:]+'))
train$Seed.second.team <- as.numeric(str_extract(train$Seed.second.team, '[:digit:]+'))
train$seed.difference <- train$Seed.first.team - train$Seed.second.team
head(train, 10)

# organizing detailed stats to join
regular.season.detailed.results$win.margin <- regular.season.detailed.results$Wscore - regular.season.detailed.results$Lscore
regular.season.detailed.results$w.dr.plus.l.or <- regular.season.detailed.results$Wdr + regular.season.detailed.results$Lor
regular.season.detailed.results$w.or.plus.l.dr <- regular.season.detailed.results$Wor + regular.season.detailed.results$Ldr
head(regular.season.detailed.results)
winner.history <- regular.season.detailed.results[,c("Season","Wteam","Daynum","Wscore","Numot","Wfgm","Wfga","Wfgm3","Wfga3","Wftm","Wfta","Wor","Wdr","Wast","Wto","Wstl","Wblk","Wpf", 'win.margin', 'w.dr.plus.l.or', 'w.or.plus.l.dr')]
winner.history$victory <- 1
loser.history <- regular.season.detailed.results[,c("Season","Lteam","Daynum","Lscore","Numot","Lfgm","Lfga","Lfgm3","Lfga3","Lftm","Lfta","Lor","Ldr","Last","Lto","Lstl","Lblk","Lpf", 'win.margin', 'w.dr.plus.l.or', 'w.or.plus.l.dr')]
loser.history$win.margin <- loser.history$win.margin * -1
head(loser.history)
loser.history$victory <- 0
colnames(winner.history) <- c("season","team","daynum","score","numot","fgmade","fgattempt","fgm3","fga3","ftmade","ftattempt","offreb","defreb","ast","turnover","steal","block","pfoul", 'score.margin', 'tot.def.reb.opps', 'tot.off.reb.opps', "victory")
colnames(loser.history) <- c("season","team","daynum","score","numot","fgmade","fgattempt","fgm3","fga3","ftmade","ftattempt","offreb","defreb","ast","turnover","steal","block","pfoul", 'score.margin', 'tot.off.reb.opps', 'tot.def.reb.opps',"victory")
reg.seas.det.join <- rbind(winner.history, loser.history)
reg.seas.det.join$steal.plus.block <- reg.seas.det.join$steal + reg.seas.det.join$block

#agging some stats
reg.seas.det.mean <- reg.seas.det.join %>% group_by(season, team) %>% summarise_all(mean, na.rm=T)
reg.seas.det.sum <- reg.seas.det.join %>% group_by(season, team) %>% summarise_all(sum, na.rm=T)
reg.seas.det.sum$fg.per <- reg.seas.det.sum$fgmade / reg.seas.det.sum$fgattempt
reg.seas.det.sum$fg.three.per <- reg.seas.det.sum$fgm3 / reg.seas.det.sum$fga3
reg.seas.det.sum$ft.per <- reg.seas.det.sum$ftmade / reg.seas.det.sum$ftattempt
reg.seas.det.sum$dr.per <- reg.seas.det.sum$defreb / reg.seas.det.sum$tot.def.reb.opps
reg.seas.det.sum$or.per <- reg.seas.det.sum$offreb / reg.seas.det.sum$tot.off.reb.opps
reg.seas.det.sum$ast.to.turn <- reg.seas.det.sum$ast / reg.seas.det.sum$turnover
sum.columns <- c('season', 'team', 'fg.per', 'fg.three.per', 'ft.per', 'dr.per', 'or.per', 'ast.to.turn')
mean.columns <- c('season', 'team', 'score.margin', 'numot', 'pfoul', 'steal.plus.block', 'victory')
reg.seas.det.mean <- reg.seas.det.mean %>% select(mean.columns)
reg.seas.det.sum <- reg.seas.det.sum %>% select(sum.columns)

# join stats
colnames(train)[1] <- 'season'
train <- left_join(train, reg.seas.det.mean, by=c('season', "first.team" = "team")) %>%
  left_join(reg.seas.det.mean, by=c('season', "second.team" = "team"), suffix=c('.first.team', '.second.team'))
train <- train %>% left_join(reg.seas.det.sum, by=c('season', "first.team" = "team")) %>%
  left_join(reg.seas.det.sum, by=c('season', "second.team" = "team"), suffix=c('.first.team', '.second.team'))

# calc differences
for (column.name in sum.columns[3:length(sum.columns)]){
  res <- paste(column.name, '.difference', sep='')
  first.col <- paste(column.name, '.first.team', sep='')
  second.col <- paste(column.name, '.second.team', sep='')
  train[, res] <- train[, first.col] - train[, second.col]
}
for (column.name in mean.columns[3:length(mean.columns)]){
  res <- paste(column.name, '.difference', sep='')
  first.col <- paste(column.name, '.first.team', sep='')
  second.col <- paste(column.name, '.second.team', sep='')
  train[, res] <- train[, first.col] - train[, second.col]
}

# keep only target and difference columns
info.columns <- c('season', 'first.team', 'second.team')
scaled.columns <- c(colnames(train)[grepl('.difference', colnames(train))])
scaled.train <- scale(train[, scaled.columns])
target.label <- train['first.team.win']
train <- cbind(train[info.columns], target.label, scaled.train)

dev <- train[train$season <= 2013, ]
val <- train[train$season > 2013, ]

# train log reg model
train.logit <- glm(first.team.win ~ ., data=dev)
summary(train.logit)

# train elnet model
x <- scaled.train
y <- train[, 'first.team.win']
fit.elnet <- glmnet(x, y)
summary(fit.elnet)
plot(fit.elnet)
print(fit.elnet)
cv.fit <- cv.glmnet(x, y)
plot(cv.fit)
log(cv.fit$lambda.min)
coef(cv.fit, s='lambda.min')


# train xgboost
xg.dev <- xgb.DMatrix(data=as.matrix(dev[, scaled.columns]), label=dev[, 'first.team.win'])
xg.val <- xgb.DMatrix(data=as.matrix(val[, scaled.columns]), label=val[, 'first.team.win'])
watchlist <- list(train=xg.dev, test=xg.val)
bst.cross.val <- xgb.cv(data=xg.dev, nfold=4, nrounds=100, metrics='logloss')
bst <- xgb.train(data=xg.dev, watchlist=watchlist, nrounds=5, eval.metric = "logloss", objective = "binary:logistic")
xgb.plot.importance(xgb.importance(feature_names=scaled.columns, model=bst))
xb.full.train <- xgboost(data=as.matrix(scaled.train), label=as.matrix(target.label), nrounds=4, eval.metric='logloss')


# metric
mult_log_loss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  return(sum(act * log(pred) + (1-act) * log(1-pred)) * -1/NROW(act))
}

# 1s and 0s
gte_point_five <- function(x){
  if (x >= .5){
    return(1)
  }
  return(0)
}



# add predictions of training data
train$prediction <- predict(train.logit, train)
#train$prediction.one.zero <- sapply(train$prediction, gte_zero)
#train$PredOne <- sapply(train$PredOne, gte_point_five)
head(train)
print(paste('train log loss:', mult_log_loss(train$first.team.win, train$prediction)))

# create score data
score <- separate(sample.submission, Id, c('Season', 'first.team', 'second.team'), '_')
score$Season <- as.numeric(score$Season)
score$first.team <- as.numeric(score$first.team)
score$second.team <- as.numeric(score$second.team)

# add tourney seeds
score <- left_join(left_join(score, tourney.seeds, by=c('Season', 'first.team' = 'Team')), tourney.seeds, by=c('Season', 'second.team' = 'Team'), suffix=c('.first.team', '.second.team'))
score$Seed.first.team <- as.numeric(str_extract(score$Seed.first.team, '[:digit:]+'))
score$Seed.second.team <- as.numeric(str_extract(score$Seed.second.team, '[:digit:]+'))
score$seed.difference <- score$Seed.first.team - score$Seed.second.team

# add win percents
# score <- left_join(left_join(score, w_l_season[,c('Season', 'team', 'percent_win')], by=c('Season', 'FirstTeam' = 'team')), w_l_season[,c('Season', 'team', 'percent_win')], by=c('Season', 'SecondTeam' = 'team'), suffix=c('.FirstTeam', '.SecondTeam'))
# score$percent_win_difference <- score$percent_win.FirstTeam - score$percent_win.SecondTeam

# add win and loss margins
# score <- left_join(left_join(score, mean_w_l_margin, by=c('Season', 'FirstTeam' = 'team')), mean_w_l_margin, by=c('Season', 'SecondTeam' = 'team'), suffix=c('.FirstTeam', '.SecondTeam'))
# score$mean_win_margin_difference <- score$mean_wins_margin.FirstTeam - score$mean_wins_margin.SecondTeam
# score$mean_loss_margin_difference <- score$mean_loss_margin.FirstTeam - score$mean_loss_margin.SecondTeam
# head(score)

# add stats
head(score)
colnames(score)[1] <- 'season'
score <- score %>% left_join(reg.seas.det.mean, by=c('season', "first.team" = "team")) %>%
  left_join(reg.seas.det.mean, by=c('season', "second.team" = "team"), suffix=c('.first.team', '.second.team'))
score <- score %>% left_join(reg.seas.det.sum, by=c('season', "first.team" = "team")) %>%
  left_join(reg.seas.det.sum, by=c('season', "second.team" = "team"), suffix=c('.first.team', '.second.team'))

# calc differences
for (column.name in sum.columns[3:length(sum.columns)]){
  res <- paste(column.name, '.difference', sep='')
  first.col <- paste(column.name, '.first.team', sep='')
  second.col <- paste(column.name, '.second.team', sep='')
  score[, res] <- score[, first.col] - score[, second.col]
}
for (column.name in mean.columns[3:length(mean.columns)]){
  res <- paste(column.name, '.difference', sep='')
  first.col <- paste(column.name, '.first.team', sep='')
  second.col <- paste(column.name, '.second.team', sep='')
  score[, res] <- score[, first.col] - score[, second.col]
}
score <- scale(score[, scaled.columns])

# score log reg
# sample.submission$Pred <- predict(train.logit, as.data.frame(score))

# score elnet
# sample.submission$Pred <- predict(cv.fit, score, s='lambda.min')

# score xgboost
sample.submission$Pred <- predict(xb.full.train, score)
head(sample.submission, 50)



# write to file
time_pst <- str_replace_all(str_replace_all(Sys.time(), "[[:punct:]]", '-'), ' ', '-')
write.csv(sample.submission, file=paste('submissions/sub-', time_pst, '.csv', sep=''), row.names=F)

