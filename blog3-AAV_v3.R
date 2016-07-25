library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)


########Score Models
score.AAV.model <- function(model, test.data) {
  
  test.yhat <-predict(model,newdata=test.data)
  
  ###fit metrics
  OOS.metrics <- cbind(round(sqrt((sum((test.data$AAV-test.yhat)^2))/nrow(test.data)),3),
                       round(mean((test.yhat - test.data$AAV)^2),3),
                       round(1 - sum((test.data$AAV-test.yhat)^2)/sum((test.data$AAV-mean(test.data$AAV))^2),3))
  
  colnames(OOS.metrics) <- c("RMSE","MSE","R2")
  
  return(OOS.metrics)
}

####Random Forest Model
rf.cross.v10 <- function(input, formula) {
  
  library(caret)
  # load the dataset
  input
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model <- train( formula ,
                 data=input, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance=TRUE)
  
  predictions <- predict(model, newdata=input)

  # var importance
  var.imp <- varImp(model, scale=FALSE)
  
  return(list(cbind(predictions,input),model,var.imp))
  
}

####################################
#######Prep CrowdScout Data
###################################

cs.data <- read.csv('~/CWA/Hockey Data/blog3_cs-score-contract.csv')
team.salary.15 <- read.csv('~/CWA/Hockey Data/team-salaries-15.csv')

###create variables & clean
cs.data <- cs.data[which(cs.data$AAV != "#N/A" & cs.data$game_count > 50 & cs.data$dob !='NULL' & cs.data$Pos != "G"),]
cs.data$AAV <- as.numeric(as.character(cs.data$AAV))/1000000
cs.data$elo <- as.numeric(as.character(cs.data$elo))

cs.data$score <- (cs.data$elo - min(cs.data$elo)) / (max(cs.data$elo) - min(cs.data$elo)) * 100

###age start of 2015 season
cs.data$dob <- as.Date(cs.data$dob, format = "%m/%d/%Y")
cs.data$Age.Start.1516 <- as.numeric(difftime("2015-10-01", cs.data$dob, units="days") / 365.25)

##draft info
cs.data$Drafted <- ifelse(cs.data$draft_year == "NULL", 0, 1)
cs.data$years.since.draft <- ceiling(cs.data$Age.Start.1516) - 18
#cs.data$years.since.draft <- ifelse(cs.data$draft_year == "NULL", ceiling(cs.data$Age.Start.1516) - 18,
#                                    2016 - as.integer(as.character(cs.data$draft_year)))

###position variables
cs.data$Pos <- gsub(c("LW"),"W",cs.data$Pos)
cs.data$Pos <- gsub(c("RW"),"W",cs.data$Pos)
cs.data$Pos <- as.factor(cs.data$Pos) 
#cs.data$gt1M <- as.factor(ifelse(cs.data$AAV > 1,1,0))
#cs.data$gt6M <- as.factor(ifelse(cs.data$AAV > 6,1,0))

###age buckets
cs.data$age_group <- as.factor(ifelse(cs.data$Age.Start.1516>=35, "gt35",
                                      ifelse(35>cs.data$Age.Start.1516 & cs.data$Age.Start.1516>=31, "31-35",
                                             ifelse(31>cs.data$Age.Start.1516 & cs.data$Age.Start.1516>=28, "28-31",
                                                    ifelse(21>cs.data$Age.Start.1516 & cs.data$Age.Start.1516>=17, "18-21",
                                                           ifelse(24>cs.data$Age.Start.1516 & cs.data$Age.Start.1516>=21, "21-24",
                                                                  ifelse(28>cs.data$Age.Start.1516 & cs.data$Age.Start.1516>=24, "24-28",  
                                                                         round(cs.data$Age.Start.1516,0))))))))


###team salary information
cs.data <- merge(cs.data, team.salary.15[, c("Team", "Team.Salary")], by="Team")
cs.data$Team.Salary <- as.numeric(cs.data$Team.Salary)
cs.data$Term <- as.integer(as.character(cs.data$Term))
#cs.data$years.since.draft <- (2015 - as.numeric(cs.data$draft_year) - 1990)

cs.data$name <- toupper(gsub(" ",".",cs.data$player_name))

###########################################
####Hockey Analysis Data
###########################################

###load all player stats

load.stats <- function(season) {
  
  data <- read.csv(paste0("C:/Users/colander1/Documents/CWA/Hockey Data/Hockey Analysis/SuperSkaterStats ",season,".csv"))
  
  data <- data[complete.cases(data$GP), ]
  data$season <- season
  
  return(data)
}

#player.seasons <- list("2015-16","2014-15","2013-14","2012-13","2011-12","2010-11","2009-10","2008-09","2007-08")
player.seasons <- "2015-16" # only last season in analysis

player.stats <-  do.call(rbind,lapply(FUN=load.stats,player.seasons))

###edit names
player.stats$name <- toupper(gsub(" ",".",player.stats$Player.Name))
names(player.stats)[3] <- "Pos.HA"

###fix
player.stats$iFirstA.60 <- player.stats$iFirstA / (player.stats$TOI / 60)
player.stats$TOI.Gm <- player.stats$TOI / player.stats$GP

#####combine player data

player.data <- merge(cs.data, player.stats, by="name")
player.data <- player.data[complete.cases(player.data), ]

##########
##Load RF Player Scoring Models 
#########

load("C:/Users/colander1/Documents/CWA/Blog/Data/rfmodel.rda")
load("C:/Users/colander1/Documents/CWA/Blog/Data/lmmodel.rda")
rfmodel <- (rfmodel)
lmmodel <- (lmmodel)


##predict skater
rf.predictions <- predict(rfmodel,player.data)
lm.predictions <- predict(lmmodel,player.data)

player.data <- cbind(rf.predictions, lm.predictions, player.data)
player.data$Predicted.Score <- (player.data$rf.predictions + player.data$lm.predictions) / 2

####################
##RANDOM FOREST
####################
library(randomForest)

cs.rf.model <- rf.cross.v10(player.data, AAV ~ Predicted.Score + Age.Start.1516 + Pos + Term + Drafted)

####summary
rf.model.scoring <- cs.rf.model[[2]]
save(rf.model.scoring, file="C:/Users/colander1/Documents/CWA/Blog/Data/rf-model-scoring.rda")

####fit metrics
cs.rf.model.details <- cs.rf.model[[2]]
cs.rf.model.details$results
#mtry      RMSE  Rsquared    RMSESD RsquaredSD
#1    2 0.9478527 0.8098290 0.1081802 0.05276237
#2    3 0.9426492 0.8081342 0.1019517 0.04998919
#3    4 0.9528827 0.8038060 0.1003132 0.04810575
#4    5 0.9617509 0.8004405 0.1025545 0.04978024
#5    6 0.9665795 0.7984976 0.1028314 0.04988734

score.AAV.model(rf.model.scoring,cs.rf.scored)
##RMSE   MSE    R2
##0.45 0.203 0.955

###Variable Importance
ggplot(data=cs.rf.model[[3]]) +
  labs(title="Variable Importance\nRandom Forest Model")


######Create Residuals
cs.rf.scored <- cs.rf.model[[1]]
cs.rf.scored$Residual <- (cs.rf.scored$predictions - cs.rf.scored$AAV)
cs.rf.scored$lastname = sapply(strsplit(as.character(cs.rf.scored$Player.Name), ' '), function(x) x[length(x)])


###plot
ggplot(data=cs.rf.scored, aes(x=AAV,y=predictions, color=Residual, label=lastname)) + 
  geom_point(aes(shape = factor(Pos))) +
  geom_text(aes(label=lastname), angle=-45, check_overlap = TRUE) +
  theme(text = element_text(size=20)) +
  labs(title="CrowdScout Score Expected AAV vs Actual AAV\nRandom Forest Model, 2015-16") +
  labs(x="AAV (millions $)", y="Random Forest Expected AAV (millions $)") +
  ##ylim(0,8) +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  annotate("segment",x=0,y=0,xend=10,yend=10) +
  annotate("text", x = 0.1, y = 8 , hjust=0, label = paste("Fit Metrics:")) +
  annotate("text", x = 0.1, y = 7.7 , hjust=0, label = paste("RMSE: ", 
                                                      round(as.numeric(score.AAV.model(rf.model.scoring,cs.rf.scored) [1]),3))) +
  annotate("text", x = 0.1, y = 7.4 , hjust=0, label = paste("R2: ", 
                                                      round(as.numeric(score.AAV.model(rf.model.scoring,cs.rf.scored) [3]),3))) +
  annotate("text", x = 0.1, y = 7 , hjust=0, label = "@CrowdScoutSprts") 

#####plot by team
                             
ggplot(data=cs.rf.scored, aes(reorder(Team.x, Residual), Residual, color=AAV)) + 
  geom_boxplot(data = cs.rf.scored, aes(reorder(Team.x, Residual, fill=mean(Residual)))) +
  geom_point(aes(shape = factor(Pos))) +
   coord_flip() +
  geom_text(aes(label=lastname), angle=-90, check_overlap = TRUE) +
  theme(text = element_text(size=20)) +
  labs(title="Expected AAV vs Actual AAV by Team, 2015-16\nRandom Forest Model") +
  labs(x="Team", y="Random Forest Expected AAV - Actual AAV  (millions $)") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green", 
                          midpoint = mean(cs.rf.scored$AAV),limits=c(0, 10.5)) +
  annotate("text", x = 1, y = -1.5 , hjust=0, label = "@CrowdScoutSprts")
