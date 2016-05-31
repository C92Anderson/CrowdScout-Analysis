library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)

################################
#####Create functions
################################

#####score model
score.model <- function(model, test.data, lab="Model", iter=1) {
  
  test.yhat <-predict(model,newdata=test.data)
  
  ###fit metrics
       OOS.metrics <- cbind(lab, iter, 
                            round(sqrt((sum((test.data$AAV-test.yhat)^2))/nrow(test.data)),3),
                            round(mean((test.yhat - test.data$AAV)^2),3),
                            round(1 - sum((test.data$AAV-test.yhat)^2)/sum((test.data$AAV-mean(test.data$AAV))^2),3))
  
  colnames(OOS.metrics) <- c("Model","Iteration","RMSE","MSE","R2")

    return(OOS.metrics)
}

####Linear Model Cross-Validation
lm.cross.v10 <- function(input, formula) {
  
  library(caret)
  # load the dataset
  input
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model.output <- train( formula , data=input, trControl=train_control, method="glm", tuneLength=5)
  
  predictions <- predict(model.output, newdata=input)
  return(list(cbind(predictions,input),model.output))
  
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
                 data=input, trControl=train_control, method="rf", tuneLength=6, ntree=500)
  
  predictions <- predict(model, newdata=input)

  return(list(cbind(predictions,input),model))
  
}
 

###################################
#####Prep WAR Data
###################################

war.data <- read.csv('~/CWA/Hockey Data/WAR-AAV.csv')

war.data <- war.data[complete.cases(war.data[ , c("AAV")]), ]

###age variables
###age buckets
war.data$age_group <- as.factor(ifelse(war.data$Age>=35, "gt35",
                                      ifelse(35>war.data$Age & war.data$Age>=31, "31-35",
                                             ifelse(31>war.data$Age & war.data$Age>=28, "28-31",
                                                    ifelse(21>war.data$Age & war.data$Age>=17, "18-21",
                                                           ifelse(24>war.data$Age & war.data$Age>=21, "21-24",
                                                                  ifelse(28>war.data$Age & war.data$Age>=24, "24-28",  
                                                                         round(war.data$Age,0))))))))



##########################
#######GAR GLM
##########################

####Run CV GLM function
war.lm.model <- lm.cross.v10(war.data, AAV ~ GAR + age_group + Pos)

####summary
summary(war.lm.model[[2]])

####output coefficients
war.model.details <- summary(war.lm.model[[2]])
war.model.coeffs <- as.data.frame(war.model.details$coefficients)

####fit metrics
war.lm.model[[2]]  
###RMSE      Rsquared 
###1.574438  0.3441322

######Score OOS
##score.model(war.lm.model,war.testing,"WAR LM")

######Create Residuals
war.scored <- war.lm.model[[1]]
war.scored$Residual <- (war.scored$predictions - war.scored$AAV)

###plot
ggplot(data=war.scored, aes(x=AAV,y=predictions, color=Residual)) + 
  geom_point() +
  theme(text = element_text(size=20)) +
  labs(title="GAR Expected AAV vs Actual AAV\nGeneralized Linear Model, 2014-15") +
  labs(x="AAV (millions $)", y="GLM Expected AAV (millions $)") +
  ylim(0,8) +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green", midpoint = 0) +
  annotate("segment",x=0,y=0,xend=8,yend=8) +
  annotate("text", x = 0.1, y = 8 , hjust=0, label = paste("Cross Validated (10 fold) Sampling:")) +
  annotate("text", x = 0.1, y = 7.7 , hjust=0, label = paste("RMSE: ", round(war.lm.model[[2]]$results$RMSE,3))) +
  annotate("text", x = 0.1, y = 7.4 , hjust=0, label = paste("R2: ", round(war.lm.model[[2]]$results$Rsquared,3))) +
  annotate("text", x = 0.1, y = 7 , hjust=0, label = "@CrowdScoutSprts") 


####################################
#######Prep CrowdScout Data
###################################

cs.data <- read.csv('~/CWA/Hockey Data/blog3_cs-score-contract.csv')
team.salary.15 <- read.csv('~/CWA/Hockey Data/team-salaries-15.csv')

###create variables & clean
cs.data <- cs.data[which(cs.data$AAV != "#N/A" & cs.data$game_count > 70 & cs.data$dob !='NULL'),]
cs.data$AAV <- as.numeric(as.character(cs.data$AAV))/1000000
cs.data$elo <- as.numeric(as.character(cs.data$elo))

#cs.data$draft_year <- as.factor(cs.data$draft_year) 
cs.data$score <- (cs.data$elo - min(cs.data$elo)) / (max(cs.data$elo) - min(cs.data$elo)) * 100

###age start of 2015 season
cs.data$dob <- as.Date(cs.data$dob, format = "%m/%d/%Y")
cs.data$age15 <- as.numeric(difftime("2015-10-01", cs.data$dob, units="days") / 365.25)

###position variables
cs.data$Pos <- gsub(c("LW"),"W",cs.data$Pos)
cs.data$Pos <- gsub(c("RW"),"W",cs.data$Pos)
cs.data$Pos <- as.factor(cs.data$Pos) 
#cs.data$gt1M <- as.factor(ifelse(cs.data$AAV > 1,1,0))
#cs.data$gt6M <- as.factor(ifelse(cs.data$AAV > 6,1,0))

###age buckets
cs.data$age_group <- as.factor(ifelse(cs.data$age15>=35, "gt35",
                             ifelse(35>cs.data$age15 & cs.data$age15>=31, "31-35",
                                    ifelse(31>cs.data$age15 & cs.data$age15>=28, "28-31",
                                           ifelse(21>cs.data$age15 & cs.data$age15>=17, "18-21",
                                                  ifelse(24>cs.data$age15 & cs.data$age15>=21, "21-24",
                                                         ifelse(28>cs.data$age15 & cs.data$age15>=24, "24-28",  
                                                                round(cs.data$age15,0))))))))


###team salary information
cs.data <- merge(cs.data, team.salary.15[, c("Team", "Team.Salary")], by="Team")
cs.data$Team.Salary <- as.numeric(cs.data$Team.Salary)
cs.data$Term <- as.numeric(as.factor(cs.data$Term))

###########################################
####CrowdScout Linear Model
###########################################

####Run CV GLM function
cs.lm.model <- lm.cross.v10(cs.data, AAV ~ score + age_group + Pos)

####summary
summary(cs.lm.model[[2]])

####output coefficients
cs.model.details <- summary(cs.lm.model[[2]])
cs.model.coeffs <- as.data.frame(cs.model.details$coefficients)

####fit metrics
cs.lm.model[[2]]  
###  RMSE      Rsquared 
###1.305061  0.6361894

######Create Residuals
cs.lm.scored <- cs.lm.model[[1]]
cs.lm.scored$Residual <- (cs.lm.scored$predictions - cs.lm.scored$AAV)

###plots
ggplot(data=cs.lm.scored, aes(x=AAV,y=predictions, color=Residual)) + 
  geom_point() +
  theme(text = element_text(size=20)) +
  labs(title="CrowdScout Score Expected AAV vs Actual AAV\nGeneralized Linear Model, 2015-16") +
  labs(x="AAV (millions $)", y="GLM Expected AAV (millions $)") +
  ylim(0,8) +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  annotate("segment",x=0,y=0,xend=8,yend=8) +
  annotate("text", x = 0.1, y = 8 , hjust=0, label = paste("Cross Validated (10 fold) Sampling:")) +
  annotate("text", x = 0.1, y = 7.7 , hjust=0, label = paste("RMSE: ", round(cs.lm.model[[2]]$results$RMSE,3))) +
  annotate("text", x = 0.1, y = 7.4 , hjust=0, label = paste("R2: ", round(cs.lm.model[[2]]$results$Rsquared,3))) +
  annotate("text", x = 0.1, y = 7 , hjust=0, label = "@CrowdScoutSprts") 


####plot by team
ggplot(data=cs.lm.scored, aes(reorder(Team, Residual), Residual, color=AAV)) + 
  geom_boxplot(data = cs.lm.scored, aes(reorder(Team, Residual, fill=mean(Residual)))) +
  geom_point() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  labs(title="CrowdScout Score Expected AAV vs Actual AAV by Team\nGeneralized Linear Model, 2015-16") +
  labs(x="Team (Descending Team Mean Residual)", y="GLM Expected AAV - Actual AAV  (millions $)") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green", 
                         midpoint = mean(cs.lm.scored$AAV),limits=c(0, 10.5)) +
  annotate("text", x = 1, y = 2, hjust= 0, label = "@CrowdScoutSprts") 


####################
##RANDOM FOREST
####################
library(randomForest)

cs.rf.model <- rf.cross.v10(cs.data2, AAV ~ score + age15 + Pos + draft_year + Term + GAR)

####summary
cs.rf.model[[2]]

####fit metrics
cs.rf.model.details <- cs.rf.model[[2]]
cs.rf.model.details$results
ggplot(cs.rf.model.details) + labs(title = "Random Forest")
###  mtry      RMSE  Rsquared     RMSESD RsquaredSD
##   2 1.5004312 0.7044310 0.11529039 0.07112214
##   7 1.0240141 0.7867020 0.07845404 0.05890156
##  13 0.9853192 0.7919126 0.08709352 0.05438953
##  19 0.9915357 0.7883710 0.08418611 0.05148263
##  25 0.9951628 0.7872361 0.08174994 0.04955841
##  31 1.0028674 0.7842598 0.08159666 0.04913634


###Variable Importance
rf.in.sample <- randomForest(AAV ~ score + age15 + Pos + Team.Salary + Term + draft_year +GAR, 
                      data=cs.data2, ntree=500)

# Variable Importance Table
var.imp <- data.frame(importance(rf.in.sample,
                                 type=2))

# make row names as columns
var.imp$Variables <- row.names(var.imp)

var.imp$Variables <- gsub("Pos","Position",var.imp$Variables)
var.imp$Variables <- gsub("Team.Salart","Team Salary (2015-16)",var.imp$Variables)
var.imp$Variables <- gsub("age15","Age (start of 2015-16)",var.imp$Variables)
var.imp$Variables <- gsub("draft_year","Draft Year",var.imp$Variables)
var.imp$Variables <- gsub("score","CrowdScout Score (0-100)",var.imp$Variables)

ggplot(data=var.imp, aes(reorder(Variables, IncNodePurity),IncNodePurity)) +
    geom_bar(stat = "identity") +
    labs(x="", y="Node Purity", title="Random Forest Model Variable Importance") +
    coord_flip() 

######Create Residuals
cs.rf.scored <- cs.rf.model[[1]]
cs.rf.scored$Residual <- (cs.rf.scored$predictions - cs.rf.scored$AAV)

###plot
ggplot(data=cs.rf.scored, aes(x=AAV,y=predictions, color=Residual)) + 
  geom_point() +
  theme(text = element_text(size=20)) +
  labs(title="CrowdScout Score Expected AAV vs Actual AAV\nRandom Forest Model, 2015-16") +
  labs(x="AAV (millions $)", y="Random Forest Expected AAV (millions $)") +
  ##ylim(0,8) +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  annotate("segment",x=0,y=0,xend=10,yend=10) +
  annotate("text", x = 0.1, y = 8 , hjust=0, label = paste("Fit Metrics:")) +
  annotate("text", x = 0.1, y = 7.7 , hjust=0, label = paste("RMSE: ", 
                                                      round(as.numeric(cs.rf.model.details$results[3,2]),3))) +
  annotate("text", x = 0.1, y = 7.4 , hjust=0, label = paste("R2: ", 
                                                      round(as.numeric(cs.rf.model.details$results[3,3]),3))) +
  annotate("text", x = 0.1, y = 7 , hjust=0, label = "@CrowdScoutSprts") 

#####plot by team
                             
ggplot(data=cs.rf.scored, aes(reorder(Team, Residual), Residual, color=AAV)) + 
  geom_boxplot(data = cs.rf.scored, aes(reorder(Team, Residual, fill=mean(Residual)))) +
  geom_point() +
  coord_flip() +
  theme(text = element_text(size=20)) +
  labs(title="Expected AAV vs Actual AAV by Team\nRandom Forest Model") +
  labs(x="Team", y="Random Forest Expected AAV - Actual AAV  (millions $)") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green", 
                          midpoint = mean(cs.rf.scored$AAV),limits=c(0, 10.5)) +
  annotate("text", x = 1.5, y = 1 , hjust=0, label = "@CrowdScoutSprts") 

 
#####plot by team2

ggplot(data=cs.rf.scored, aes(x=AAV,y=predictions, color=Team)) + 
  geom_point() +
  theme(text = element_text(size=20)) +
  labs(title="CrowdScout Score Expected AAV vs Actual AAV\nRandom Forest Model, 2015-16") +
  labs(x="AAV (millions $)", y="Random Forest Expected AAV (millions $)") +
  annotate("segment",x=0,y=0,xend=10,yend=10) +
  annotate("text", x = 0.1, y = 8 , hjust=0, label = paste("Fit Metrics:")) +
  annotate("text", x = 0.1, y = 7.7 , hjust=0, label = paste("RMSE: ", 
                                                             round(as.numeric(cs.rf.model.details$results[3,2]),3))) +
  annotate("text", x = 0.1, y = 7.4 , hjust=0, label = paste("R2: ", 
                                                             round(as.numeric(cs.rf.model.details$results[3,3]),3))) +
  annotate("text", x = 0.1, y = 7 , hjust=0, label = "@CrowdScoutSprts") 


#####################
####Team Spending Efficiency
####################
library(dplyr)

team.residuals <- merge(setNames(aggregate(Residual ~ Team, cs.rf.scored, sum ), c("Team","mean.resid")),
                        setNames(aggregate(Residual ~ Team, cs.rf.scored, length ), c("Team","n.players")), by = "Team")

team.residuals <- merge(team.salary.15, team.residuals, by="Team")
team.residuals$team.residuals <- ((team.residuals$ACTIVE.PLAYERS / team.residuals$n.players) * team.residuals$mean.resid)
team.residuals$resid.to.cap <- team.residuals$team.residuals / (team.residuals$Team.Salary / 1000000)

######################
####Output by Team
#####################

team.plots <- function(team) {
    
    pl <- ggplot(data=cs.rf.scored[which(cs.rf.scored$Team==team),], aes(reorder(player_name, Residual), Residual)) +
##    geom_bar(stat="identity", aes( fill=AAV)) +
    
          geom_bar(stat="identity") +
      coord_flip() +
  #    theme(text = element_text(size=20)) +
      labs(title=paste(team,"Expected AAV vs Actual AAV by Team\nRandom Forest Model")) +
      labs(x="Team", y="Random Forest Expected AAV - Actual AAV  (millions $)") +
      scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green", 
                             midpoint=0,limits=c(0, 10.5)) +
      annotate("text", x = 1.5, y = 1.5 , hjust=0, label = "@CrowdScoutSprts") 
    
  setwd("~/CWA/Blog/Graphics/blog3")  
  ggsave(paste(team,"Plot.pdf"),pl)

}

team.plots("BlueJackets")
