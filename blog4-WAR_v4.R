library(randomForest)
library(reshape2)
library(ggplot2)
library(dplyr)
library(caret)
library(devtools)
source_gist("524eade46135f6348140")

########Score Models
score.model <- function(model, test.data) {
  
  test.yhat <-predict(model,newdata=test.data)
  
  ###fit metrics
  OOS.metrics <- cbind(round(sqrt((sum((test.data$score-test.yhat)^2))/nrow(test.data)),3),
                       round(mean((test.yhat - test.data$score)^2),3),
                       round(1 - sum((test.data$score-test.yhat)^2)/sum((test.data$score-mean(test.data$score))^2),3))
  
  colnames(OOS.metrics) <- c("RMSE","MSE","R2")
  
  return(OOS.metrics)
}

####################################
#######Prep CrowdScout Data
###################################

cs.data <- read.csv('~/CWA/Hockey Data/blog3_cs-score-contract.csv')

###create variables & clean
cs.data <- cs.data[which(cs.data$AAV != "#N/A" & cs.data$game_count > 50 & cs.data$dob !='NULL' & cs.data$Pos != "G"),]
cs.data$AAV <- as.numeric(as.character(cs.data$AAV))/1000000
cs.data$elo <- as.numeric(as.character(cs.data$elo))

###create variables
cs.data$score <- (cs.data$elo - min(cs.data$elo)) / (max(cs.data$elo) - min(cs.data$elo)) * 100

###age start of 2015 season
cs.data$dob <- as.Date(cs.data$dob, format = "%m/%d/%Y")
cs.data$age15 <- as.numeric(difftime("2015-10-01", cs.data$dob, units="days") / 365.25)

###position variables
cs.data$Pos <- gsub(c("LW"),"W",cs.data$Pos)
cs.data$Pos <- gsub(c("RW"),"W",cs.data$Pos)
cs.data$Pos <- as.factor(cs.data$Pos) 

####name collapse
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

player.data <- merge(cs.data[, c("name", "score", "Pos", "age15")], player.stats, by="name")
player.data <- player.data[complete.cases(player.data), ]

###########################################
####Variable Selection
###########################################

linear.combos <- findLinearCombos(player.data[-is.na(player.data),c(8:ncol(player.data))])


######################
##automatically select features using Caret R PackageR
######################

recursive.feature.elimin <- function(data, model.vars) {
      # ensure the results are repeatable
      set.seed(7)
      # load the library
      library(mlbench)
      library(caret)
      # load the data
      data <- data[names(data) %in% model.vars]
      data <- data[complete.cases(data),]
      
      # define the control using a random forest selection function
      control <- rfeControl(functions=rfFuncs, method="cv", number=10)
      # run the RFE algorithm
      results <- rfe(data[3:ncol(data)], data[,2], sizes=c(3:ncol(data)), rfeControl=control)
      # summarize the results
      print(results)
      # list the chosen features
      predictors(results)
      # plot the results
      return(results)
  }

###test linear combos
linear.combos.ds <- as.data.frame(player.data[names(player.data) %in% model.vars.rfe], stringsAsFactors = TRUE )
linear.combos <- findLinearCombos(linear.combos.ds[c(4:ncol(linear.combos.ds))])

model.vars.rfe <- c('name','score','Pos','TOI.Gm',	'GF60',	'GA60',	'TMGF60',	'TMGA60',	'OppGF60',	'OppGA60',	
                    'GF60.RelTM',	'GA60.RelTM',	'GF..RelTM',	'SF60',	'SA60',	'TMSF60',	'TMSA60',	'OppSF60',	'OppSA60',	
                    'SF60.RelTM',	'SA60.RelTM',	'Sh.',	'Sv.',	'Sh..RelTM',	'Sv..RelTM',	'FF60',	'FA60',	'FSh.',	
                    'FSv.',	'TMFF60',	'TMFA60',	'OppFF60',	'OppFA60',	'FF60.RelTM',	'FA60.RelTM',	'CF60',	'CA60',	
                    'CSh.',	'CSv.',	'TMCF60',	'TMCA60',	'OppCF60',	'OppCA60',	'CF60.RelTM',	'CA60.RelTM',	
                    'iSh.',	'iGoals/60',	'iAssists/60',	'iFirstA/60',	'iPoints/60',	'iPrimaryPoints/60',	'iShots/60',	
                    'iFenwick/60',	'iCorsi/0',	'IPP',	'IGP',	'IAP',	'IPPP',	'NZFO.',	'DZFO.',	'OZFO.',	'.ofTeam.TOI',
                    '.ofTeam.GF',	'.ofTeam.GA',	'.ofTeam.SF',	'.ofTeam.SA',	'.ofTeam.FF',	'.ofTeam.FA',	'.ofTeam.CF',	
                    '.ofTeam.CA',	'.ofTeam.OZFO',	'.ofTeam.DZFO',	'.ofTeam.NZFO')

rfe <- recursive.feature.elimin(player.data,model.vars.rfe)
plot(rfe, type=c("g", "o"),main="Recursive Feature Elimination\nPlayer Metrics Predicting CrowdScout Score") 

top.variables <- as.matrix(rfe$fit$forest$xlevels,ncol=1)
top.variables <- as.data.frame(paste0(rownames(top.variables),sep=""))


###from above ###38  #now 48
##OLD##model.vars <- c('TOI','iFirstA','GF60','SF60','FF60','CF60','SF60.RelTM','GF60.RelTM','TMGF60','OppGF60','OppFF60','OppSF60','CF60.RelTM','FF60.RelTM','OppCF60','OZFO.','CSh.','Sh.','GA60.RelTM','DZFO.','FA60','GF..RelTM','NZFO.','TMFA60','GP','FSh.','OppGA60','TMGA60','Sv..RelTM','SA60','iSh.','FA60.RelTM','TMFF60','IPPP','IGP','IPP','CA60','TMSF60')
#ORIGINALmodel.vars <- c('GF60',	'SF60',	'FF60',	'OppSF60',	'OppGF60',	'OppFF60',	'GF60.RelTM',	'SF60.RelTM',	'CF60',	'OppCF60',	'TMGF60',	'CSh.',	'Sh.',	'CF60.RelTM',	'FF60.RelTM',	'FSh.',	'Pos',	'TMFA60',	'GA60.RelTM',	'OppGA60',	'OZFO.',	'DZFO.',	'iSh.',	'GF..RelTM',	'TMGA60',	'FA60',	'OppCA60',	'IPP',	'TMFF60',	'TMSA60',	'IAP',	'TMSF60',	'CSv.',	'GA60',	'Sv..RelTM',	'FSv.',	'FA60.RelTM',	'OppFA60',	'OppSA60',	'SA60',	'CA60.RelTM',	'Sv.',	'SA60.RelTM',	'IGP',	'Sh..RelTM',	'IPPP')


##########################################
####Linear Model Cross-Validation
##########################################

lm.cross.v10 <- function(input, model.vars) {
  
  library(caret)
  library(Rcmdr)
  
  # load the dataset
  input
  input.cc <- input[names(input) %in% model.vars]
  input.cc <- input.cc[complete.cases(input.cc),]
  
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model.output <- train( score ~ . , data=input.cc[2:ncol(input.cc)], trControl=train_control, method="glm", 
                          tuneLength=10)
  
  # predict
  predictions <- predict(model.output, newdata=input)
  
  # var importance
  var.imp <- varImp(model.output)
  
  return(list(cbind(predictions,input),model.output,var.imp))
  
}


model.vars <- c('TOI.Gm',
                'GF60','GA60',
                'CF60','CA60',
                #'OppCF60',	
                'OppCA60', 
                #'TMCF60',	
                'TMCA60',
                #'GF60.RelTM','GA60.RelTM',
                'CF60.RelTM',#'CA60.RelTM',
                'OZFO.', #'NZFO.', #,'DZFO.',
                #'iSh.',#	'Sh.',
                #'IPPP', 
                'iFirstA.60','iGoals.60')

###correlation matrix
qplot(x=Var1, y=Var2, data=melt(cor(player.data[names(player.data) %in% model.vars])), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1),low="purple", mid="dark grey",high="orange") +
  labs(title="Correlation Matrix\n2015-16 Select Skater Metrics, puckalytics.com")
  

################
####GLM Skaters
################
lm.model <- lm.cross.v10(player.data, c("score","name","Pos",model.vars))

###output dataset
lm.scored <- lm.model[[1]]
lm.scored$Residuals <- lm.scored$predictions - lm.scored$score

####output coefficients
lm.model[[2]]$results$RMSE
# 12.55357
lm.model[[2]]$results$Rsquared
# 0.714768

score.model(lmmodel,player.data)
# RMSE     MSE    R2
# 12.304 151.396 0.716


###output sumamry
summary(lm.model[[2]])
lmmodel <- lm.model[[2]]
###RMSE      Rsquared 
###14.09568  0.6579524

###plot variable importance
ggplot(data=lm.model[[3]]) +
    labs(title="Variable Importance\nGeneralized Linear Model")

lm.scored$lastname = sapply(strsplit(as.character(lm.scored$Player.Name), ' '), function(x) x[length(x)])

###plot
ggplot(data=lm.scored, aes(x=score,y=predictions, color=Residuals, label=lastname)) + 
  geom_point(aes(shape = factor(Pos))) +
  geom_text(aes(label=lastname), angle=-45, check_overlap = TRUE) +
  theme(text = element_text(size=20)) +
  labs(title="Predicted vs Actual CrowdScout Score\nGeneralized Linear Model, 2015-16") +
  labs(x="Actual CrowdScout Score", y="Predicted CrowdScout Score") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  annotate("segment",x=0,y=0,xend=100,yend=100) +
  annotate("text", x = 70, y = 21 , hjust=0, label = paste("Fit Metrics:")) +
  annotate("text", x = 70, y = 15 , hjust=0, label = paste("RMSE: ", 
                                                             round(as.numeric(score.model(lmmodel,player.data) [1]), 3))) +
  annotate("text", x = 70, y = 10 , hjust=0, label = paste("R2: ", 
                                                             round(as.numeric(score.model(lmmodel,player.data) [3]), 3))) +
  annotate("text", x = 70, y = 5 , hjust=0, label = "@CrowdScoutSprts") 


####################
##RANDOM FOREST
####################

####Random Forest Model
rf.cross.v10 <- function(input, model.vars, label.vars) {
  
  library(caret)
  # load the dataset
  input
  input.cc <- input[names(input) %in% model.vars]
  input.cc <- input.cc[complete.cases(input.cc),]
  input.model <- input.cc[!(names(input.cc) %in% c("name"))]
  # define training control
  set.seed(1234)
  train_control <- trainControl(method="cv", number=10)
  
  # train the model
  model <- train( score ~ . ,
                  data=input.model, trControl=train_control, method="rf", tuneLength=6, ntree=500, importance = TRUE)
  
  predictions <- predict(model, newdata=input.cc)
  
  # var importance
  var.imp <- varImp(model, scale=FALSE)
  
  return(list(cbind(predictions,input.cc),model, var.imp))
  
}

####################################
######Skater Random Forest
####################################

rf.model <- rf.cross.v10(player.data,c("score","name","Pos",model.vars))

###output dataset
rf.scored <- rf.model[[1]]
rf.scored$Residuals <- rf.scored$predictions - rf.scored$score

####output metrics, explaining variation, not variance
rf.model[[2]]$results$RMSE
##12.72754 12.57471 12.48472 12.48814 12.48636 12.56895
rf.model[[2]]$results$Rsquared
##0.7101481 0.7122057 0.7143866 0.7131747 0.7132254 0.7085013

score.model(rfmodel,player.data)
#RMSE    MSE    R2
#5.21 27.145 0.949


###plot variable importance
ggplot(data=rf.model[[3]]) +
  labs(title="Variable Importance\nRandom Forest Model")

rf.scored$lastname <- lm.scored$lastname

###plot
ggplot(data=rf.scored, aes(x=score,y=predictions, color=Residuals, label=lastname,shape)) + 
  geom_text(aes(label=lastname), angle=-45, check_overlap = TRUE) +
  geom_point(aes(shape = factor(Pos))) +
  theme(text = element_text(size=20)) +
  labs(title="Predicted vs Actual CrowdScout Score\nRandom Forest Model, 2015-16") +
  labs(x="Actual CrowdScout Score", y="Predicted CrowdScout Score") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  annotate("segment",x=0,y=0,xend=100,yend=100) +
  annotate("text", x = 70, y = 21 , hjust=0, label = paste("Fit Metrics:")) +
  annotate("text", x = 70, y = 15 , hjust=0, label = paste("RMSE: ", 
                                                           round(as.numeric(score.model(rfmodel,player.data) [1]), 3))) +
  annotate("text", x = 70, y = 10 , hjust=0, label = paste("R2: ", 
                                                           round(as.numeric(score.model(rfmodel,player.data) [3]), 3))) +
  annotate("text", x = 70, y = 5 , hjust=0, label = "@CrowdScoutSprts") 

#############################################
##Save Models
#############################################

###save models
save(rfmodel,file="C:/Users/colander1/Documents/CWA/Blog/Data/rfmodel.rda")
###save models
save(lmmodel,file="C:/Users/colander1/Documents/CWA/Blog/Data/lmmodel.rda")


#############################################
##Check Model Residual Relationship
#############################################

player.resids <- cbind(lm.scored[c("lastname","Residuals","score","Pos")],rf.scored[c("Residuals")])
colnames(player.resids) <- c("name","GLM.Residuals","score","Pos","RF.Residuals")

resid.fit <- lm(GLM.Residuals ~ RF.Residuals,data=player.resids)
predict <- predict(resid.fit,data=player.resids)
resid.data <- cbind(predict,player.resids)

###plot
ggplot(data=player.resids, aes(x=RF.Residuals,y=GLM.Residuals, color=score, label=name)) + 
  geom_text(aes(label=name), angle=0, check_overlap = TRUE) +
  geom_point(aes(shape = factor(Pos))) +
  geom_smooth(method = "lm", se = FALSE) +
  theme(text = element_text(size=20)) +
  labs(title="GLM vs. RF Model Residual Compare\nPlayer-Level Residuals") +
  labs(x="Random Forest Model Residuals", y="Generalized Linear Model Model Residuals") +
  scale_colour_gradient2(low="Dark Red", mid="dark grey",high="dark green") +
  annotate("segment",x=-50,y=-50,xend=50,yend=50) +
annotate("text", x = 0, y = -50, hjust=0, label = "@CrowdScoutSprts") +
  stat_smooth_func(geom="text",method="lm",hjust=0.6,parse=TRUE)


