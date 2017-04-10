setwd("/Users/juntaeklee/Documents/Projects/weeklyAnalysis/Employment")
source("functions.R")

require(lubridate)
require(Amelia)
require(caret)
require(pROC)
require(doMC)
require(mosaic)
require(ggplot2)
require(rattle)
require(corrplot)
require(dplyr)

#ASSUMPTION
#Those who are retired are unemployed

#read data and change variable types accordingly
#cleaned <- read.csv("data/cleaned.csv")
#emp <- data.frame(apply(cleaned[,1:(dim(cleaned)[2]-3)],2,as.factor))
#emp$weight <- cleaned$weight
#emp$age <- as.integer(cleaned$age)
#emp$empl <- as.factor(cleaned$empl)
#
#set.seed(179)
#tIndex <- createDataPartition(emp$empl,p=0.7,list=F)
#train <- emp[tIndex,]
#test <- emp[-tIndex,]
#
#registerDoMC(cores = 3)
#myControl <- trainControl(method = "repeatedcv",
#                          repeats = 3,number = 10)

##GBM
#model01 <- train(empl~.-psraid, data = train,
#                 method = 'gbm',
#                 trControl = myControl,
#                 #tuneGrid = gbmGrid,
#                 na.action = na.exclude,
#                 nTrain = round(0.75*dim(train)[1]))
#pred01 <- predict(model01,test[,-which(colnames(train)=="empl")],type='prob')[,1]
#roc01 <- roc(test$empl,pred01)
#varImp(model01)

#for whatever reason, refusing to answer job1 is the best indicator. let's remove it.
#train2 <- train[,-c(which(colnames(train)=='job1e'),
#                    which(colnames(train)=='job1a'),
#                    which(colnames(train)=='job1b'),
#                    which(colnames(train)=='job1c'),
#                    which(colnames(train)=='job1f'),
#                    which(colnames(train)=='job1d'))]
#test2 <- test[,-c(which(colnames(test)=='job1e'),
#                   which(colnames(test)=='job1a'),
#                   which(colnames(test)=='job1b'),
#                   which(colnames(test)=='job1c'),
#                   which(colnames(test)=='job1f'),
#                   which(colnames(test)=='job1d'))]
#model011 <- train(empl~.-psraid, data = train2,
#                 method = 'gbm',
#                 trControl = myControl,
#                 na.action = na.exclude,
#                 train.fraction = 0.5)
#pred011 <- predict(model011,test2[,-which(colnames(train)=="empl")],type='prob')[,1]
#roc011 <- roc(test$empl,pred011)
#varImp(model011)
##########################

#Naive Bayes
#model02 <- train(empl~., data = train,
#                 method = 'nb',
#                 trControl = myControl,
#                 na.action = na.exclude)
#pred02 <- predict(model02,test[,-which(colnames(train)=="empl")],type='prob')[,1]
#roc02 <- roc(test$empl,pred02)
#varImp(model02)

#CART
#model03 <- train(empl~.-psraid, data = train,
#                 method = 'rpart',
#                 trControl = myControl,
#                 #tuneGrid = gbmGrid,
#                 na.action = na.exclude)
#pred03 <- predict(model03,test[,-which(colnames(train)=="empl")],type='prob')[,1]
#roc03 <- roc(test$empl,pred03)
#varImp(model03)

##############################
#model01 shows that weights are important, so we use weighted data
#UPDATE: then the training data can see the test data

#read data and change variable types accordingly
cleaned <- read.csv("data/cleaned.csv")
empw <- data.frame(apply(cleaned[,1:(dim(cleaned)[2]-2)],2,as.factor))
empw$age <- as.integer(cleaned$age)
empw$empl <- as.factor(cleaned$empl)

set.seed(179)
tIndexw <- createDataPartition(empw$empl,p=0.7,list=F)
trainw <- empw[tIndexw,]
testw <- empw[-tIndexw,]

registerDoMC(cores = 3)
myControl <- trainControl(method = "repeatedcv",
                          repeats = 3,number = 10)

#GBM
model04 <- train(empl~.-psraid-state, data = trainw,
                 method = 'gbm',
                 trControl = myControl,
                 #tuneGrid = gbmGrid,
                 na.action = na.exclude,
                 nTrain = round(0.75*dim(trainw)[1]))
pred04 <- predict(model04,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc04 <- roc(testw$empl,pred04)
v04 <- varImp(model04)

saveRDS(model04, "model04.rds")
model04 <- readRDS("model04.rds")

#rpart
model05 <- train(empl~.-psraid-state, data = trainw,
                 method = 'rpart',
                 trControl = myControl,
                 na.action = na.exclude)
pred05 <- predict(model05,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc05 <- roc(testw$empl,pred05)
v05 <- varImp(model05)

saveRDS(model05, "model05.rds")
model05 <- readRDS("model05.rds")

#logistic regression with penalty
model06 <- train(empl~., data = trainw[,-c(which(colnames(trainw)=='psraid'),
                                           which(colnames(trainw)=='state'))],
                 method = 'plr',
                 trControl = myControl,
                 na.action = na.exclude,
                 preProc = c("center","scale"))
pred06 <- predict(model06,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc06 <- roc(testw$empl,pred06)
v06 <- varImp(model06)

saveRDS(model06, "model06.rds")
model06 <- readRDS("model06.rds")

#bayesian generalized linear model
model07 <- train(empl~., data = trainw[,-c(which(colnames(trainw)=='psraid'),
                                           which(colnames(trainw)=='state'))],
                 method = 'bayesglm',
                 trControl = myControl,
                 na.action = na.exclude,
                 preProc = c("center","scale"))
pred07 <- predict(model07,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc07 <- roc(testw$empl,pred07)
v07 <- varImp(model07)

saveRDS(model07, "model07.rds")
model07 <- readRDS("model07.rds")

#rf
model08 <- train(empl~.-psraid-state, data = trainw,
                 method = 'rf',
                 trControl = myControl,
                 na.action = na.exclude)
fixed08 <- predict(model08,testw[,-which(colnames(testw)=="empl")])
pred08 <- predict(model08,testw[,-which(colnames(testw)=="empl")],type='prob')[,1]
roc08 <- roc(testw$empl,pred08)
v08 <- varImp(model08)
sum(fixed08==testw$empl)/length(fixed08)

saveRDS(model08, "model08.rds")
model08 <- readRDS("model08.rds")

###################################################

#variable importance plot
labels <- c("Disability",
            "Age",
            "Sex",
            "Education",
            "Internet for Jobs",
            "Parental Status",
            "Marital Status",
            "Internet Frequency",
            "Internet Usage")
varImpPlot(model08$finalModel,n.var = 10,
           main = "Variable Importance Plot",pch=16, type=2,
           labels = labels)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(varImpPlot(model08$finalModel,n.var = 10,
                  main = "Variable Importance Plot",pch=16, type=2,
                  labels = labels))

imp <- varImpDF(model08$finalModel,9)
imp$variable <- labels
imp$variable <- factor(imp$variable,levels = rev(labels))

qplot(variable, data=imp, geom="bar", 
      weight=importance, colour = importance,
      xlab = "", ylab="Mean Gini Decrease",) +
    coord_flip() + 
    
    ###################################################

#Model04 is the best model. We explore it more
#NOTE: not anymore

model041 <- train(empl~.-psraid-month-cregion-state-
                      q1-intfreq-bbhome3-device1a-
                      game2f-stud-smjob1-snsjob2a-snsjob2b-
                      hisp, data = trainw,
                  method = 'gbm',
                  trControl = myControl,
                  #tuneGrid = gbmGrid,
                  na.action = na.exclude,
                  nTrain = round(0.75*dim(trainw)[1]))
pred041 <- predict(model041,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc041 <- roc(testw$empl,pred041)
v041 <- varImp(model041)
#better model than model04

saveRDS(model041, "model041.rds")
model041 <- readRDS("model041.rds")

plot(model041$finalModel,i.var=60,col='blue')
plot(model041$finalModel,i.var=2,col='blue')
plot(model041$finalModel,i.var=3,col='blue')
plot(model041$finalModel,i.var=4,col='blue')
plot(model041$finalModel,i.var=5,col='blue')
plot(model041$finalModel,i.var=6,col='blue')
plot(model041$finalModel,i.var=7,col='blue')
plot(model041$finalModel,i.var=8,col='blue')
plot(model041$finalModel,i.var=9,col='blue')
plot(model041$finalModel,i.var=10,col='blue')
plot(model041$finalModel,i.var=11,col='blue')
plot(model041$finalModel,i.var=20,col='blue')

#age density plot
ggplot( data = weighted, aes(x = age)) + 
    geom_density(adjust=0.6,alpha=0.3) + 
    #geom_vline(xintercept = 57, colour="blue") + 
    aes(colour=empl,fill=empl) + 
    labs(title="Density Plot of Age",
         x = "Age",
         y = "Density",
         colour = "Employment") + 
    theme(legend.position="right",
          plot.title = element_text(hjust = 0.5,
                                    size = 14,
                                    face = 'bold')) + 
    guides(fill="none")

#disability plot
disa <- weighted$disa[weighted$disa != 9]
disa[disa==1] <- "Disabled"
disa[disa==2] <- "Not Disabled"
emp <- weighted$empl[weighted$disa != 9]
disdatm <- propggplot(emp,disa)
ggplot(disdatm,aes(x = colvar, y = value,fill = rowvar)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous(labels = percent_format()) + 
    labs(title = "Employment Status by Disability",
         x = "",
         y = "Percentage",
         fill = "Employment") + 
    theme(plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 14))
#removed refused becayse there were too few

#sex
sex <- weighted$sex
emp <- weighted$empl
sexdatm <- propggplot(emp,sex)
#Add an id variable for the filled regions
ggplot(sexdatm,aes(x = colvar, y = value,fill = rowvar)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous(labels = percent_format()) + 
    labs(title = "Employment Status by Sex",
         x = "",
         y = "Percentage",
         fill = "Employment") + 
    theme(plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 14))

#females have higher unemployment

#sex & age
ggplot( data = weighted, aes(x = age)) + 
    geom_density(adjust=0.4) + 
    aes(colour=sex,fill=sex,alpha=0.3) + 
    facet_wrap(~empl, ncol=4) + 
    labs(title="Employment Status by Age and Sex",
         x = "Age",
         y = "Density",
         colour = "Sex") + 
    theme(legend.position="top",
          plot.title = element_text(hjust = 0.5,
                                    size = 14,
                                    face = 'bold')) +
    guides(alpha=F, fill = F)
#possible conclusions - 
#younger women have babies so they leave the workforce
#younger women are trying to enter more male-dominated jobs

#explore babies to leave the workforce (female)
fembabies <- weighted[weighted$sex == "Female",]
fembabies$par[fembabies$par==1] <- "Parent"
fembabies$par[fembabies$par==2] <- "Not Parent"
fembabies$par[fembabies$par==9] <- "Don't Know"

ggplot( data = fembabies, aes(x = age)) + 
    geom_histogram(binwidth=3.2, aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
    aes(colour=par,fill=par,alpha=0.3) + 
    facet_wrap(~empl, ncol=2) + 
    labs(title="Women in the Workforce by Parental Status",
         y="Percentage",
         x="Age",
         fill="Parental Status") + 
    theme(legend.position="top",
          plot.title=element_text(hjust = 0.5,
                                  face='bold',
                                  size =14)) +
    guides(alpha=F,colour=F) +
    scale_y_continuous(labels = percent_format())

#explore babies to leave the workforce (male)
malbabies <- weighted[weighted$sex == "Male",]
malbabies$par[malbabies$par==1] <- "Parent"
malbabies$par[malbabies$par==2] <- "Not Parent"
malbabies$par[malbabies$par==9] <- "Don't Know"

ggplot( data = malbabies, aes(x = age)) + 
    geom_histogram(binwidth=3.2, aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
    aes(colour=par,fill=par,alpha=0.3) + 
    facet_wrap(~empl, ncol=2) + 
    labs(title="Men in the Workforce by Parental Status",
         y="Percentage",
         x="Age",
         fill="Parental Status") + 
    theme(legend.position="top",
          plot.title=element_text(hjust = 0.5,
                                  face='bold',
                                  size =14)) +
    guides(alpha=F,colour=F) +
    scale_y_continuous(labels = percent_format())

######################################

#examine smart1 next
smart1 <- weighted$smart1
smart1[smart1==1] <- "Smartphone"
smart1[smart1==2] <- "No Smartphone"
smart1[smart1==9] <- "Refused"
emp <- weighted$empl
smartdatm <- propggplot(emp,smart1)

ggplot(smartdatm,aes(x = colvar, y = value,fill = rowvar)) + 
    geom_bar(position = "fill",stat = "identity") + 
    scale_y_continuous(labels = percent_format()) + 
    scale_x_discrete(limits = c("Smartphone","No Smartphone","Refused")) + 
    labs(title = "Employment Status by Smartphone Ownership",
         x = "",
         y = "Percentage",
         fill = "Employment") + 
    theme(plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 12))

#age & smartphone
smartdf <- weighted[weighted$smart1 != 9,]
smartdf$smart1[smartdf$smart1 == 1] <- "Smartphone"
smartdf$smart1[smartdf$smart1 == 2] <- "No Smartphone"
#smartdf$smart1[smartdf$smart1 == 9] <- "Refused"
ggplot( data = smartdf, aes(x = age)) + 
    geom_histogram(binwidth=3.2, aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
    aes(colour=smart1,fill=smart1,alpha=0.3) + 
    facet_wrap(~empl, ncol=2) + 
    labs(title="Employment Status by Age and Smartphone",
         y="Percentage",
         x="Age",
         fill="Smartphone Ownership") + 
    theme(legend.position="top",
          plot.title=element_text(hjust = 0.5,
                                  face='bold',
                                  size =14)) +
    guides(alpha=F,colour=F) +
    scale_y_continuous(labels = percent_format())
