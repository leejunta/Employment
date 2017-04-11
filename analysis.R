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
require(reshape)
require(ggmosaic)
require(reshape)
require(scales)

##############################

#read data and change variable types accordingly
cleaned <- read.csv("data/cleaned.csv")
emp <- data.frame(apply(cleaned[,1:(dim(cleaned)[2]-2)],2,as.factor))
emp$age <- as.integer(cleaned$age)
emp$empl <- as.factor(cleaned$empl)

weighted <- read.csv("data/weighted.csv")
empw <- data.frame(apply(weighted[,1:(dim(weighted)[2]-2)],2,as.factor))
empw$age <- as.integer(weighted$age)
empw$empl <- as.factor(weighted$empl)

set.seed(179)
tIndex <- createDataPartition(emp$empl,p=0.7,list=F)
train <- emp[tIndex,]
test <- emp[-tIndex,]

registerDoMC(cores = 3)
myControl <- trainControl(method = "repeatedcv",
                          repeats = 3,number = 10)

#GBM
model04 <- train(empl~.-psraid-state, data = train,
                 method = 'gbm',
                 trControl = myControl,
                 #tuneGrid = gbmGrid,
                 na.action = na.exclude,
                 nTrain = round(0.75*dim(train)[1]))
pred04 <- predict(model04,test[,-which(colnames(train)=="empl")],type='prob')[,1]
roc04 <- roc(test$empl,pred04)
v04 <- varImp(model04)

saveRDS(model04, "models/model04.rds")
model04 <- readRDS("models/model04.rds")

#rpart
model05 <- train(empl~.-psraid-state, data = train,
                 method = 'rpart',
                 trControl = myControl,
                 na.action = na.exclude)
pred05 <- predict(model05,test[,-which(colnames(train)=="empl")],type='prob')[,1]
roc05 <- roc(test$empl,pred05)
v05 <- varImp(model05)

saveRDS(model05, "models/model05.rds")
model05 <- readRDS("models/model05.rds")

#logistic regression with penalty
model06 <- train(empl~., data = train[,-c(which(colnames(train)=='psraid'),
                                           which(colnames(train)=='state'))],
                 method = 'plr',
                 trControl = myControl,
                 na.action = na.exclude,
                 preProc = c("center","scale"))
pred06 <- predict(model06,test[,-which(colnames(train)=="empl")],type='prob')[,1]
roc06 <- roc(test$empl,pred06)
v06 <- varImp(model06)

saveRDS(model06, "models/model06.rds")
model06 <- readRDS("models/model06.rds")

#bayesian generalized linear model
model07 <- train(empl~., data = train[,-c(which(colnames(train)=='psraid'),
                                           which(colnames(train)=='state'))],
                 method = 'bayesglm',
                 trControl = myControl,
                 na.action = na.exclude,
                 preProc = c("center","scale"))
pred07 <- predict(model07,test[,-which(colnames(train)=="empl")],type='prob')[,1]
roc07 <- roc(test$empl,pred07)
v07 <- varImp(model07)

saveRDS(model07, "models/model07.rds")
model07 <- readRDS("models/model07.rds")

#rf
model08 <- train(empl~.-psraid-state, data = train,
                 method = 'rf',
                 trControl = myControl,
                 na.action = na.exclude)
fixed08 <- predict(model08,test[,-which(colnames(test)=="empl")])
pred08 <- predict(model08,test[,-which(colnames(test)=="empl")],type='prob')[,1]
roc08 <- roc(test$empl,pred08)
v08 <- varImp(model08)
sum(fixed08==test$empl)/length(fixed08)

saveRDS(model08, "models/model08.rds")
model08 <- readRDS("models/model08.rds")
#this is the best model. WE USE THIS FOR VARIABLE IMPORTANCE


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

imp <- varImpDF(model08$finalModel,9)
imp$variable <- labels
imp$variable <- factor(imp$variable,levels = rev(labels))

ggplot(imp,aes(variable,importance)) + 
    geom_point(col='tomato2',size=3) + 
    geom_segment(aes(x=variable,xend=variable,
                     y=min(importance),
                     yend=max(importance)),
                 linetype = "dashed",
                 size =0.1) +
    coord_flip() + 
    theme(panel.grid.major.y = element_line(size=0.1),
          panel.grid.minor = element_blank()) + 
    labs(title = "Variable Importance Plot",
         y = "Mean Gini Decrease",
         x = "") + 
    theme(plot.title = element_text(hjust=0.5,
                                    face='bold',
                                    size=14))


qplot(variable, data=imp, geom="bar", 
      weight=importance, fill = importance,
      xlab = "", ylab="Mean Gini Decrease (GDM)") +
    coord_flip() +
    labs(title = "Variable Importance Plot",
         fill = "GDM") +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    size = 14))
    
###################################################

#Model04 is the best model. We explore it more
#NOTE: not anymore

model041 <- train(empl~.-psraid-month-cregion-state-
                      q1-intfreq-bbhome3-device1a-
                      game2f-stud-smjob1-snsjob2a-snsjob2b-
                      hisp, data = train,
                  method = 'gbm',
                  trControl = myControl,
                  #tuneGrid = gbmGrid,
                  na.action = na.exclude,
                  nTrain = round(0.75*dim(train)[1]))
pred041 <- predict(model041,test[,-which(colnames(train)=="empl")],type='prob')[,1]
roc041 <- roc(test$empl,pred041)
v041 <- varImp(model041)
#better model than model04

saveRDS(model041, "models/model041.rds")
model041 <- readRDS("models/model041.rds")

#plot(model041$finalModel,i.var=60,col='blue')
#plot(model041$finalModel,i.var=2,col='blue')
#plot(model041$finalModel,i.var=3,col='blue')
#plot(model041$finalModel,i.var=4,col='blue')
#plot(model041$finalModel,i.var=5,col='blue')
#plot(model041$finalModel,i.var=6,col='blue')
#plot(model041$finalModel,i.var=7,col='blue')
#plot(model041$finalModel,i.var=8,col='blue')
#plot(model041$finalModel,i.var=9,col='blue')
#plot(model041$finalModel,i.var=10,col='blue')
#plot(model041$finalModel,i.var=11,col='blue')
#plot(model041$finalModel,i.var=20,col='blue')

#disability plot
disa <- as.vector(empw$disa)
disa[disa=="1"] <- "With Disability"
disa[disa=="2"] <- "Without Disability"
disa[disa=="9"] <- "Refused"
empl <- empw$empl
disdatm <- propggplot(empl,disa)
disdatm$colvar <- factor(disdatm$colvar,levels=c("With Disability","Without Disability","Refused"))

ggplot(data = disdatm) +
    geom_mosaic(aes(weight = value, x = product(rowvar, colvar), 
                    fill=factor(rowvar)), na.rm=TRUE) +
    labs(title = "Employment Status by Disability",
         x = "",
         y = "Percentage",
         fill = "Employment") + 
    theme(plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 14)) +
    scale_fill_brewer(direction=-1,palette=14)

#0.129553 disabled
#0.2859425 employed|disabled
#0.7140575 unemployed|disabled

#0.8677566 able
#0.8297162 employed|able
#0.1702838 unemployed|able

#0.7584851 employed
#0.2415149 unemployed

#for further analysis, we add income to the data
inc <- read.csv("data/income.csv")
inc <- data.frame("Disability" = inc$disa,
                  "Employment" = inc$empl,
                  "Income" = inc$salary)
inc$Income[inc$Income==1] <- "<$10,000"
inc$Income[inc$Income==2] <- "$10,000-$19,999"
inc$Income[inc$Income==3] <- "$20,000-$29,999"
inc$Income[inc$Income==4] <- "$30,000-$39,999"
inc$Income[inc$Income==5] <- "$40,000-$49,999"
inc$Income[inc$Income==6] <- "$50,000-$64,999"
inc$Income[inc$Income==7] <- "$75,000-$99,999"
inc$Income[inc$Income==8] <- "$100,000-$149,999"
inc$Income[inc$Income==9] <- ">$150,000"
inc$Income[inc$Income==10] <- "Refused"
inc$Disability <- as.vector(inc$Disability)
inc$Disability[(inc$Disability==1)] <- "With Disability"
inc$Disability[(inc$Disability==2)] <- "Without Disability"
inc$Disability[(inc$Disability==9)] <- "Refused"
inc$Disability <- factor(inc$Disability, levels=c("With Disability","Without Disability","Refused"))
inc$Income <- factor(inc$Income,levels=c("<$10,000",
                                 "$10,000-$19,999",
                                 "$20,000-$29,999",
                                 "$30,000-$39,999",
                                 "$40,000-$49,999",
                                 "$50,000-$64,999",
                                 "$75,000-$99,999",
                                 "$100,000-$149,999",
                                 ">$150,000",
                                 "Refused"))
inc$Employment <- factor(inc$Employment,levels=c("Employed","Unemployed"))
incdatm <- propggplot(inc$Disability,inc$Income)
incdatm$colvar <- factor(incdatm$colvar,levels=c("<$10,000",
                                             "$10,000-$19,999",
                                             "$20,000-$29,999",
                                             "$30,000-$39,999",
                                             "$40,000-$49,999",
                                             "$50,000-$64,999",
                                             "$75,000-$99,999",
                                             "$100,000-$149,999",
                                             ">$150,000","Refused"))
incdatm$rowvar <- factor(incdatm$rowvar,levels=c("With Disability",
                                                 "Without Disability",
                                                 "Refused"))
ggplot(data = incdatm[order(incdatm$rowvar),]) +
    geom_mosaic(aes(weight = value, x = product(rowvar, colvar), 
                    fill=rowvar), na.rm=TRUE) +
    labs(title = "Income Distribution",
         x = "",
         y = "Percentage",
         fill = "Disability") + 
    theme(legend.position = 'bottom',
          plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 14),
          axis.text.x = element_text(angle = 30,
                                     size = 8)) +
    scale_fill_brewer(direction=-1,palette=2)

###############################

#age
#age density plot
i <- 6
ggplot( data = empw, aes(x = age)) + 
    aes(colour=empl,fill=empl) + 
    geom_area(aes(y = ..count.., fill = empl, alpha =0.4), stat = "bin") +
    labs(title="Frequency Plot of Age",
         x = "Age",
         y = "Frequency",
         fill = "Employment") + 
    theme(legend.position="right",
          plot.title = element_text(hjust = 0.5,
                                    size = 14,
                                    face = 'bold')) + 
    guides(colour="none",alpha="none") + 
    scale_fill_brewer(direction = -1, type='qual',palette=i) + 
    scale_color_brewer(direction = -1,type='qual',palette=i) 
    
#sex
sex <- empw$sex
emp <- empw$empl
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
                                  size = 14)) + 
    coord_flip() + 
    scale_fill_brewer(direction = -1,type = 'qual',palette =4) + 
    scale_color_brewer(direction = -1,type = 'qual',palette =4) 

#females have higher unemployment

#sex & age
j <- 6
ggplot( data = empw[empw$empl=="Unemployed",], aes(x = age)) + 
    geom_area(aes(y=..count..,fill=sex,alpha = 1),stat = "bin") + 
    aes(colour=sex) + 
    #facet_wrap(~empl, ncol=4) + 
    labs(title="Frequency of Unemployed by Age and Sex",
         x = "Age",
         y = "Frequency") + 
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5,
                                    size = 14,
                                    face = 'bold')) +
    guides(colour="none",alpha="none") + 
    scale_fill_brewer(direction = -1, type='qual',palette=j) + 
    scale_color_brewer(direction = -1,type='qual',palette=j) +
    annotate("rect", xmin = 26.3, xmax = 33, ymin = 0, ymax = Inf,
             alpha = .3)

#possible conclusions - 
#younger women have babies so they leave the workforce
#younger women are trying to enter more male-dominated jobs

#explore babies to leave the workforce (female)
babies <- empw[empw$empl=="Unemployed",]
babies$par <- as.vector(babies$par)
babies$par[babies$par==1] <- "Parent"
babies$par[babies$par==2] <- "Not Parent"
babies$par[babies$par==9] <- "Don't Know"

ggplot( data = babies, aes(x = age)) + 
    geom_area(aes(y=..count..,fill=par,alpha = 1),stat = "bin") + 
    aes(colour=par) + 
    facet_wrap(~sex, ncol=4) + 
    labs(title="Unemployment by Age, Sex, and Parental Status",
         x = "Age",
         y = "Frequency",
         fill = "Parental Status") + 
    theme(legend.position="bottom",
          plot.title = element_text(hjust = 0.5,
                                    size = 14,
                                    face = 'bold')) +
    guides(colour="none",alpha="none") + 
    scale_fill_brewer(direction = -1, type='qual',palette=j) + 
    scale_color_brewer(direction = -1,type='qual',palette=j)

######################################

#web1-a: internet for jobs
web1 <- as.vector(empw$web1a)
web1[web1==1] <- "Yes"
web1[web1==2] <- "No"
empl <- empw$empl
web1datm <- propggplot(empl,web1)

ggplot(web1datm) + 
    geom_mosaic(aes(weight = value, x = product(rowvar,colvar),
                    fill = rowvar)) + 
    #scale_y_continuous(labels = percent_format()) + 
    #scale_x_discrete(limits = c("Smartphone","No Smartphone","Refused")) + 
    labs(title = "Employment Status by Internet Use for Job Searches",
         x = "",
         y = "Percentage",
         fill = "Employment") + 
    theme(plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 12))

salary <- read.csv("data/income.csv")
salary <- data.frame("Income" = salary$salary,
                     "InternetJob" = web1)
salary$Income[salary$Income==1] <- "<$10,000"
salary$Income[salary$Income==2] <- "$10,000-$19,999"
salary$Income[salary$Income==3] <- "$20,000-$29,999"
salary$Income[salary$Income==4] <- "$30,000-$39,999"
salary$Income[salary$Income==5] <- "$40,000-$49,999"
salary$Income[salary$Income==6] <- "$50,000-$64,999"
salary$Income[salary$Income==7] <- "$75,000-$99,999"
salary$Income[salary$Income==8] <- "$100,000-$149,999"
salary$Income[salary$Income==9] <- ">$150,000"
salary$Income[salary$Income==10] <- "Refused"
salary$Income <- factor(salary$Income,levels=c("<$10,000",
                                               "$10,000-$19,999",
                                               "$20,000-$29,999",
                                               "$30,000-$39,999",
                                               "$40,000-$49,999",
                                               "$50,000-$64,999",
                                               "$75,000-$99,999",
                                               "$100,000-$149,999",
                                               ">$150,000",
                                               "Refused"))
saldatm <- propggplot(salary$InternetJob,salary$Income)
saldatm$colvar <- factor(saldatm$colvar,levels=c("<$10,000",
                                                 "$10,000-$19,999",
                                                 "$20,000-$29,999",
                                                 "$30,000-$39,999",
                                                 "$40,000-$49,999",
                                                 "$50,000-$64,999",
                                                 "$75,000-$99,999",
                                                 "$100,000-$149,999",
                                                 ">$150,000","Refused"))
saldatm$rowvar <- factor(saldatm$rowvar,levels=c("Yes","No"))

ggplot(saldatm,aes(x=rowvar,value)) +
    geom_bar(aes(fill=colvar), position = "dodge", stat="identity") +
    labs(title = "Income Distribution by Internet Use for Job Search",
         x = "Internet Use for Job Search?",
         y = "Frequency",
         fill = "Income") +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = 'bold',
                                    size = 14))
table(salary)

u#examine smart1 next


smart1 <- as.vector(empw$smart1)
smart1[smart1==1] <- "Smartphone"
smart1[smart1==2] <- "No Smartphone"
smart1[smart1==9] <- "Refused"
empl <- empw$empl
smartdatm <- propggplot(empl,smart1)
smartdatm$colvar <- factor(smartdatm$colvar,levels=c("No Smartphone",
                                              "Smartphone",
                                              "Refused"))

ggplot(smartdatm) + 
    geom_mosaic(aes(weight = value, x = product(rowvar,colvar),
                    fill = rowvar)) + 
    #scale_y_continuous(labels = percent_format()) + 
    #scale_x_discrete(limits = c("Smartphone","No Smartphone","Refused")) + 
    labs(title = "Employment Status by Smartphone Ownership",
         x = "",
         y = "Percentage",
         fill = "Employment") + 
    theme(plot.title=element_text(hjust = 0.5,
                                  face = 'bold',
                                  size = 12))

#age & smartphone
smartdf <- empw[empw$smart1 != 9,]
smartdf$smart1 <- as.vector(smartdf$smart1)
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
