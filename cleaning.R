setwd("/Users/juntaeklee/Documents/Projects/weeklyAnalysis/Employment")

require(lubridate)
require(Amelia)
require(caret)

pew <- read.csv("data/June 10-July 12, 2015 - Gaming, Jobs and Broadband - CSV.csv")

#REMOVE RETIRED AND STUDENT
pew <- pew[(pew$empl!=3) & (pew$empl!=7),]

#Change Interview date to just month
month <- sapply(pew$int_date,function(x) substring(x,1,1))
month[month=="6"] <- "June"
month[month=="7"] <- "July"
pew <- data.frame(pew[,1:(which(colnames(pew)=="int_date")-1)],
                   month,
                   pew[,(which(colnames(pew)=="int_date")+1):dim(pew)[2]])

#Change language variables to English and Spanish
pew$lang[pew$lang==1] <- "English"
pew$lang[pew$lang==2] <- "Spanish"

#Don't know what `usr` is
pew$usr <- NULL

#Sex as male or female
pew$sex[pew$sex==1] <- "Male"
pew$sex[pew$sex==2] <- "Female"

#combine dont know and refuse
pew$q1[pew$q1 == 8] <- 9
pew$q3[pew$q3 == 8] <- 9

#combine eminuse and intmob
pew$eminuse[pew$intmob == 1] <- 1
pew$eminuse[pew$eminuse == 8] <- 9
pew$intmob <- NULL

#intfreq is a conditional, but for missing, use 6: no use
pew$intfreq[is.na(pew$intfreq)] <- 6
pew$intfreq[pew$eminuse == 9] <- 9
pew$intfreq[pew$intfreq == 8] <- 9

#Remove conditional variables
pew$home4nw <- NULL
pew$bbhome1 <- NULL
pew$bbhome2 <- NULL

#combine all subquestions in bbhome3 to one question whether there
#is a disadvantage or not
bbhome3 <- sapply(c(1:dim(pew)[1]),function(x) {
    if ((1 %in% c(pew$bbhome3a[x],
                 pew$bbhome3b[x],
                 pew$bbhome3c[x],
                 pew$bbhome3d[x],
                 pew$bbhome3e[x])) |
        (2 %in% c(pew$bbhome3a[x],
                  pew$bbhome3b[x],
                  pew$bbhome3c[x],
                  pew$bbhome3d[x],
                  pew$bbhome3e[x]))) 1
    else if (3 %in% c(pew$bbhome3a[x],
                       pew$bbhome3b[x],
                       pew$bbhome3c[x],
                       pew$bbhome3d[x],
                       pew$bbhome3e[x])) 2
    else 9
})
pew <- data.frame(pew[,1:(which(colnames(pew)=="bbhome3a")-1)],
                  bbhome3,
                  pew[,(which(colnames(pew)=="bbhome3a")+1):dim(pew)[2]])
pew$bbhome3[pew$bbhome3 == 8] <- 9
pew$bbhome3b <- NULL
pew$bbhome3c <- NULL
pew$bbhome3d <- NULL
pew$bbhome3e <- NULL

#smart1a: do you have a smartphone?
pew$smart1[is.na(pew$smart1)] <- pew$device1a[is.na(pew$smart1)]
pew$smart1[pew$smart1 == 8] <- 9
#device1a: do you have a phone?
pew$device1a <- pew$smart1
pew$device1a[(pew$device1a == 1) | (pew$device1a == 2)] <- 1
pew$device1a[pew$device1a == 8] <- 9
pew$device1a[pew$sample == 2] <- 1

pew$q4 <- NULL
pew$bbsmart1 <- NULL
pew$bbsmart2 <- NULL
pew$bbsmart3a <- NULL
pew$bbsmart3b <- NULL
pew$bbsmart3c <- NULL
pew$bbsmart3d <- NULL
pew$bbsmart3e <- NULL
pew$bbsmart3f <- NULL
pew$bbsmart4 <- NULL

pew$cable1[pew$cable1 == 8] <- 9

pew$cable2 <- NULL
pew$cable3a <- NULL
pew$cable3b <- NULL
pew$cable3c <- NULL

#combine act19,act87a,act87b,act137 to do you use internet for job search
web1a <- sapply(c(1:dim(pew)[1]),function(x) {
    if (1 %in% c(pew$act19[x],
                 pew$act87a[x],
                 pew$act87b[x],
                 pew$act137[x])) 1
    else if (2 %in% c(pew$act19[x],
                      pew$act87a[x],
                      pew$act87b[x],
                      pew$act137[x])) 2
    else if (NA %in% c(pew$act19[x],
                      pew$act87a[x],
                      pew$act87b[x],
                      pew$act137[x])) 2
    else 9
})
web1a[pew$eminuse == 9] <- 9
pew <- data.frame(pew[,1:(which(colnames(pew)=="act19")-1)],
                  web1a,
                  pew[,(which(colnames(pew)=="act19")+1):dim(pew)[2]])
pew$web1a[pew$web1a == 8] <- 9
pew$act137 <- NULL
pew$act87a <- NULL
pew$act87b <- NULL

#change q5 to do you use your phone for entertainment
q5 <- sapply(c(1:dim(pew)[1]),function(x) {
    if (1 %in% c(pew$q5a[x],
                 pew$q5b[x],
                 pew$q5c[x],
                 pew$q5d[x],
                 pew$q5e[x],
                 pew$q5f[x])) 1
    else if (2 %in% c(pew$q5a[x],
                      pew$q5b[x],
                      pew$q5c[x],
                      pew$q5d[x],
                      pew$q5e[x],
                      pew$q5f[x])) 2
    else if (NA %in% c(pew$q5a[x],
                      pew$q5b[x],
                      pew$q5c[x],
                      pew$q5d[x],
                      pew$q5e[x],
                      pew$q5f[x])) 2
    else 9
})
q5[pew$device1a == 9] <- 9
pew <- data.frame(pew[,1:(which(colnames(pew)=="q5a")-1)],
                  q5,
                  pew[,(which(colnames(pew)=="q5a")+1):dim(pew)[2]])
pew$q5[pew$q5 == 8] <- 9
pew$q5b <- NULL
pew$q5c <- NULL
pew$q5d <- NULL
pew$q5e <- NULL
pew$q5f <- NULL

pew$date1a <- NULL
pew$date2a <- NULL

#merge date3a and date3b
date3 <- sapply(c(1:dim(pew)[1]),function(x) {
    if (1 %in% c(pew$date3a[x],
                 pew$date3b[x])) 1
    else if (2 %in% c(pew$date3a[x],
                      pew$date3b[x])) 2
    else if (2 %in% c(pew$date3a[x],
                      pew$date3b[x])) 2
    else 9
})
pew <- data.frame(pew[,1:(which(colnames(pew)=="date3a")-1)],
                  date3,
                  pew[,(which(colnames(pew)=="date3a")+1):dim(pew)[2]])
pew$date3[pew$date3 == 8] <- 9
pew$date3b <- NULL

#merge date4 questions to date4
date4 <- sapply(c(1:dim(pew)[1]),function(x) {
    if (1 %in% c(pew$date4a[x],
                 pew$date4b[x],
                 pew$date4e[x])) 1
    else if (1 %in% c(pew$date4c[x],
                      pew$date4d[x],
                      pew$date4f[x])) 2
    else 9
})
pew <- data.frame(pew[,1:(which(colnames(pew)=="date4a")-1)],
                  date4,
                  pew[,(which(colnames(pew)=="date4a")+1):dim(pew)[2]])
pew$date4[pew$date4 == 8] <- 9
pew$date4b <- NULL
pew$date4c <- NULL
pew$date4d <- NULL
pew$date4e <- NULL
pew$date4f <- NULL

#combine dont know and decline
pew$game1[pew$game1 == 8] <- 9
pew$game2a[pew$game2a == 8] <- 9
pew$game2b[pew$game2b == 8] <- 9
pew$game2c[pew$game2c == 8] <- 9
pew$game2d[pew$game2d == 8] <- 9
pew$game2e[pew$game2e == 8] <- 9
pew$game2f[pew$game2f == 8] <- 9

pew$game3a[pew$game3a == 8] <- 9
pew$game3b[pew$game3b == 8] <- 9

#change game4 as gamer, not gamer
pew$game4[is.na(pew$game4)] <- 2
pew$game4[pew$game1 == 8] <- 9



#remove other employment questions
#keep later for salary analysis
pew$emptype1 <- NULL
pew$emptype2 <- NULL
pew$emptype3 <- NULL
pew$job5a <- NULL
pew$job5b <- NULL
pew$job5c <- NULL
pew$job5d <- NULL
pew$auto1a <- NULL
pew$auto1b <- NULL
pew$auto1c <- NULL
pew$auto1d <- NULL
pew$auto1e <- NULL
pew$auto2 <- NULL
pew$inc <- NULL

#change stud to include everyone
pew$stud[is.na(pew$stud)] <- 1

#change job1 to include everyone; UPDATE: correlated with empl
#pew$job1a[is.na(pew$job1a)] <- 9
#pew$job1b[is.na(pew$job1b)] <- 9
#pew$job1c[is.na(pew$job1c)] <- 9
#pew$job1d[is.na(pew$job1d)] <- 9
#pew$job1e[is.na(pew$job1e)] <- 9
#pew$job1f[is.na(pew$job1f)] <- 9

pew$job1a <- NULL
pew$job1b <- NULL
pew$job1c <- NULL
pew$job1d <- NULL
pew$job1e <- NULL
pew$job1f <- NULL

#remove job3&4--too many NAs
pew$job2 <- NULL
pew$job3a <- NULL
pew$job3b <- NULL
pew$job3c <- NULL
pew$job3d <- NULL
pew$job3e <- NULL
pew$job3f <- NULL
pew$job3g <- NULL
pew$job3h <- NULL
pew$job4 <- NULL

pew$smjob1[is.na(pew$smjob1)] <- 2
pew$smjob1[pew$smart1 == 9] <- 9

#remove smjob2s & 3 & 4--too many NA
pew$smjob2a <- NULL
pew$smjob2b <- NULL
pew$smjob2c <- NULL
pew$smjob2d <- NULL
pew$smjob2e <- NULL
pew$smjob3 <- NULL
pew$smjob4a <- NULL
pew$smjob4b <- NULL
pew$smjob4c <- NULL
pew$smjob4d <- NULL
pew$smjob4e <- NULL

#combine snsjob1
#snsjob1 <- sapply(c(1:dim(pew)[1]), function(x) {
#    if (1 %in% c(pew$snsjob1a,
#                 pew$snsjob1b,
#                 pew$snsjob1c)) 1
#    else if (2 %in% c(pew$snsjob1a,
#                      pew$snsjob1b,
#                      pew$snsjob1c)) 2
#    else 9
#})
#pew <- data.frame(pew[,1:(which(colnames(pew)=="snsjob1a")-1)],
#                  snsjob1,
#                  pew[,(which(colnames(pew)=="snsjob1a")+1):dim(pew)[2]])
#all of them have 1s in them, so variance = 0
pew$snsjob1a <- NULL
pew$snsjob1b <- NULL
pew$snsjob1c <- NULL

#snsjob2
pew$snsjob2a[is.na(pew$snsjob2a)] <- 2
pew$snsjob2a[pew$web1a == 9] <- 9
pew$snsjob2b[is.na(pew$snsjob2b)] <- 2
pew$snsjob2b[pew$web1a == 9] <- 9

#auto3
pew$auto3[pew$auto3 == 8] <- 9

#marital status
pew$marital[pew$marital == 2] <- 1
pew$marital[(pew$marital == 3) | 
                (pew$marital == 4) | 
                (pew$marital == 5)] <- 2
pew$marital[pew$marital == 8] <- 9

#house hold size
#change to ordinal maybe?
pew$hh1[pew$hh1 == 8] <- 9

pew$kidage1 <- NULL
pew$kidage2 <- NULL
pew$kidage3 <- NULL

#change education to HS, undergrad, post undergrad
pew$educ2[(pew$educ2 == 2) |
              (pew$educ2 == 3) |
              (pew$educ2 == 4)] <- 1
pew$educ2[(pew$educ2 == 5) |
              (pew$educ2 == 6)] <- 2
pew$educ2[(pew$educ2 == 7) |
              (pew$educ2 == 8)] <- 3
pew$educ2[(pew$educ2 == 98) |
              (pew$educ2 == 99)] <- 9

#EDinst too many NA
pew$edinst <- NULL

pew$disa[pew$disa == 8] <- 9

#Party affiliation
pew$party[(pew$party == 8)] <- 9
pew$party[(pew$partyln == 8) | (pew$partyln == 9)] <- 9
pew$party[(pew$partyln == 1)] <- 1
pew$party[(pew$partyln == 2)] <- 2
pew$partyln <- NULL

#political ideology
pew$ideo[pew$ideo == 2] <- 1
pew$ideo[pew$ideo == 3] <- 2
pew$ideo[(pew$ideo == 4) | (pew$ideo == 5)] <- 3
pew$ideo[pew$ideo == 8] <- 9

#hisp
pew$hisp[pew$hisp == 8] <- 9
pew$birth_hisp <- NULL

#race
pew$race[(pew$race == 5) | (pew$race == 6)] <- 4
pew$race[pew$race == 8] <- 9

#cell phoneuse 
pew$ql1a <- NULL
pew$qc1 <- NULL

pew$zipcode <- NULL

#change emplnw so that 1,2,5 = 1, other = 0

pew$empl[(pew$emplnw == 1) |
               (pew$emplnw == 2) |
               (pew$emplnw == 5)] <- "Employed"
pew$empl[(pew$emplnw != 1) &
               (pew$emplnw != 2) &
               (pew$emplnw != 5)] <- "Unemployed"
pew$emplnw <- NULL

pew$standwt <- NULL

age <- pew$age
age[(pew$age == 98) | (pew$age == 99)] <- NA
weight <- as.numeric(pew$weight)
empl <- as.factor(pew$empl)
pew <- data.frame(apply(pew[,-c(which(colnames(pew)=="weight"),
                                which(colnames(pew)=="age"),
                                which(colnames(pew)=="empl"))],2, as.factor))
pew$weight <- weight
pew$age <- age
pew$empl <- empl

#age: change 98 and 99 to NA to impute later
#imputed!
preProcData <- preProcess(pew, method = 'knnImpute')
pew_imp <- predict(preProcData, pew)
pew$age <- pew_imp$age*sd(pew$age,na.rm = T)+mean(pew$age,na.rm = T)
pew$weight <- NULL
#missmap(pew)
write.csv(pew,"data/cleaned.csv",row.names = F)



################################
#Change the data to take sample weights into account



#cleaned <- read.csv("data/cleaned.csv")
#weighted <- weightedDF(cleaned,cleaned$weight)
#weighted$weight <- NULL

#write.csv(weighted,"data/weighted.csv",row.names=F)

######################################





