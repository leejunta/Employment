#GBM
model04 <- train(empl~.-psraid, data = trainw,
                 method = 'gbm',
                 trControl = myControl,
                 #tuneGrid = gbmGrid,
                 na.action = na.exclude,
                 nTrain = round(0.75*dim(trainw)[1]))
#Stochastic Gradient Boosting 
#4377 samples
#46 predictor
#2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 3940, 3939, 3939, 3938, 3940, 3940, ... 
#Resampling results across tuning parameters:
#    
#    interaction.depth  n.trees  Accuracy   Kappa    
#1                   50      0.8101479  0.5963454
#1                  100      0.8114431  0.5987971
#1                  150      0.8138023  0.6037419
#2                   50      0.8115946  0.5999232
#2                  100      0.8201972  0.6197121
#2                  150      0.8250698  0.6307747
#3                   50      0.8167733  0.6116980
#3                  100      0.8301736  0.6420059
#3                  150      0.8410649  0.6660511
#
#Tuning parameter 'shrinkage' was held constant at a value
#of 0.1
#Tuning parameter 'n.minobsinnode' was held constant
#at a value of 10
#Accuracy was used to select the optimal model using 
#the largest value.
#The final values used for the model were n.trees =
#    150, interaction.depth = 3, shrinkage = 0.1
#and n.minobsinnode = 10. 

#A gradient boosted model with bernoulli loss function.
#150 iterations were performed.
#The best test-set iteration was 36.
#There were 161 predictors of which 27 had non-zero influence.

pred04 <- predict(model04,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc04 <- roc(testw$empl,pred04)
#Call:
#    roc.default(response = testw$empl, predictor = pred04)
#
#Data: pred04 in 775 controls (testw$empl 0) > 1099 cases (testw$empl 1).
#Area under the curve: 0.8952

v04 <- varImp(model04)
#gbm variable importance
#only 20 most important variables shown (out of 161)
#Overall
#age     100.000
#disa2    40.315
#sexMale   9.277
#web1a2    7.184
#smart12   5.978
#educ22    3.273
#auto34    3.236
#educ29    2.719
#party9    2.440
#race2     2.318
#auto33    2.292
#hh14      2.224
#state22   2.184
#game42    2.116
#cable12   1.872
#game3a2   1.606
#hh12      1.451
#q39       1.432
#game3b9   1.390
#educ23    1.322

###################################################
###################################################
###################################################

#rpart
model05 <- train(empl~.-psraid, data = trainw,
                 method = 'rpart',
                 trControl = myControl,
                 na.action = na.exclude)
#CART 
#
#4377 samples
#46 predictor
#2 classes: '0', '1' 
#
#No pre-processing
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 3940, 3939, 3939, 3940, 3939, 3939, ... 
#Resampling results across tuning parameters:
#    
#    cp          Accuracy   Kappa    
#0.00496963  0.8198108  0.6183728
#0.15350635  0.7690972  0.4938386
#0.38818332  0.6547415  0.1884886
#
#Accuracy was used to select the optimal model using 
#the largest value.
#The final value used for the model was cp = 0.00496963. 

pred05 <- predict(model05,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc05 <- roc(testw$empl,pred05)
#Data: pred05 in 775 controls (testw$empl 0) > 1099 cases (testw$empl 1).
#Area under the curve: 0.7759

v05 <- varImp(model05)

#rpart variable importance
#only 20 most important variables shown (out of 161)
#Overall
#disa2      100.00
#age         83.72
#web1a2      54.51
#smart12     47.81
#q52         36.70
#intfreq6    11.08
#eminuse2    11.08
#state39      0.00
#race3        0.00
#device1a9    0.00
#par2         0.00
#educ22       0.00
#state20      0.00
#web1a9       0.00
#`state 8`    0.00
#state47      0.00
#disa9        0.00
#game42       0.00
#`state 2`    0.00
#race2        0.00

###################################################
###################################################
###################################################

#logistic regression with penalty
model06 <- train(empl~., data = trainw[,-which(colnames(trainw)=='psraid')],
                 method = 'plr',
                 trControl = myControl,
                 na.action = na.exclude,
                 preProc = c("center","scale"))
#Penalized Logistic Regression 
#
#4377 samples
#45 predictor
#2 classes: '0', '1' 
#
#Pre-processing: centered (161), scaled (161) 
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 3939, 3940, 3939, 3938, 3940, 3940, ... 
#Resampling results across tuning parameters:
#    
#    lambda  Accuracy   Kappa    
#0e+00         NaN        NaN
#1e-04   0.8180638  0.6201984
#1e-01   0.8178351  0.6197098
#
#Tuning parameter 'cp' was held constant at a value of bic
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were lambda = 1e-04 and cp = bic. 

#Coefficients:
#    Intercept     sample2   monthJune langSpanish    cregion2    cregion3    cregion4     state 2 
#-0.48022    -0.13083     0.02729    -0.28082     0.00177     0.12282     0.02342    -0.07764 
#state 4     state 5     state 6     state 8     state 9     state10     state11     state12 
#-0.01576    -0.01596     0.07906    -0.09794    -0.06372     0.04987     0.06157    -0.17478 
#state13     state15     state16     state17     state18     state19     state20     state21 
#-0.18114    -0.06000    -0.03902     0.11535     0.04697    -0.23336    -0.04981    -0.10665 
#state22     state23     state24     state25     state26     state27     state28     state29 
#0.12838    -0.12079    -0.05951    -0.08521     0.16682    -0.08677     0.01178    -0.01419 
#state30     state31     state32     state33     state34     state35     state36     state37 
#-0.31510     0.13253    -0.01651    -0.96906     0.11318    -0.07760     0.02407    -0.05732 
#state38     state39     state40     state41     state42     state44     state45     state46 
#-0.56471     0.02555    -0.04944    -0.01039    -0.05110     0.02767    -0.05785    -0.06071 
#state47     state48     state49     state50     state51     state53     state54     state55 
#0.05947     0.13298     0.16194     0.10973     0.08930     0.03337     0.01096    -0.05507 
#state56       form2     sexMale         q12         q13         q14         q19         q32 
#0.48984    -0.00901    -0.50004     0.00220     0.00404    -0.07653    -0.02215    -0.05015 
#q33         q34         q39    eminuse2    eminuse9    intfreq2    intfreq3    intfreq4 
#0.00664    -0.00922     0.09504     0.11689     0.28015     0.09045     0.15152     0.10475 
#intfreq5    intfreq6    intfreq9    bbhome32    bbhome39   device1a9     smart12     smart19 
#0.06587     0.11689     0.09338    -0.09630     0.06182     0.02848     0.22746     0.06738 
#cable12     cable19      web1a2      web1a9         q52         q59      date32      date39 
#0.23015     0.13184     0.28379     0.28015    -0.06466     0.02848     0.06150    -0.21015 
#date42      date49      game12      game19     game2a2     game2a3     game2a9     game2b2 
#0.02818    -0.10162    -0.05845     0.35576    -0.10884     0.10722    -0.11723    -0.14274 
#game2b3     game2b9     game2c2     game2c3     game2c9     game2d2     game2d3     game2d9 
#-0.10628    -0.06750    -0.00780     0.18785     0.07663    -0.13185    -0.13390    -0.00006 
#game2e2     game2e3     game2e9     game2f2     game2f3     game2f9     game3a2     game3a9 
#0.00500    -0.07621     0.04242    -0.14754    -0.09674    -0.20386     0.22247     0.24662 
#game3b2     game3b9      game42      game48      game49       stud2       stud3       stud9 
#-0.20316    -0.20528    -0.20402    -0.08431     0.35576    -0.33391    -0.53701     0.50545 
#job22       job28     smjob12     smjob19   snsjob2a2   snsjob2a8   snsjob2a9   snsjob2b2 
#-0.06695     0.04423     0.22141     0.06738     0.06146     0.02015    -0.41422    -0.28576 
#snsjob2b8   snsjob2b9      auto32      auto33      auto34      auto39    marital2    marital6 
#-0.07475    -0.08201    -0.34120    -0.43748    -0.51602    -0.08301     0.04980     0.27548 
#marital9        hh12        hh13        hh14        hh15        hh16        hh19        par2 
#0.05575     0.31804    -0.03717     0.06512    -0.09763    -0.12538     0.01484    -0.06941 
#par9      educ22      educ23      educ29       disa2       disa9      party2      party9 
#-0.01440    -0.12045    -0.18117     0.12028    -0.80280    -0.04038    -0.00298     0.20604 
#ideo2       ideo3       ideo9       hisp2       hisp9       race2       race3       race4 
#0.05195     0.08848     0.10097    -0.19709    -0.16661     0.02473    -0.03845    -0.07256 
#race9         age 
#-0.07531     1.08853 
#
#Null deviance: 5936.92 on 4376 degrees of freedom
#Residual deviance: 3464.23 on 4223.44 degrees of freedom
#Score: deviance + 8.4 * df = 4751.71  

pred06 <- predict(model06,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc06 <- roc(testw$empl,pred06)
#Data: pred06 in 775 controls (testw$empl 0) > 1099 cases (testw$empl 1).
#Area under the curve: 0.8699
v06 <- varImp(model06)
#ROC curve variable importance
#
#only 20 most important variables shown (out of 45)
#
#Importance
#disa         100.00
#sample        89.83
#educ2         83.63
#auto3         82.92
#game3b        82.28
#sex           81.33
#hh1           77.14
#race          64.93
#lang          63.12
#marital       62.52
#snsjob2b      61.80
#ideo          60.85
#cable1        60.23
#form          60.06
#cregion       59.89
#hisp          59.72
#device1a      59.56
#month         59.35
#game4         58.21
#state         57.93

###################################################
###################################################
###################################################

#bayesian generalized linear model
model07 <- train(empl~., data = trainw[,-which(colnames(trainw)=='psraid')],
                 method = 'bayesglm',
                 trControl = myControl,
                 na.action = na.exclude,
                 preProc = c("center","scale"))
#Bayesian Generalized Linear Model 
#
#4377 samples
#45 predictor
#2 classes: '0', '1' 
#
#Pre-processing: centered (161), scaled (161) 
#Resampling: Cross-Validated (10 fold, repeated 3 times) 
#Summary of sample sizes: 3939, 3940, 3939, 3940, 3939, 3939, ... 
#Resampling results:
#    
#    Accuracy   Kappa    
#0.8211118  0.6263663

#Coefficients:
#    (Intercept)      sample2    monthJune  langSpanish     cregion2     cregion3     cregion4    `state 2`  
#0.4703016    0.1293931   -0.0276665    0.2649128    0.0016578   -0.2090754    0.0173136    0.0390051  
#`state 4`    `state 5`    `state 6`    `state 8`    `state 9`      state10      state11      state12  
#-0.0211694    0.0143089   -0.1656692    0.0571183    0.0388961   -0.0400360   -0.0525755    0.1679317  
#state13      state15      state16      state17      state18      state19      state20      state21  
#0.1706738    0.0346678    0.0146174   -0.1459068   -0.0725661    0.2007485    0.0302799    0.1004195  
#state22      state23      state24      state25      state26      state27      state28      state29  
#-0.1251602    0.0957357    0.0549947    0.0581732   -0.1967102    0.0578022   -0.0118367   -0.0142968  
#state30      state31      state32      state33      state34      state35      state36      state37  
#0.2482492   -0.1468158   -0.0029763    0.2545980   -0.1373871    0.0502652   -0.0646183    0.0546188  
#state38      state39      state40      state41      state42      state44      state45      state46  
#0.0930475   -0.0671688    0.0451414   -0.0225759    0.0088580   -0.0285620    0.0487588    0.0350010  
#state47      state48      state49      state50      state51      state53      state54      state55  
#-0.0596142   -0.1345101   -0.1839277   -0.1084693   -0.0874997   -0.0777131   -0.0094905    0.0259521  
#state56        form2      sexMale          q12          q13          q14          q19          q32  
#-0.0557162    0.0065765    0.4930353    0.0007181   -0.0092910    0.0765577    0.0151791    0.0514340  
#q33          q34          q39     eminuse2     eminuse9     intfreq2     intfreq3     intfreq4  
#-0.0057582    0.0128384   -0.0875050   -0.1137778   -0.0032843   -0.0860598   -0.1478874   -0.1027078  
#intfreq5     intfreq6     intfreq9     bbhome32     bbhome39    device1a9      smart12      smart19  
#-0.0661211   -0.1137778   -0.0830512    0.0921925   -0.0558940   -0.0274085   -0.2234790   -0.0656340  
#cable12      cable19       web1a2       web1a9          q52          q59       date32       date39  
#-0.2233276   -0.0766272   -0.2777316   -0.0032843    0.0588915   -0.0274085   -0.0584595    0.1740405  
#date42       date49       game12       game19      game2a2      game2a3      game2a9      game2b2  
#-0.0284049    0.0977153    0.0621303   -0.0845240    0.1066650   -0.1017597    0.1109432    0.1431706  
#game2b3      game2b9      game2c2      game2c3      game2c9      game2d2      game2d3      game2d9  
#0.1019522    0.0666814    0.0014855   -0.1916868   -0.0827932    0.1333501    0.1359141   -0.0008800  
#game2e2      game2e3      game2e9      game2f2      game2f3      game2f9      game3a2      game3a9  
#-0.0029830    0.0748893   -0.0420984    0.1456790    0.0926162    0.2040079   -0.2154322   -0.2439722  
#game3b2      game3b9       game42       game48       game49        stud2        stud3        stud9  
#0.1983028    0.2032708    0.1982825    0.0820650   -0.0845240    0.3201476    0.5269504   -0.0795376  
#job22        job28      smjob12      smjob19    snsjob2a2    snsjob2a8    snsjob2a9    snsjob2b2  
#0.0644647   -0.0429518   -0.2196620   -0.0656340   -0.0589622   -0.0178853    0.0570219    0.2430608  
#snsjob2b8    snsjob2b9       auto32       auto33       auto34       auto39     marital2     marital6  
#0.0508872    0.0680209    0.3310059    0.4278124    0.5006644    0.0760746   -0.0484996   -0.2678505  
#marital9         hh12         hh13         hh14         hh15         hh16         hh19         par2  
#-0.0607727   -0.3118387    0.0376088   -0.0682524    0.0893561    0.1189682   -0.0064645    0.0685020  
#par9       educ22       educ23       educ29        disa2        disa9       party2       party9  
#0.0107227    0.1218199    0.1777929   -0.1127510    0.7878446    0.0388504    0.0020376   -0.1970653  
#ideo2        ideo3        ideo9        hisp2        hisp9        race2        race3        race4  
#-0.0543334   -0.0869551   -0.0969226    0.1814896    0.1578982   -0.0276071    0.0446247    0.0631739  
#race9          age  
#0.0746099   -1.0683024  
#
#Degrees of Freedom: 4376 Total (i.e. Null);  4215 Residual
#Null Deviance:	    5937 
#Residual Deviance: 3470 	AIC: 3794

pred07 <- predict(model07,testw[,-which(colnames(trainw)=="empl")],type='prob')[,1]
roc07 <- roc(testw$empl,pred07)
#Data: pred07 in 775 controls (testw$empl 0) > 1099 cases (testw$empl 1).
#Area under the curve: 0.872
#v07 <- varImp(model07)
#ROC curve variable importance
#
#only 20 most important variables shown (out of 45)
#
#Importance
#disa         100.00
#sample        89.83
#educ2         83.63
#auto3         82.92
#game3b        82.28
#sex           81.33
#hh1           77.14
#race          64.93
#lang          63.12
#marital       62.52
#snsjob2b      61.80
#ideo          60.85
#cable1        60.23
#form          60.06
#cregion       59.89
#hisp          59.72
#device1a      59.56
#month         59.35
#game4         58.21
#state         57.93



