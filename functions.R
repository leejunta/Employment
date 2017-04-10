library(randomForest)
library(dplyr)
library(ggplot2)
#functions

#duplicate rows
duplicateRow <- function(data,row,n) {
    new <- data.frame()
    for (rep in 1:n) {
        new <- data.frame(rbind(new,data[row,]))
    }
    return(new)
}

weightedDF <- function(data,weights) {
    new <- data.frame()
    for (f in 1:dim(data)[1]) {
        new <- data.frame(rbind(new,duplicateRow(data,f,round(weights[f]))))
    }
    return(new)
}

#get proportions and melt for ggplot
propggplot <- function(rowvar,colvar) {
    freqtab <- table(rowvar,colvar)
    melted <- melt(freqtab)
    return(melted)
}

#get variable importance plot
varImpDF <- function(rf.model,n.var) {
    var_imp <- data.frame(variable=rownames(importance(rf.model)),
                          importance=as.vector(importance(rf.model)))
    var_imp$importance <- var_imp$importance/max(var_imp$importance)*100
    var_imp <- arrange(var_imp,desc(importance))
    var_imp$id <- c(1:length(rownames(importance(rf.model))))
    var_imp <- var_imp[var_imp$id<=n.var,]
    var_imp$id <- NULL
    var_imp$variable <- as.factor(var_imp$variable)
    return(var_imp)
}




