#
#' load packages	
#' 	
# Load packages	
#library('ggplot2') # visualization	
#library('ggthemes') # visualization	
#library('scales') # visualization	
#library('plyr')
#library('dplyr') # data manipulation
#library('mice') # imputation	
library('caret') # controls cross-validation	
#library('ranger') # for random forest	
#library('e1071')  # for random forest	
#library('ggpubr') # visulization	
#library('Metrics')# evaluate model	
#library('randomForest') # classification algorithm	
library('glmnet')    # elastic net
library('Publish') 	
library('data.table')
#library('readxl')
#library('rio')
library('ROCR')  # plot ROC
library('pROC')	
#library('naivebayes')
#library('xgboost')
library('missForest') # imputation
#library("MatchIt")
library("doParallel")


#library('elasticnet')
#library('h2o')
#h2o.init()

# evalF1 <- function(preds, dtrain) {
#   labels <- getinfo(dtrain, "label")
#   result <- confusionMatrix(preds, labels)
#   return(list(metric = "F1", value = result$byClass[1]))
# }


rm(list = ls())


# if ncol <=2, run complete data analysis. if ncol>2, use missForest
#   Input and output are both matrices even if there is a single feature. Keep the column name. 
#   Each row represents a subject.

deal_with_missing <- function(data, PARALLEL)
{
  colNum <- ncol(data)
  
  if (colNum<=2)
  {
    ## use mean imputation for the corresponding group
    imputed_data <- data
    for(c in 1:colNum)
    {
      groupMeans <- ave(data[, c],  FUN = function(x) mean(x, na.rm = TRUE))
      imputed_data [is.na(data[,c]), c] <- groupMeans[is.na(data[,c])]
    }
    return(imputed_data)
    
    # get complete cases
    # if (colNum==1) {
    #   matNames<- dimnames(data) # get the names of row and column
    #   compData <- data[complete.cases(data)] # it becomes a vector
    #   return ( matrix(compData, byrow = FALSE, dimnames = matNames )  )
    # }else
    # {
    #   return ( data[complete.cases(data),] ) # matrix
    # }
    
    # use missForest
  } else{
    if (PARALLEL)
    {
      listImputed <- missForest(data, parallelize = 'forest')
    }else
    {
      listImputed <- missForest(data, parallelize = 'no')
    }
    imputed_data <- as.matrix(listImputed$ximp)
    return(imputed_data)
  }
  
}


calculate_resid <- function(trainData, testData, coVars)
{
  # input: trainData and testData are matrices
  # If there is no coVars, set it as character(0), which has a length of 0
  # output: list(residTrain, residTest)
  
  if (length(coVars)==0){ # if there is no coVars, return data as they are
    return( list(residTrain= trainData, residTest=testData) )
    
  }else
  {
    
    residTrain <- matrix(data = NA, nrow = nrow(trainData), ncol=ncol(trainData), dimnames = list(rownames(trainData) ,colnames(trainData)) )
    residTest <- matrix(data = NA, nrow = nrow(testData), ncol=ncol(testData), dimnames = list(rownames(testData) ,colnames(testData)) )
    
    for (v in which(!colnames(trainData) %in% coVars)) # skip the column of the covar
    {
      lm_formula<- as.formula(paste0("trainData[,v]", "~", 
                                     paste0(coVars, collapse = "+") ) ) 
      
      regModelTrain <- lm(lm_formula, data=as.data.frame(trainData))
      residTrain[,v] <- regModelTrain$residuals
      
      testCovar <-  setNames(data.frame(testData [,coVars,drop = FALSE]), coVars) # predict() takes in dataframes
      predTest <- predict(object = regModelTrain, newdata = testCovar)
      residTest[,v] <- testData[, v]- predTest
    }
    
    # remove covar columns from data
    colOutIdx <- !colnames(residTrain) %in% coVars
    residTrain <- residTrain[,colOutIdx, drop = FALSE]# drop option to avoid the conversion to an atomic vector if there is only one column left
    residTest <-  residTest[,colOutIdx, drop = FALSE]
    
    return( list(residTrain= residTrain, residTest=residTest) )
  }
  
}

# estimate mean and SD on training data and apply it to testData
scale_data <- function(trainData, testData)
{
  colMean <- colMeans(trainData)
  colSD <- apply(trainData, 2, sd)
  
  colSD[colSD==0] <- 1  # if column sd = 0, revalue it as 1
  
  scaledData <- scale(testData, center = colMean, scale = colSD)
  
  return (scaledData)
  
}

