# Elastic net logistic regression
# nested CV to choose optimal alpha and lambda
# binary outcome, AUC as performance
# sample weights are used to account for group size imbalance

# input:
# data preparation beforehand:
#   use a csv file, remove unnecessary variables beforehand or specify the 'colRemove' below to remove
#   remove spaces in variable names
#   
#   for typical 10-fold CV, use
#     OuterCVFolds=   # outer CV to evaluate performance	
#     InnerCVFolds =  # inner CV to turn parameters	
#     Repeats=         # repeat of CV
#


#
#' load packages	
#' 	
# Load packages	
library('ggplot2') # visualization	
#library('ggthemes') # visualization	
library('scales') # visualization	
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
library('missForest')


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
    
    # use missForest
  } else{
    
    if (PARALLEL)
    {
      listImputed <- missForest(data, parallelize = 'forest')
    }else{
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


run_model_for_one_outcome <- function  (outcomeName, allOutcome, dfModality, mod, MLModel, PARALLEL,
                                        inputFile, OuterCVFolds, InnerCVFolds, Repeats, coVars)
{

#' 	
#' 	
#' #0. Input variables
#' 	

# data("Sonar", package = "mlbench")
 
if (PARALLEL==TRUE)
{
  nthread = 6
}else
{
  nthread = 1
}



#outcomeName= 'bingeDev'	   

resultDir<- paste0( './results/', outcomeName,'/', mod, '/')
# create parent dir
#if (!dir.exists(paste0(resultDir, '..'))) {
  dir.create(paste0( './results/', outcomeName,'/' ))
#}
# create dir
#if (!dir.exists(resultDir)){
  dir.create(resultDir)
#}

resultDir<- paste0( './results/', outcomeName,'/', mod, '/')


#' 	
#' 	
#' #1. Read in data and remove unnecesary columns
#' 	
#' 	
 	
dfRaw <- fread(inputFile, stringsAsFactors = FALSE, data.table=FALSE) # return a data frame

dfRaw$outcome <- dfRaw[[outcomeName]]

dfComplete = dplyr::filter(dfRaw, outcome == 0 | outcome == 1)

#' 	
#' 	
#' #2. Specify outcome measure
#' 	

outcome= dfComplete$outcome

#  columns to remove
colRemove= c('SubjectID', 'outcome')	

rawData = dfComplete[, ! names(dfComplete) %in% c(allOutcome, colRemove)]

# use features that belongs to the modality
if (mod == 'female')
{
  ftNames <- with(dfModality, na.omit(VarName[ ToClassify ==1 ]) )
  rawData <- subset(rawData, select =  ftNames)
  
  # select female only
  idxFemale <- rawData$sex==1
  rawData <- rawData[idxFemale,]
  outcome <- outcome[idxFemale]
  #remove gender from data
  # remove gender as a feature to classify
  dfModality$ToClassify[dfModality$VarName=='sex']<-0
  ftNames <- ftNames[ftNames != 'sex']
  rawData$sex <-NULL
  
} else if (mod == 'male')
{
  ftNames <- with(dfModality, na.omit(VarName[ ToClassify ==1 ]) )
  rawData <- subset(rawData, select =  ftNames)
  
  # select male only
  idxMale <- rawData$sex==0
  rawData <- rawData[idxMale,]
  outcome <- outcome[idxMale]
  #remove gender from data
  # remove gender as a feature to classify
  dfModality$ToClassify[dfModality$VarName=='sex']<-0
  ftNames <- ftNames[ftNames != 'sex']
  rawData$sex <-NULL
  
}else if (mod =='all'){ # else use all features, apriori + selected
  
  ftNames <- with(dfModality, na.omit(VarName[ ToClassify ==1 ]) )
  rawData <- subset(rawData, select =  ftNames)
  
}else if (mod =='fewer'){ # exclude PDS, sex, BMI
  
  ftNames <- with(dfModality, na.omit(VarName[ ToClassify ==1 ]) )
  ftNames <- ftNames[!ftNames %in% c('sex', 'PDS', 'zBMI_BL_age_available')]
  rawData <- subset(rawData, select =  ftNames)
  
}else { # use specific modality
  ftNames <- with(dfModality, na.omit(VarName[Category ==mod & ToClassify ==1 ]) )

  rawData <- subset(rawData, select =  ftNames)
}



if (PARALLEL==TRUE)
{ 
  # missForest requires that the number of parallel cores should not exceed the number of variables
  nthread <- ifelse(length(ftNames) <6, length(ftNames), 6  )
  
  cl <- makePSOCKcluster(nthread)
  registerDoParallel(cl)
}
else
{
  nthread = 1
}


print(outcomeName)
print(mod)
#timestamp(prefix = 'Start ', suffix = '')


#' 	
#' 	
#' 	
#' #3. Cross validation	
#' 	

set.seed(999)	


optimalAlpha = rep(-Inf, OuterCVFolds)	
optimalLambda= rep(-Inf, OuterCVFolds)
Imps=data.frame()
Imps_ROC_single_var= data.frame()

# for ROC ploting
Sensitivites= -Inf
Specs= -Inf

predictedCV=list(predictions=list(), labels=list())
for (r in 1:Repeats*OuterCVFolds){
  predictedCV$predictions[[r]] <- numeric()
  predictedCV$labels[[r]] <- numeric()
}

# save trained models
CVModels<-  vector("list", Repeats*OuterCVFolds)

# CV partitians
cvIdices <- matrix(NA, nrow = length(outcome), ncol = Repeats)

# timer
#start_time <- Sys.time()

i_cv=1 # index of CV
# repeat cv many times
for (r in 1:Repeats)
{
  cat('    repeat = ', r)
  
  cvIdx = createFolds(factor(outcome), k = OuterCVFolds, list = FALSE)
  
  cvIdices[, r] <- cvIdx
  
  # one fold of CV
  for (f in 1:OuterCVFolds)
  {
    testIdx  =  cvIdx == f
    trainIdx = !testIdx
    
    # specify train and test
    testData =  data.frame(rawData[testIdx,])
    colnames(testData) <- ftNames
    
    trainData = data.frame(rawData[trainIdx,])
    colnames(trainData) <- ftNames
    
    testOutcome = outcome[testIdx]
    trainOutcome = outcome[trainIdx]
    
    
    # set up weights due to group imbalance.Calculate weights on training data only
    trainWeights <- rep(NA, length(trainOutcome))
    nCase <- sum(trainOutcome)
    nControl <- sum(trainOutcome==0)
    
    trainWeights[trainOutcome==1] <- length(trainOutcome)/nCase
    trainWeights[trainOutcome==0] <- length(trainOutcome)/nControl
    
   
    # stratify inner CV
    innerCVIndex <- createFolds(factor(trainOutcome), InnerCVFolds, returnTrain = T)
    

    # if there if only one variable, use ordinary GLM without CV
    myControlOneVar <- trainControl(
      method = "none",  # only one model fit to the entire training data
      verboseIter = FALSE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary, 
      selectionFunction = 'best', #'best' or 'oneSE'
      allowParallel= PARALLEL
    )


    # outcome is converted from 0 1 to "X0" anc "X1"
    # if there is one feature, use ordinary GLM
    if (length(ftNames) - length(coVars) < 2)  
    {
      options(warn=-1) # turn off warning for non-integer #successes in a binomial glm
      
      trainData <- as.matrix(trainData)
      testData <- as.matrix(testData)
      
      # impute on training and testing separately
      
      trainData <-deal_with_missing(trainData, PARALLEL)
      testData <- deal_with_missing(testData, PARALLEL)
      
      ##### regress out age from training and testing separtely, and drop coVars ####
      
      lsResid <- calculate_resid (trainData, testData, coVars)
      trainData <- lsResid$residTrain
      testData <- lsResid$residTest
      
      ##### scale training and testing data separately ####
      colMean <- colMeans(trainData)
      colSD <- apply(trainData, 2, sd)
      
      colSD[colSD==0] <- 1  # if column sd = 0, revalue it as 1
      
      trainData <- scale(trainData, center = colMean, scale = colSD)
      
      # Scale the columns in testData using the means and standard deviations from trainData
      testData <- scale(testData, center = colMean, scale = colSD)
      
      # stop if there are missing values
      if (anyNA(trainData)) {
        stop("The trainData contains NA values.")
      }
      
      if (anyNA(testData)) {
        stop("The trainData contains NA values.")
      }
      
      # trim data at 3 and -3
      trainData[trainData >  3] <- 3
      testData[testData <  -3] <- -3
      
      # run cross-validation:
      model_cv <-
        train(
          x = as.matrix(trainData), 
          y = make.names(trainOutcome),
          method = 'glm',
          trControl = myControlOneVar,
          metric = "ROC",
          family = 'binomial',
          weights = trainWeights
        )

    } 
    else {
      # if there are more than 1 feature
      options(warn = 0) # turn warnings back on
      

      #######  Elastic Net ##########
      if (MLModel == 'EN_Glmnet'){
        
        # remove rows with all NA (no information at all for this subject) before imputation
        numCol <- ncol(trainData)
        
        rows_all_NA <- rowSums(is.na(trainData)) == numCol
        trainData <- trainData[!rows_all_NA, ]
        trainOutcome <- trainOutcome[!rows_all_NA]
        
        rows_all_NA <- rowSums(is.na(testData)) == numCol
        testData <- testData[!rows_all_NA, ]
        testOutcome <- testOutcome[!rows_all_NA]
        
        ##### impute training and testing separately ####
        trainData <-deal_with_missing(trainData, PARALLEL)
        testData <- deal_with_missing(testData, PARALLEL)
        
        ##### regress out age from training and testing separately, and drop coVars ####
        
        lsResid <- calculate_resid (trainData, testData, coVars)
        trainData <- lsResid$residTrain
        testData <- lsResid$residTest
        
        
        ##### scale training and testing data separately ####
        colMean <- colMeans(trainData)
        colSD <- apply(trainData, 2, sd)
        
        colSD[colSD==0] <- 1  # if column sd = 0, revalue it as 1
        
        trainData <- scale(trainData, center = colMean, scale = colSD)
        
        # Scale the columns in testData using the means and standard deviations from trainData
        testData <- scale(testData, center = colMean, scale = colSD)
        
        # stop if there are missing values
        if (anyNA(trainData)) {
          stop("The trainData contains NA values.")
        }
        
        if (anyNA(testData)) {
          stop("The trainData contains NA values.")
        }
        
        ##### trim data at 3 and -3 ####
        trainData[trainData >  3] <- 3
        testData[testData <  -3] <- -3
        
        best_auc<- -999
        
        #### manually create inner CV folds ####
        innerCVIdx = createFolds(factor(trainOutcome), k = InnerCVFolds, list = FALSE)
        
        for (alp in seq(1, 0.1, by = -0.1) ) 
        {
          cv_lasso<- cv.glmnet(x = as.matrix(trainData), y = trainOutcome, 
                               weights = trainWeights, # sample weights
                               offset = NULL, 
                               family = "binomial",
                               standardize = FALSE,
                               lambda = NULL, 
                               alpha = alp,
                               type.measure = c("auc"), 
                                
                               foldid = innerCVIdx)
          
          if ( best_auc < max(cv_lasso$cvm [cv_lasso$nzero>0]))
          {
            best_auc<- max(cv_lasso$cvm [cv_lasso$nzero>0]) # ignore the null model
            best_lambda <- max(cv_lasso$lambda[cv_lasso$cvm ==best_auc])# sometimes more than one best lambda is selected
            best_alpha <- alp
            model_cv <- cv_lasso
          }
          
        }
        
        optimalAlpha[i_cv] <- best_alpha
        optimalLambda[i_cv] <- best_lambda
      }

      else {
        stop(paste(MLModel,"ML model does not exist"))
      }
      
    } # end of model training
    
    # # get the final model	
    CVModels [[i_cv]] <- model_cv
    
    ## Get variable importance for this CV fold

    if (length(ftNames)-length(coVars) >= 2 &&  ( MLModel =='EN_Glmnet'))
    {
      imp<- as.matrix(coef(cv_lasso, s = best_lambda ))
      
      imp<- imp[2:nrow(imp), 1] # sometimes more than one imps are returned, weird
      
      if(ncol(Imps > 0) && length(imp)!=ncol(Imps)){
        print(imp)
        stop(paste('length of imp = ',length(imp)))
      }
      
      Imps<- rbind(Imps, imp)
      colnames(Imps) <- names(imp)

      predictedProbs <- predict(cv_lasso, newx = as.matrix(testData), type = "response", s = best_lambda)

  
    }
    else
    {
      imp<- varImp(model_cv, scale = FALSE)
      Imps <- rbind( Imps,t(imp$importance) )	
      # get predicted outcome, probablity values		
      predictedProbs <- predict(model_cv,testData, type = "prob")$X1
    }
      
    ### calcuate ROC for each fold of CV
    predictedCV$labels[[i_cv]] =testOutcome
    predictedCV$predictions[[i_cv]] = as.vector( predictedProbs)
    
    
    i_cv= i_cv+1
  }	 #end of OuterCVFolds
  

  
  print (paste('  Repeat = ',as.character(r),'out of',as.character(Repeats)))
  #timestamp(prefix = '', suffix = '')
  
} # end of all repeats

 
# save all CV indices 
write.table(cvIdices, file = file.path(resultDir, 'CV particians.txt'),row.names=FALSE, col.names=FALSE, sep = '\t' )
 
# save all the models from CV
save(CVModels, file = file.path(resultDir, 'CVModels.Rdata'))

# save the feature importance across all models
meanImp <- data.frame( median_t= sapply(Imps,FUN = median ))

meanImp$Name <- row.names(meanImp)


meanImp$Ratio<-  colMeans(Imps!=0  )

meanImp <- dplyr::arrange(meanImp, desc(median_t))
meanImp <- meanImp[, c(2,1,3)]

write.csv(Imps, file = paste0(resultDir, outcomeName ,' feature importance.csv'))
write.csv(meanImp, row.names = FALSE, file = paste0(resultDir, outcomeName,' median importance.csv'))


if(length(ftNames)-length(coVars)>=2)
{
  # save optimal parameters for each CV fold
  optimalParam <- data.frame(optimalAlpha, optimalLambda)
  write.csv(optimalParam,  file=paste0(resultDir, outcomeName, ' optimal param.csv'))	
  
  png(paste0(resultDir, outcomeName, ' optimal param.png'))
  plot(optimalAlpha,optimalLambda)
  dev.off()
}


# save outcome imbalance
write.table(x = table(outcome),file = paste0(resultDir,outcomeName, ' outcome info.txt'),row.names = FALSE)

# save raw data used and outcome in one excel file
write.csv(x=cbind(rawData, outcome), file = paste0(resultDir,outcomeName, ' raw data used.csv'),row.names=FALSE)




#' 	
#' 	
#' 	
#' #4.1 save predicted values and outcomes
#'

predObj <- prediction(predictedCV$predictions, predictedCV$labels)

# save predicted values and outcome
save(predictedCV, file = paste0(resultDir, 'predictedCV.Rdata'))


# stop parallel
if (PARALLEL ==TRUE)
{
  stopCluster(cl)
}

}



