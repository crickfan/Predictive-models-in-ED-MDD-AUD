


run_model_for_one_outcome_retrain <- function  (inputFileTest, modalityFileTest,
                                         sorceName, sourceMod, modelRootDir,
                                         outcomeName, allOutcomeTest, coVarsTest, varsToExludeTest,
                                         PARALLEL, covarTrain)
{
    # coVarsTest: if there is none, set it as character(0)
  
  #' 	
  #' 	
  #' #0. Input variables
  #' 	
  
  # inputFileTest= '../../AUD_MDD_data/organise data all vars/AUD_MDD_organised.csv'
  # 
  # modalityFileTest = '../../AUD_MDD_data/organise data all vars/VariableToUse_AUD_MDD.csv'
  dfModality <-   dfModality <- read.csv(modalityFileTest, as.is = TRUE)
  

  resultDir<- paste0( './results-retrain/', paste0(sourceName, ' as source'),'/', outcomeName, '/')
  
 

  modelDir <- file.path(modelRootDir, sourceName, sourceMod)
  
  optimalParaFile<- file.path(modelRootDir, sourceName, sourceMod, paste0(sourceName,' optimal param.csv') )
  
  
  
  # create dir
  if (!dir.exists(resultDir)){
    dir.create(resultDir, recursive = TRUE)
  }
  
  
   	
  #1.  read data and filter cases and controls for this anlysis ----
   	
   	
  dfRaw <- fread(inputFileTest, stringsAsFactors = FALSE, data.table=FALSE) # return a data frame
  
  dfRaw$outcome <- dfRaw[[outcomeName]]
  
  dfRaw <- subset(dfRaw, outcome == 0 | outcome == 1)
  
  optimalParams <- read.csv(optimalParaFile)
  
  	
  #2. remove unnecessary outcome vars and columns ----
   	
  
  outcome=  dfRaw$outcome
  
  #  columns to remove
  colRemove= c('Usercode', 'outcome')	
  
  
  rawData = dfRaw[, !colnames(dfRaw) %in% c(allOutcomeTest, colRemove)]
  
  
  ## check if all columns are numeric ----
  non_numeric_columns <- colnames(rawData)[!sapply(rawData, is.numeric)]
  
  # If non-numeric columns are found, return an error message
  if (length(non_numeric_columns) > 0) {
    stop("The following columns are not numeric: ", paste(non_numeric_columns, collapse = ", "))
  }
  
  
  ## select features that belongs to the modality ----
  
  
    ftNames <- with(dfModality, na.omit(VarName[ToClassify == 1]))
      
   ##### exclude variables (here BMI for ED analysis) ####
    ftNames <- ftNames [!ftNames %in% varsToExludeTest]
  
    rawData <- subset(rawData, select = ftNames)
    
  
  if (PARALLEL==TRUE)
  { 
    # missForest requires that the number of parallel cores should not exceed the number of variables
    nthread <- ifelse(length(ftNames) <6, length(ftNames), 6  )
    
    cl <- makePSOCKcluster(nthread)
    registerDoParallel(cl)
  }else
  {
    nthread = 1
  }
  
  print(paste('source:',sourceName))
    
  print(paste('outcome:', outcomeName))
  #timestamp(prefix = 'Start ', suffix = '')
  
   nCase <- sum(outcome)
   nControl <- sum(outcome==0)

  
  #' 	
  #' 	
  #' 	
  # 3. model training and testing ----	
  #' 	
  
  set.seed(999)	
  
  
  #i_cv=1 # index of CV
  
  
  
  # specify train and test
  testData <- rawData
  
  
  ##### remove depression and alcohol related variables ####
  if (outcomeName=='MDDvsHC')
  {
    testData[, 'depband']<-0
    testData[, 'semotion']<-0
    
    # exclude due to excessive missing 
    varsToSetZero <- c(
      'PerceptualReasoning',
      'VerbalComprehension',
      'CGTDelayaversion',
      'CGTDeliberationtime',
      'CGTOverallproportionbet',
      'CGTQualityofdecisionmaking',
      'CGTRiskadjustment',
      'CGTRisktaking',
      'SWMBetweenerrors',
      'SWMStrategy',
      'TotalEA',
      'TotalPA',
      'TotalEN',
      'TotalPN'
    )
    testData[, varsToSetZero] <-0

  }
  
  if (outcomeName=='AUDvsHC')
  {
    testData[, 'audit_total']<-0
    
    # exclude due to excessive missing
    varsToSetZero <- c(
      'PerceptualReasoning',
      'VerbalComprehension',
      'CGTDelayaversion',
      'CGTDeliberationtime',
      'CGTOverallproportionbet',
      'CGTQualityofdecisionmaking',
      'CGTRiskadjustment',
      'CGTRisktaking',
      'SWMBetweenerrors',
      'SWMStrategy',
      'TotalEA',
      'TotalPA',
      'TotalEN',
      'TotalPN'
    )
    testData[, varsToSetZero] <-0
    
  }
  
  
  if (sourceName=='MDDvsHC') # because depband and semotion were not involved in MDD analysis
  {
    testData$depband <- NULL
    testData$semotion <- NULL
    
  }
  
  if (sourceName =='AUDvsHC') # because autit_total was not involved in AUDvsHC analysis
  {
    testData$audit_total <- NULL
  }
  
  if (outcomeName %in% c('ANvsCtl', 'BNvsCtl'))
  {
    testData$BMI <- 0
  }
  
  
  ##### impute training and testing separately ####
  testData <- deal_with_missing(testData, PARALLEL)
  
  
  ##### regress out age from training and testing separtely, and drop coVarsTest ####
  
  lsResid <- calculate_resid (testData, testData, coVarsTest)
  testData <- lsResid$residTrain
  
  ##### scale testing data  ####
  
  # Scale the columns in testData
  
  testData <- scale_data(testData, testData)
  
  if (anyNA(testData)) {
    stop("The trainData contains NA values.")
  }
  
  ##### trim test data at 3 and -3 ####
  testData[testData <  -3] <- -3
  testData[testData >   3] <- 3
  
  ##### train model using training data ####
  trainData <- read.csv(file.path(modelDir, paste0(sourceName,' raw data used.csv')))
  
  trainOutcome <- trainData$outcome
  
  trainData$outcome <- NULL # remove outcome column
  # training data: fill missing
  set.seed(999)  # set the seed otherwise the imputated values were different each time
  
  trainData <- deal_with_missing(trainData, PARALLEL)
  
  #fwrite(x=data.frame(cbind(trainData, trainOutcome)), file = paste0(resultDir, 'imputed training data used.csv'),row.names=FALSE )
  
  # training data: get 
  lsResid <- calculate_resid (trainData, trainData, covarTrain)
  trainData <- lsResid$residTrain
  # scale data
  trainData <- scale_data(trainData, trainData)
  # trim data
  trainData[trainData <  -3] <- -3
  trainData[trainData >   3] <- 3
  
  # save a copy of processed training data
  fwrite(x=data.frame(cbind(trainData, trainOutcome)), file = paste0(resultDir, 'processed training data used.csv'),row.names=FALSE)
  
  #### check if train data match test data####
  if (any(colnames(trainData) != colnames(testData)))
  {
    stop(paste0(sourceName, ' and ', outcomeName,  ' column names do not match'))
  }
  
  # load beast alpha and lambda
  tbParam <- read.csv(file.path(modelDir, paste0(sourceName, ' optimal param.csv')))
  # get geometric median value of alpha and lambda
  
  alpha_to_use <- median(tbParam$optimalAlpha)
  lambda_to_use <- median(tbParam$optimalLambda[tbParam$optimalAlpha==alpha_to_use])
  
  
  #### train model ####
  #set.seed(999) # if not set the seed, beta values in the EN model are slightly different each time even training on the same data
  
  EN_model<- glmnet(x=trainData, y=trainOutcome,
                    family = 'binomial',
                    alpha = alpha_to_use, 
                    lambda = lambda_to_use, 
                    standardize = FALSE)
  
  predictedProbs <- predict(EN_model, newx = as.matrix(testData), type = "response")
  
  # save model
  save(EN_model, file = paste0(resultDir, 'EN_model_trained.RData'))
  # save predicted values
  fwrite(x=data.frame(predictedProbs, outcome), file = paste0(resultDir, 'predictedProbs.csv'),row.names=FALSE)
  
  #### get performance on training data ####
  predictedProbs_training <- predict(EN_model, newx = as.matrix(trainData), type = "response")
  
  
  ####  feature importance    ####
  
  
  features <- colnames(trainData)  # remove the last column 'outcome'
  
  if (!all(rownames(EN_model$beta)==features))
  {
    stop('features names of EN_model$beta do not match features')
  }
  
  
  tbFeatureImp<- data.frame(beta = as.vector( EN_model$beta),
                            r_train_data = rep(NA,length(features)) ,
                            P_train_data = rep(NA,length(features))
                            )
  row.names(tbFeatureImp)<- colnames(trainData)
  
  for (feat in features )
  {
    tbFeatureImp[feat, 'r_train_data'] <- cor(trainData[,feat],predictedProbs_training )
    tbFeatureImp[feat, 'P_train_data'] <- cor.test(trainData[,feat], predictedProbs_training)$p.value
    
    
  }
  tbFeatureImp <- tbFeatureImp[order(abs(tbFeatureImp$beta), decreasing = TRUE),]
  write.csv(tbFeatureImp, file = file.path(resultDir, 'feature_importance.csv'))
  
  # 
  # 
  #   
  # ##### save  sample size ####
  write.table(x = table(outcome),file = paste0(resultDir, 'outcome info.txt'),row.names = FALSE)
  # 
  # ##### save raw data used and outcome in one excel file ####
  fwrite(x=data.frame(cbind(testData, outcome)), file = paste0(resultDir, 'processed test data used.csv'),row.names=FALSE)
  
  
  
  
  #4.1 calculate AUC and plot ROC ----
  #' 	https://www.datatechnotes.com/2019/03/how-to-create-roc-curve-in-r.html
  
  predObj <- prediction(predictedProbs, outcome)
  
  # AUC of ROC
  perf_AUC_ROC <- ROCR::performance(predObj, measure="auc")
  AUCs_ROC <- unlist(perf_AUC_ROC@y.values, use.names=FALSE)
  
  print(AUCs_ROC)

  # AUC of PR
  perf_AUC_PR <- ROCR::performance(predObj,measure = 'aucpr')
  AUCs_PR <- unlist(perf_AUC_PR@y.values, use.names=FALSE)
  print(AUCs_PR)
  

  # ROC  no boxplot, if error occurs, do not plot the average curve
  tryCatch(
    expr = {
      perf_ROC <- ROCR::performance(predObj,"tpr","fpr")
      pdf(paste0(resultDir,outcomeName, ' ROC_plot_without_boxpolt.pdf'), width=4, height=4.5)
      plot(perf_ROC,col="grey70",lty=3,xlim=c(0,1),ylim=c(0,1))
      plot(perf_ROC,lwd=3,avg="vertical",add=TRUE)
      abline(a=0 , b=1,  lwd=1, lty=2)
      dev.off()
    },
    error = function(e){
      message(paste('Caught an error! Plot ROC in', outcomeName, mod))
      print(e)
      dev.off()
      
      # plot without average curve
      perf_ROC <- ROCR::performance(predObj,"tpr","fpr")
      pdf(paste0(resultDir,outcomeName, ' ROC_plot_without_boxpolt.pdf'), width=4, height=4.5)
      plot(perf_ROC,col="grey70",lty=3, xlim=c(0,1),ylim=c(0,1))
      #plot(perf_ROC,lwd=3,avg="vertical",add=TRUE)
      abline(a=0 , b=1,  lwd=1, lty=2)
      dev.off()
      
    }
  )
  
  #  PR curve, if error occurs, do not plot the average curve
  tryCatch(
    expr = {
      perf_PRCurve <- ROCR::performance(predObj, measure="prec", x.measure="rec")
      pdf(paste0(resultDir,outcomeName, ' PR_plot.pdf'), width=4, height=4.5)
      plot(perf_PRCurve,col="grey70",lty=3, xlim=c(0,1),ylim=c(0,1))
      plot(perf_PRCurve,lwd=3,avg="vertical",add=TRUE)
      abline(h = nCase/(nCase+nControl), lwd=1, lty=2)
      dev.off()
    },
    error = function(e){
      message(paste('Caught an error! Plot PR in', outcomeName, mod))
      print(e)
      dev.off()
      
      # plot without average curve
      perf_PRCurve <- ROCR::performance(predObj, measure="prec", x.measure="rec")
      pdf(paste0(resultDir,outcomeName, ' PR_plot.pdf'), width=4, height=4.5)
      plot(perf_PRCurve,col="grey70",lty=3, xlim=c(0,1),ylim=c(0,1))
      #plot(perf_PRCurve,lwd=3,avg="vertical",add=TRUE)
      abline(h = nCase/(nCase+nControl), lwd=1, lty=2)
      dev.off()
    }
  )
  
  #### performance on training data ####
  
  predObjTrain <- prediction(predictedProbs_training, trainOutcome)
  # ROC on train data
  perf_AUC_ROC_train <- ROCR::performance(predObjTrain, measure="auc")
  ROC_train <- unlist(perf_AUC_ROC_train@y.values, use.names=FALSE)
  
  # PR on train data
  perf_AUC_PR_train <- ROCR::performance(predObjTrain,measure = 'aucpr')
  PR_train <- unlist(perf_AUC_PR_train@y.values, use.names=FALSE)

  
  #### organse result table ####
  tbResult <-  data.frame(source_model=sourceName,
                          source_modality= sourceMod,
                          test_data=outcomeName,
                          ROC=AUCs_ROC, 
                          PR= AUCs_PR,
                          ROC_train = ROC_train,
                          PR_train = PR_train)
  
  
  # stop parallel
  if (PARALLEL)
  {
    stopCluster(cl)
  }
  
  
  return(tbResult)
  
  
 }
 
 
 