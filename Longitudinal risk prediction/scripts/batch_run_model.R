library(doParallel)
#library(readxl)
library(data.table)
source('scripts/function_model_one_outcome.R')
#debugSource('scripts/function_model_one_outcome.R')


batch_run_model <- function  (idxToClassify)
{

  dataFile <- './scripts/data/tbOrganised.csv'
  
  numModality = 10
  
  
  MLModel = 'EN_Glmnet'  
  
  PARALLEL <- FALSE
  
  OuterCVFolds=10
  InnerCVFolds=5
  Repeats=10
  
  
  # which modalities to add
  ModsToAdd <- c( 'all','fewer')
  
  # all outcome variables in the data file, to be removed from the raw data table before classification
  allOutcome = c('control',
                 'anyDev',
                 'bingeDev',
                 'purgeDev',
                 'fastDev3',
                 'BOPDev',
                 'auditDev',
                 'sdepbandDev',
                 'anyDev3',
                 'controlNew',
                 'cogDevNew',
                 'debDev3New', 
                 'anyDev3New',
                 'bingeDevNew',
                 'purgeDevNew',
                 'fastDev3New')
  
  # outcome variables of interest, to be used as DV
  toClassify <- c( 'anyDev3New', 'auditDev','sdepbandDev')
  
  coVars = c('c1', 'c2','c3','c4', 'c5', 'c6', 'c7', 'MRI_AGE_BL') # if no coVars, set it as character(0)
  
  
############    start processing  ############

  
  outcomeIdx = idxToClassify %/% numModality
  
  modIdx  =  idxToClassify %% numModality + 1
  
  outcomeName <- toClassify[outcomeIdx]
  
  
  ## load modalities from file
  if (outcomeName =='auditDev')
  {
    modalityFile = './scripts/data/VariableToUseBL_AUD_Dev.csv'
    
  }else if (outcomeName=='sdepbandDev')
  {
    modalityFile = './scripts/data/VariableToUseBL_MDD_Dev.csv'
    
  }else if (outcomeName == 'anyDev3New')
  {
    modalityFile = './scripts/data/VariableToUseBL_DEB_Dev.csv'
    
  }else
  {
    stop("The outcomeName is not in the predefined list")
  }
  
  
  dfModality <- fread(modalityFile, sep = ',')
  
  Modalities <- na.omit(unique(dfModality$Category))
  Modalities <- Modalities[Modalities!=""]
  
  Modalities <- c(ModsToAdd, Modalities)
  
  
  mod = Modalities[modIdx]
  
  
  run_model_for_one_outcome(outcomeName, allOutcome, dfModality, mod, MLModel, PARALLEL,
                            inputFile = dataFile , 
                            OuterCVFolds=OuterCVFolds,
                            InnerCVFolds=InnerCVFolds, 
                            Repeats= Repeats, 
                            coVars = coVars) # if no coVars, set it as character(0)
  



  
}
