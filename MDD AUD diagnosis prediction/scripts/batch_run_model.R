library(doParallel)
library(readxl)

#debugSource('scripts/function_model_one_outcome.R')
source('scripts/function_model_one_outcome.R')

# idxToClassify=1 means AUDvsHC, 2 means MDDvsHC
batch_run_model <- function  (idxToClassify)
{
  
  
  #######   input area    #######
  
  dataFile <- './scripts/data/tbOrganised_to_classify.csv'
  
  if (idxToClassify==1){
    modalityFile <- './scripts/data/VariableToUse_AUD_all.csv'
    mod <- 'all'
    outcomeName <- 'AUDvsHC'
    
  }else if(idxToClassify==2){
    modalityFile <- './scripts/data/VariableToUse_MDD_all.csv'
    mod <- 'all'
    outcomeName <- 'MDDvsHC'
    
  }
  
  # outcome variable, to be removed from the raw data table before classification
  allOutcome = c('HC',	'AUDvsMDD','AUDvsHC','MDDvsHC')
  
  
  
  MLModel <- "EN_Glmnet" 
  
  PARALLEL <- TRUE
  
  OuterCVFolds=10
  InnerCVFolds=5
  Repeats=10
  
  coVars = c('sex', 'StudySiteLondon','StudySiteBerlin','age_psytools_year') # if no coVars, set it as character(0)
  
  
  ########## start processing #########
  # parallel
  if (PARALLEL) {
    cl <- makePSOCKcluster(7)
    registerDoParallel(cl)
  }
  
  ##### load modalities ####

   dfModality <- read.csv(modalityFile, stringsAsFactors = FALSE)
  
  

  run_model_for_one_outcome (outcomeName, allOutcome, dfModality, mod, MLModel, PARALLEL,
                             inputFile = dataFile , 
                             OuterCVFolds=OuterCVFolds,
                             InnerCVFolds=InnerCVFolds, 
                             Repeats= Repeats, 
                             coVars = coVars) # if no coVars, set it as character(0)

  
  
  # stop parallel
  if (PARALLEL)
  {
    stopCluster(cl)
  }
}

