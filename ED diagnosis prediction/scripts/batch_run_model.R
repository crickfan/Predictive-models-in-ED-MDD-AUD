library(doParallel)
#library(readxl)

#debugSource('scripts/function_model_one_outcome.R')
source('scripts/function_model_one_outcome.R')


batch_run_model <- function  (idxToClassify)
{
  
  #######   input area    #######
  
  modalityFile = './scripts/data/VariableToUseESTRA.csv'
  

  # outcome variable, to be removed from the raw data table before classification
  allOutcome = c('Control',	'ANvsCtl','BNvsCtl','ANvsBN', 'AN_BN','BPvsR','RvsBP' ,'patientgroup', 'MININegGirlsLondon')
  
  toClassify <- c('ANvsCtl','BNvsCtl', 'ANvsBN')  
  
  MLModel <- "EN_Glmnet" 
  
  PARALLEL <- TRUE
  
  inputFile= './scripts/data/tbOrganised_add_label.csv'
  
  OuterCVFolds=  10 # outer CV to evaluate performance	
  InnerCVFolds = 5 # inner CV to turn parameters	
  Repeats= 10 # repeat of CV, try 5 or 10 
  
  coVars = 'age_IA_year' #  coVars: if there is none, set it as character(0), whose length is 0
  
  
  ########## start processing #########
  # parallel

  
  # load modalities

  dfModality <- read.csv(modalityFile, as.is = TRUE)
  
  Modalities <- na.omit(unique(dfModality$Category[dfModality$ToClassify==1]))
  Modalities <- Modalities[Modalities!=""]
  Modalities <- c('all','no-BMI',Modalities ) 
  
  numModality = length(Modalities);
  
  
  
  outcomeIdx = idxToClassify %/% numModality;
  
  modIdx  =  idxToClassify %% numModality + 1;
  
  outcomeName <- toClassify[outcomeIdx]
  
  
  mod = Modalities[modIdx]
  run_model_for_one_outcome (outcomeName, allOutcome, dfModality, mod, MLModel, PARALLEL, 
                             inputFile= inputFile, 
                             OuterCVFolds=  OuterCVFolds, # outer CV to evaluate performance	
                             InnerCVFolds = InnerCVFolds, # inner CV to turn parameters	
                             Repeats= Repeats,     # repeat of CV
                             coVars = coVars   # coVars: if there is none, set it as character(0), whose length is 0
                             )


}

