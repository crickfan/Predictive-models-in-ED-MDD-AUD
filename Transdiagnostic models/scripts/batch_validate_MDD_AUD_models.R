library(doParallel)
#library(readxl)


## train models on MDD and AUD samples, and apply to AN and BN

rm(list=ls())

source('scripts/utility_functions.R')

source('scripts/function_model_one_outcome_retrain.R')


baseDir<- 'G:/kings backup/Research/ED_project'



inputFileTest= '../../../ESTRA_data/split groups/tbOrganised_add_label.csv'

modalityFileTest = '../../../ESTRA_data/organise data/VariableToUseESTRA.csv'

sourceMod <- 'all'

modelRootDir <- file.path(baseDir, 'scripts/AUD_MDD_caret/results/section_10')

# vars to exclude on the testing data, because they were excluded due to high missingness
varsToExludeTest <- c(
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

# these variables will be removed from test data before model training
allOutcome = c('Control',	'ANvsCtl','BNvsCtl','ANvsBN', 'AN_BN','BPvsR','RvsBP' ,'patientgroup', 'MININegGirlsLondon') 

coVarsTest = c('age_IA_year')

PARALLEL <- TRUE

# STRATIFY coVarsTest
covarTrain <-c('sex', 'StudySiteLondon','StudySiteBerlin','age_psytools_year')

tbResult<- data.frame()

for (sourceName in c('MDDvsHC', 'AUDvsHC' ) )
{
  for (outcomeName in c('ANvsCtl', 'BNvsCtl')	)
  {

      one_result<-run_model_for_one_outcome_retrain  (inputFileTest, modalityFileTest,
                                              sourceName, sourceMod, modelRootDir,
                                              outcomeName, allOutcome, coVarsTest, varsToExludeTest,
                                              PARALLEL, covarTrain )
      
      
    tbResult<- rbind(tbResult,one_result)
  }
}

fwrite(tbResult, 'Transdiag_model_results_validate_MDD_AUD_retrain.csv')


