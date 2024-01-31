library(doParallel)
#library(readxl)

## train models on ED samples, and apply to MDD and AUD

rm(list=ls())

source('scripts/utility_functions.R')

source('scripts/function_model_one_outcome_retrain.R')


baseDir<- 'G:/kings backup/Research/ED_project'


inputFileTest= '../../../AUD_MDD_data/split_groups_AUD_MDD_all_vars/tbOrganised_to_classify.csv'

modalityFileTest = '../../../AUD_MDD_data/organise data all vars/VariableToUse_AUD_MDD.csv'

sourceMod <- 'no-BMI'

modelRootDir <- file.path(baseDir, 'scripts/ESTRA_caret_NAN/results/section_35/')

varsToExludeTest <- 'BMI'# to remove from testing data

# these variables will be removed from test data before model training
allOutcomeTest = c('HC',	'AUDvsMDD','AUDvsHC','MDDvsHC')

coVarsTest = c('sex', 'StudySiteLondon','StudySiteBerlin','age_psytools_year') # if no coVarsTest, set it as character(0)

PARALLEL <- TRUE

covarTrain <-  'age_IA_year'


tbResult<- data.frame()

for (sourceName in c('ANvsCtl', 'BNvsCtl') )
{
  for (outcomeName in c('MDDvsHC', 'AUDvsHC')	)
  {
      one_result<-run_model_for_one_outcome_retrain  (inputFileTest, modalityFileTest,
                                              sourceName, sourceMod, modelRootDir,
                                              outcomeName, allOutcomeTest, coVarsTest,varsToExludeTest,
                                              PARALLEL, covarTrain)

    tbResult<- rbind(tbResult,one_result)
  }
}

fwrite(tbResult, 'Transdiag_ED_model_results_retrained.csv')


