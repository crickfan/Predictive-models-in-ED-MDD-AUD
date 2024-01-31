
rm(list=ls())

source('scripts/batch_run_model.R')
#debugSource('scripts/batch_run_model.R')


#### input area ####

# Modalities:
# 'all' : selected features + apriori features (PDS, sex, BMI)
# 'fewer': selected features only
ModsToAdd <- c('all','fewer') 

# 'anyDev3New': development of ED symptoms
# 'auditDev': development of AUD symptoms
# 'sdepbandDev': development of MDD symptoms
toClassify <- c('anyDev3New', 'auditDev','sdepbandDev')

N_other_mods <- 8 # number of moralities existing in 'scripts/data/VariableToUseBL' csv file

# specify which mods and outcomes to run
names_mod_to_test <- c('all','fewer') 
names_outcome_to_test <- toClassify


#### start processing ####
n
idx_mod_to_test <- which(ModsToAdd %in% names_mod_to_test )
idx_outcome_to_test <- which (toClassify %in% names_outcome_to_test)

nModality = length(ModsToAdd) + N_other_mods
nOutcome = length(toClassify)

idx_to_test<-NULL

for (m in idx_mod_to_test)
{
  idx_to_test <- c(idx_to_test, m -1 + idx_outcome_to_test * nModality)
}



for (i in idx_to_test)
  
{
  batch_run_model(i)
  
}



