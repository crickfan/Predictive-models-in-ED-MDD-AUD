

rm(list=ls())


#debugSource('scripts/batch_run_model.R')
source('scripts/batch_run_model.R')


# 6 individual modalities, plus all,  no-BMI  = 8 modalities 

numModalities = 8

 for (i in c(numModalities:(numModalities+ numModalities*3 -1)))
 {

    batch_run_model(i)
 }

