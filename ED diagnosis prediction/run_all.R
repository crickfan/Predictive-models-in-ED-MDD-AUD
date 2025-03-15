

rm(list=ls())


#debugSource('scripts/batch_run_model.R')
source('scripts/batch_run_model.R')


# 8 modalities: all, no-BMI, BMI, Personality, Cognition, Psychopathology, Substance use, Environment

numModalities = 8

 for (i in c(numModalities:(numModalities+ numModalities*3 -1)))
 {

    batch_run_model(i)
 }

