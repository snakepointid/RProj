#load requried packages
library(dplyr)
library(stringr)
library(data.table)
library(moments)
library(RODBC)
#-----------------------------------------------------------------------------------------------------
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')
paste("feature construnction batch start at",Sys.time())%>%print
#-----------------------------------------------------------------------------------------------------
#set hyper parameters
HP_LAST_DATE         <- loadOBJ(LOGS,HP_SHARE_DIR)$batch_date%>%max#importance hyper parameter 
HP_NUM_POPappNumbers <- 5
HP_LIMIT_SAMP        <- ""
HP_SAVE_CONTRACT     <- '~/../datashare/LABEL_DATA/CONTRACT/'
HP_SAVE_CUSTOMER     <- '~/../datashare/LABEL_DATA/CUSTOMER/'
HP_SAVE_MACRO        <- '~/../datashare/LABEL_DATA/MACRO/'
HP_RESULT_DIR        <- '~/project/LabelAutoComputeScripts/SHARE/RESULT/'
HP_SHARE_DIR         <- '~/project/LabelAutoComputeScripts/SHARE/DATA/'
HP_SUBMUDULE_TIME    <- T
HP_DATE_GAPS         <- c(7,14,30,90,360,1000)
#------------------------------------------------------------------------------
#handle social related labels
source('~/../changlue.she/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/workFlow.R')
#------------------------------------------------------------------------------
#handle consume relate labels
source('~/../changlue.she/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/workFlow.R')
#------------------------------------------------------------------------------
#handle credit query relate labels
source('~/../changlue.she/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/workFlow.R')
#------------------------------------------------------------------------------
#handle main business relate labels
source('~/../changlue.she/project/LabelAutoComputeScripts/MAINBUSI_MODULE/CODE/workFlow.R')
#------------------------------------------------------------------------------
#save the feature construction Logs
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')
LOGS<-data.table(batch_date=Sys.time()%>%as.Date)
batchSaveOBJ(LOGS,HP_SHARE_DIR,'batch_date')
#-----------------------------------------------------------------------------------------------------
#resave the data into the data share director
source('~/project/LabelAutoComputeScripts/SHARE/CODE/reSaveResult.R')
#-----------------------------------------------------------------------------------------------------
paste("feature construnction batch end as",Sys.time())%>%print