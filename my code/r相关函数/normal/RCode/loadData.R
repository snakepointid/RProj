library(data.table)
library(dplyr)
#read date
maxrows=-1
action02<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData/JData_Action_201602.csv',nrows = maxrows)
action03<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData/JData_Action_201603.csv',nrows = maxrows)
action04<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData/JData_Action_201604.csv',nrows = maxrows)
#merge
actionInfo<-rbind(action02,action03,action04)
rm(action02,action03,action04)
gc()
#action[,"time"]<-action$time%>%substr(.,1,18)
actionInfo<-actionInfo%>%unique
 
#read clean date
# maxrows<--1
# actionInfo<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData/cleanActionData.csv',nrows = maxrows)
 
userInfo <-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData/JData_User_clean.csv',nrows = -1)
#load functions
source('/Users/snakepointid/Documents/project/JDproj/RCode/basicFunc.R')
#data preprocess
source('~/Documents/project/JDproj/RCode/preprocess.R')

