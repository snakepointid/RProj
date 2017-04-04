library(data.table)
library(dplyr)
#read date
maxrows=-1
actionInfo<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/AllActionData.csv',nrows = maxrows)
userInfo<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData_UserPreprocess.csv',encoding="UTF-8")

#load functions
source('/Users/snakepointid/Documents/project/JDproj/hcmodel2e/basicFunc.R')
#data preprocess
source('~/Documents/project/JDproj/hcmodel2e/preprocess.R')

