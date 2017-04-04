library(data.table)
library(dplyr)
#read date
maxrows=-1
actionInfo<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/AllActionData.csv',nrows = maxrows)
userInfo<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData_User.csv',encoding="UTF-8")
 
source('~/Documents/project/JDproj/hcmodel/preprocess.R')

