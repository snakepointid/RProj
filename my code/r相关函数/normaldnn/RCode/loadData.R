library(data.table)
library(dplyr)
#read date
# maxrows=-1
# action02<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/rawdatas/JData_Action_201602.csv',nrows = maxrows)
# action03<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/rawdatas/JData_Action_201603.csv',nrows = maxrows)
# action04<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/rawdatas/JData_Action_201604.csv',nrows = maxrows)
# #merge
# actionInfo<-rbind(action02,action03,action04)
# rm(action02,action03,action04)
# gc()
# actionInfo[,"time"]<-actionInfo$time%>%substr(.,1,18)
# actionInfo<-actionInfo%>%unique
# sampNum<-actionInfo$user_id%>%length
# samp<-sample(c(1:sampNum),sampNum,replace = F)
# actionInfo<-actionInfo[samp,]
# write.csv(actionInfo,'C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/rawdatas/cleanActionData.csv', row.names = F)
#read clean date
maxrows<-300000
actionInfo<-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/rawdatas/cleanActionData.csv',nrows = maxrows)
userInfo <-fread('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/rawdatas/JData_User_clean.csv',nrows = -1)
actionInfo[,'user_id']<-actionInfo$user_id%>%as.integer
#load functions
source('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/normaldnn/RCode/basicFunc.R')
#data preprocess
source('C:/Users/Administrator.NBJXUEJUN-LI/Desktop/project/RProj/my code/r相关函数/normaldnn/RCode/preprocess.R')

