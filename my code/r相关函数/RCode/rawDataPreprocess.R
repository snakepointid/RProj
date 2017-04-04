#user info one hot 
userInfo<-fread('/Users/snakepointid/Documents/project/JDproj/rawdata/JData/JData_User.csv',nrows = maxrows)
userInfo[age=='NULL','age']<-'-1'
ageMP<-userInfo$age%>%unique%>%oneHotEncode
userInfo[,paste("age",c(1:length(ageMP)),sep="_")]<-do.call(rbind,ageMP[userInfo$age%>%as.character]%>%strsplit(.,"#"))%>%data.table
sexMP<-userInfo$sex%>%unique%>%oneHotEncode
userInfo[,paste("sex",sexMP%>%names,sep="_")]<-do.call(rbind,sexMP[userInfo$sex%>%as.character]%>%strsplit(.,"#"))%>%data.table
user_lv_cdMP<-userInfo$user_lv_cd%>%unique%>%oneHotEncode
userInfo[,paste("user_lv_cd",user_lv_cdMP%>%names,sep="_")]<-do.call(rbind,user_lv_cdMP[userInfo$user_lv_cd%>%as.character]%>%strsplit(.,"#"))%>%data.table
 
userInfo[,"user_reg_tm"]<-(as.Date("2017-03-30")-userInfo$user_reg_tm%>%as.Date())%>%as.numeric
userInfo[,c('age',"sex","user_lv_cd"):=NULL]

write.csv(userInfo,'/Users/snakepointid/Documents/project/JDproj/rawdata/JData/JData_User_clean.csv')
