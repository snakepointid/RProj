#user timegap feature construct
timegapSeq<-c(1,3,5,10,30,60,4*60,24*60,5*24*60,7*24*60,30*24*60,Inf)
alldata<-windowFeature(UserActionInfo,timegapSeq) 
 
#join user info
setkey(alldata,user_id)
setkey(userInfo,user_id)
alldata  <-userInfo[alldata]

# setkey(alldata,targwindowday)
# setkey(overallSTA,targwindowday)
# alldata  <-overallSTA[alldata]
#load user embeddings
user_embeddings<-fread('/Users/snakepointid/Documents/project/JDproj/save/UserEmbeddings.csv',header =T )
names(user_embeddings)<-c(paste("user_embed",c(1:100),sep="_"),'user_id')
setkey(alldata,user_id)
setkey(user_embeddings,user_id)
alldata  <-user_embeddings[alldata]
##miss imp
alldata   <- data.frame(alldata,   stringsAsFactors = F)
alldata   <- fillna(alldata,  0,   c("user_id","targwindowday","label"))
source('/Users/snakepointid/Documents/project/JDproj/RCode/constructModel.R')
