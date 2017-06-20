#get app relate features
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load pop apps
popAPP  <- loadOBJ(popAPP,HP_SHARE_DIR)$popAPP
#change data types
packagesInfo$app_name  <-packagesInfo$app_name%>%as.character
packagesInfo$custorm_id<-packagesInfo$custorm_id%>%as.character()
#drop null samples
packagesInfo<-packagesInfo[!is.na(packagesInfo$app_name),]
#norm the characters
packagesInfo$app_name<-packagesInfo$app_name%>%tolower()
#drop duplicated samples
packagesInfo<-unique(packagesInfo)
#--------------------------------------------------------------------------------------------------------------------------
#initial app Dataframe
CUSTORMER_CRAWLER_APP<-data.table(custorm_id=packagesInfo$custorm_id%>%unique)
setkey(CUSTORMER_CRAWLER_APP,custorm_id)
#--------------------------------------------------------------------------------------------------------------------------
#get pop app 
popAppidx       <-packagesInfo$app_name%in%popAPP
popAppInstallDF <-table(c(rep(-1,popAPP%>%length),packagesInfo$custorm_id[popAppidx]),c(popAPP,packagesInfo$app_name[popAppidx]))%>%as.data.frame.matrix
#set feature name
featName  <- popAppInstallDF%>%names
featName  <-paste('app',featName,'has',sep="_")
names(popAppInstallDF)<-featName
#add customer id
popAppInstallDF$custorm_id<-popAppInstallDF%>%row.names%>%as.character
popAppInstallDF<-popAppInstallDF%>%data.table
#merge
setkey(popAppInstallDF,custorm_id)
CUSTORMER_CRAWLER_APP<-popAppInstallDF[CUSTORMER_CRAWLER_APP]
#--------------------------------------------------------------------------------------------------------------------------
#get topic feature
KEYWORDS<-c("银行","信用卡","借|贷","现金|钱包","金融","地图|导航","外卖|饿了","头条|新闻|资讯|报道","阅读|小说|读书"
            ,"知乎|豆瓣|贴吧","直播","优酷|影音|爱奇艺","音乐|电台","邮箱","租房|租车|公寓","药|医|健康|大夫"
            ,"美图|相册","出行|打车")
#get topical pop app
for(KEYWORD in KEYWORDS){
  userInstallTopic <- packagesInfo$custorm_id[str_detect(packagesInfo$app_name, KEYWORD)]%>%table 
  if(userInstallTopic%>%length>0)
    userInstallTopic <-data.table(cid=userInstallTopic%>%names,num=userInstallTopic%>%as.vector,stringsAsFactors = F)else
      userInstallTopic<-data.table(cid='-1',num=0)
    names(userInstallTopic)<-c('custorm_id',paste('crawl_app_',KEYWORD,'installed_num',sep=""))
    setkey(userInstallTopic,custorm_id)
    CUSTORMER_CRAWLER_APP<-userInstallTopic[CUSTORMER_CRAWLER_APP]
}
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTORMER_CRAWLER_APP,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get app labels [",time_diff,"secs ]")%>%print
