#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#data type
baiTiaoInfo[,'overdraft']   <-baiTiaoInfo$overdraft%>%as.character%>%as.numeric
baiTiaoInfo[,'quota']       <-baiTiaoInfo$quota%>%as.character%>%as.numeric
baiTiaoInfo[,'credit_score']<-baiTiaoInfo$credit_score%>%as.character%>%as.numeric
#new feature
baiTiaoInfo[,'quotaUsage']  <-baiTiaoInfo$overdraft/(baiTiaoInfo$quota+1)
#aggregated features
CUSTOMER_CRAWLER_JD_CREDIT<-dcast(baiTiaoInfo,custorm_id~.,fun=list(mean,min,max),value.var = c('credit_score','quota','overdraft','quotaUsage'))
names(CUSTOMER_CRAWLER_JD_CREDIT)<-c('custorm_id',
                    'crawl_jd_creditScore_avg','crawl_jd_quota_avg','crawl_jd_overdraft_avg','crawl_jd_quotaUsage_avg',
                    'crawl_jd_creditScore_min','crawl_jd_quota_min','crawl_jd_overdraft_min','crawl_jd_quotaUsage_min',
                    'crawl_jd_creditScore_max','crawl_jd_quota_max','crawl_jd_overdraft_max','crawl_jd_quotaUsage_max')
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_CREDIT,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get bai tiao credit features [",time_diff,"secs ]")%>%print
