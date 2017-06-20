#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
packagesInfo <-loadDATA('app_name','custorm_id','dt','crawler.app_package_info_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
messageInfo  <-loadDATA('name,phone,sms_time,content','custorm_id','dt','crawler.app_sms_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
callInfo     <-loadDATA('login_name,type_call,num_contact,time_contact,phone_contact','custorm_id','dt','crawler.crawl_mobile_call_record_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: load social related raw datas [",time_diff,"secs ]")%>%print