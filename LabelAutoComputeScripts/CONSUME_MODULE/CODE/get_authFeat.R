#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#select subset 
CUSTOMER_CRAWLER_JD_AUTH<-authInfo[,.(crawl_jd_authChannel_cate = first(channel),
                        crawl_jd_financialService_cate=first(finan_serv)),.(custorm_id)] 

#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_AUTH,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get authentication features [",time_diff,"secs ]")%>%print
