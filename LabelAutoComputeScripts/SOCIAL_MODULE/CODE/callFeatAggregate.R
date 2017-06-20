#record time
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
callInfo<-callInfo[custorm_id!='NULL'&!is.na(custorm_id)] 
callInfo[,'time_contact']<-dateMap(callInfo$time_contact)
callInfo[,'num_contact'] <-callInfo$num_contact%>%as.character%>%as.numeric
#-----------------------------------------------------------------------------------------------------
#orgMapping
PHONE_ORG_INFO  <-loadOBJ(PHONE_ORG_INFO,HP_SHARE_DIR)
callInfo<-merge(callInfo,PHONE_ORG_INFO,by.x='phone_contact',by.y='phone',all.x = T)
#-----------------------------------------------------------------------------------------------------
#custormMapping
PHONE_CUST_INFO<-loadOBJ(PHONE_CUST_INFO,HP_SHARE_DIR)
callInfo<-merge(callInfo,PHONE_CUST_INFO,by.x='phone_contact',by.y='phone',all.x = T)
#----------------------------------------------------------------------------------------------------- 
CUSTORMER_CRAWLER_CALL  <- dateNumIntFE(callInfo,NULL,'time_contact','num_contact','custorm_id',HP_DATE_GAPS,'call_customerContactTime_')
CUSTORMER_CRAWLER_CALL  <- dateVarFE(callInfo,CUSTORMER_CRAWLER_CALL,'time_contact','custorm_id','call_customerContactTime_')

CUSTORMER_CRAWLER_CALL  <- dateNumIntFE(callInfo[type_call ==2],CUSTORMER_CRAWLER_CALL,'time_contact','num_contact','custorm_id',HP_DATE_GAPS,'call_customerCallInTime_')
CUSTORMER_CRAWLER_CALL  <- dateVarFE(callInfo[type_call ==1],CUSTORMER_CRAWLER_CALL,'time_contact','custorm_id','call_customerCallInTime_')
CUSTORMER_CRAWLER_CALL  <- dateNumIntFE(callInfo[type_call ==2],CUSTORMER_CRAWLER_CALL,'time_contact','num_contact','custorm_id',HP_DATE_GAPS,'call_customerCallOutTime_')
CUSTORMER_CRAWLER_CALL  <- dateVarFE(callInfo[type_call ==2],CUSTORMER_CRAWLER_CALL,'time_contact','custorm_id','call_customerCallOutTime_')
#-----------------------------------------------------------------------------------------------------
#cateinfo
CUSTORMER_CRAWLER_CALL  <-cateVarFE(callInfo[!org%>%is.na],CUSTORMER_CRAWLER_CALL,'org','custorm_id','call_contactMostOrg' )
CUSTORMER_CRAWLER_CALL  <-cateVarFE(callInfo[!org%>%is.na&type_call ==1],CUSTORMER_CRAWLER_CALL,'org','custorm_id','call_callInMostOrg' )
CUSTORMER_CRAWLER_CALL  <-cateVarFE(callInfo[!org%>%is.na&type_call ==2],CUSTORMER_CRAWLER_CALL,'org','custorm_id','call_callOutMostOrg' )
#-----------------------------------------------------------------------------------------------------
#cateinfo
CUSTORMER_CRAWLER_CALL  <-cateVarFE(callInfo[!custorm_no%>%is.na],CUSTORMER_CRAWLER_CALL,'org','custorm_id','call_contactMostCust' )
CUSTORMER_CRAWLER_CALL  <-cateVarFE(callInfo[!custorm_no%>%is.na&type_call ==1],CUSTORMER_CRAWLER_CALL,'org','custorm_id','call_callInMostCust' )
CUSTORMER_CRAWLER_CALL  <-cateVarFE(callInfo[!custorm_no%>%is.na&type_call ==2],CUSTORMER_CRAWLER_CALL,'org','custorm_id','call_callOutMostCust' )
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTORMER_CRAWLER_CALL,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: call info [",time_diff,"secs ]")%>%print
