#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#reorder the data
CUSTOMER_CREDIT_LIVEINFO<-onePerOneRecord(liveInfo,'union_id','create_tm')
#load address info
addressSets <- loadOBJ(PROVINCE_CITY_DISTRICT_INFO,HP_SHARE_DIR)
#extract address
CUSTOMER_CREDIT_LIVEINFO[,'credit_liveInfo_Province_cate']<-CUSTOMER_CREDIT_LIVEINFO$home_addr%>%str_extract(.,addressSets$provinces%>%unique%>%paste(.,collapse='|'))
CUSTOMER_CREDIT_LIVEINFO[,'credit_liveInfo_City_cate']    <-CUSTOMER_CREDIT_LIVEINFO$home_addr%>%str_extract(.,addressSets$cities%>%unique%>%paste(.,collapse='|'))
CUSTOMER_CREDIT_LIVEINFO[,'credit_liveInfo_District_cate']<-CUSTOMER_CREDIT_LIVEINFO$home_addr%>%str_extract(.,addressSets$district%>%unique%>%paste(.,collapse='|'))
#rename
CUSTOMER_CREDIT_LIVEINFO[,'credit_liveInfo_liveNum_cate']      <-CUSTOMER_CREDIT_LIVEINFO$num
CUSTOMER_CREDIT_LIVEINFO[,'credit_liveInfo_liveSituation_cate']<-CUSTOMER_CREDIT_LIVEINFO$living_situation
#drop useless
CUSTOMER_CREDIT_LIVEINFO[,c('num','living_situation','home_addr'):=NULL]
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_LIVEINFO,HP_RESULT_DIR,'create_tm')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: clean credit query living info [",time_diff,"secs ]")%>%print
