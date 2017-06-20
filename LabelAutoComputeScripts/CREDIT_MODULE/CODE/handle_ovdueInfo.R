#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
CUSTOMER_CREDIT_OVDUE<-ovdueInfo[,.(union_id)]%>%unique
#preprocess
ovdueInfo[,'ovdue_mon']<-ovdueInfo$ovdue_mon%>%str_sub(.,1,4)
#-----------------------------------------------------------------------------------------------------
#handle cate num vars
DFT<-c("2012","2013","2014","2015","2016")
CUSTOMER_CREDIT_OVDUE<-cateNumIntFE(ovdueInfo,CUSTOMER_CREDIT_OVDUE,'ovdue_mon','ovdue_amt'     ,'union_id','credit','ovdueInfo_AmtIn',DFT)
CUSTOMER_CREDIT_OVDUE<-cateNumIntFE(ovdueInfo,CUSTOMER_CREDIT_OVDUE,'ovdue_mon','ovdue_last_mon','union_id','credit','ovdueInfo_MonIn',DFT)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_OVDUE,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: handle ovdue info [",time_diff,"secs ]")%>%print
