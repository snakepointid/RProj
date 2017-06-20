#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#reorder the data
CUSTOMER_CREDIT_NEGINFO<-onePerOneRecord(negInfo,'union_id','create_tm')
#-----------------------------------------------------------------------------------------------------
#mapping education
CUSTOMER_CREDIT_NEGINFO[,'edu_highest']<-CUSTOMER_CREDIT_NEGINFO$edu_highest%>%str_extract(.,"博士|研究生|大学|大专|技校|高中|中专|初中|小学")
#-----------------------------------------------------------------------------------------------------
#drop
CUSTOMER_CREDIT_NEGINFO[,c('create_tm'):=NULL]
#rename
featName<-c('union_id','credit_basicInfo_nation_cate','credit_basicInfo_birthday_date','credit_basicInfo_higEduDegre_cate','credit_basicInfo_marrStat_cate',
            'credit_basicInfo_hregAddrProv_cate','credit_basicInfo_hregAddrCity_cate','credit_basicInfo_hregAddrDist_cate',
            'credit_basicInfo_largJudgRest_cate','credit_basicInfo_crimeDrugJudg_cate','credit_basicInfo_gender_cate')
names(CUSTOMER_CREDIT_NEGINFO)<-featName
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_NEGINFO,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: clean negtive credit data [",time_diff,"secs ]")%>%print
