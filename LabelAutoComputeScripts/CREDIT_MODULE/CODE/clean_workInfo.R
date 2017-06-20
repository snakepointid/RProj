#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#reorder the data
CUSTOMER_CREDIT_WORKINFO<-onePerOneRecord(workInfo,'union_id','create_tm')
#-----------------------------------------------------------------------------------------------------
#load address info
addressSets <- loadOBJ(PROVINCE_CITY_DISTRICT_INFO,HP_SHARE_DIR)
#-----------------------------------------------------------------------------------------------------
#extract address
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_Province_cate']<-CUSTOMER_CREDIT_WORKINFO$unit_addr%>%str_extract(.,addressSets$provinces%>%unique%>%paste(.,collapse='|'))
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_City_cate']    <-CUSTOMER_CREDIT_WORKINFO$unit_addr%>%str_extract(.,addressSets$cities%>%unique%>%paste(.,collapse='|'))
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_District_cate']<-CUSTOMER_CREDIT_WORKINFO$unit_addr%>%str_extract(.,addressSets$district%>%unique%>%paste(.,collapse='|'))
#-----------------------------------------------------------------------------------------------------
#extract empUnit type
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_UnitType_cate']<-CUSTOMER_CREDIT_WORKINFO$unit_name%>%str_extract(.,'公司|集团|医院|银行|分行|学院|学校|大学')
#-----------------------------------------------------------------------------------------------------
#rename
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_careerType_cate']      <-CUSTOMER_CREDIT_WORKINFO$career
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_careerPosition_cate']  <-CUSTOMER_CREDIT_WORKINFO$emp_position       
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_industryType_cate']    <-CUSTOMER_CREDIT_WORKINFO$unit_inds_cat
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_workNum_cate']         <-CUSTOMER_CREDIT_WORKINFO$num
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_workEnrYear_date']     <-CUSTOMER_CREDIT_WORKINFO$join_year
CUSTOMER_CREDIT_WORKINFO[,'credit_workInfo_workTitle_date']       <-CUSTOMER_CREDIT_WORKINFO$title
#-----------------------------------------------------------------------------------------------------
#drop useless
CUSTOMER_CREDIT_WORKINFO[,c('num','unit_name','career','unit_inds_cat','title','join_year','unit_addr','emp_position'):=NULL]
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_WORKINFO,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: clean credit query working info [",time_diff,"secs ]")%>%print
