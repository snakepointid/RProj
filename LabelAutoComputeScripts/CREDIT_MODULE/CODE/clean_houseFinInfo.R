#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#reorder the data
CUSTOMER_CREDIT_HOUSEFIN<-onePerOneRecord(hsFinInfo,'union_id','create_tm')
#-----------------------------------------------------------------------------------------------------
#load address info
addressSets <- loadOBJ(PROVINCE_CITY_DISTRICT_INFO,HP_SHARE_DIR)
#-----------------------------------------------------------------------------------------------------
#extract address
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payProvince_cate']<-CUSTOMER_CREDIT_HOUSEFIN$hf_area%>%str_extract(.,addressSets$provinces%>%unique%>%paste(.,collapse='|'))
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payCity_cate']    <-CUSTOMER_CREDIT_HOUSEFIN$hf_area%>%str_extract(.,addressSets$cities%>%unique%>%paste(.,collapse='|'))
#-----------------------------------------------------------------------------------------------------
#extract empUnit type
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payEmp_cate']     <-CUSTOMER_CREDIT_HOUSEFIN$hf_pay_emp%>%str_extract(.,'公司|集团|医院|银行|分行|学院|学校|大学')
#-----------------------------------------------------------------------------------------------------
#simple interact compute
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payDuration_diff']<-suppressWarnings((CUSTOMER_CREDIT_HOUSEFIN$hf_end_mo%>%str_sub(.,1,4)%>%as.numeric-CUSTOMER_CREDIT_HOUSEFIN$hf_start_mo%>%str_sub(.,1,4)%>%as.numeric)*12+(CUSTOMER_CREDIT_HOUSEFIN$hf_end_mo%>%str_sub(.,6,7)%>%as.numeric-CUSTOMER_CREDIT_HOUSEFIN$hf_start_mo%>%str_sub(.,6,7)%>%as.numeric))
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_perEmpPay_rate']  <-CUSTOMER_CREDIT_HOUSEFIN$hf_person_percent%>%as.numeric/CUSTOMER_CREDIT_HOUSEFIN$hf_emp_percent%>%as.numeric
#-----------------------------------------------------------------------------------------------------
#rename
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payEnd_date']    <-CUSTOMER_CREDIT_HOUSEFIN$hf_end_mo
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payStart_date']  <-CUSTOMER_CREDIT_HOUSEFIN$hf_start_mo       
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_payStatus_cate'] <-CUSTOMER_CREDIT_HOUSEFIN$hf_status
CUSTOMER_CREDIT_HOUSEFIN[,'credit_houseFin_monthPay_money'] <-CUSTOMER_CREDIT_HOUSEFIN$hf_pay_amt
#-----------------------------------------------------------------------------------------------------
#drop useless
CUSTOMER_CREDIT_HOUSEFIN[,c('create_tm','hf_end_mo','hf_start_mo','hf_status','hf_pay_amt','hf_area','hf_person_percent','hf_emp_percent','hf_pay_emp'):=NULL]
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_HOUSEFIN,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: clean house finance data [",time_diff,"secs ]")%>%print
