#record time
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
CONTRACT_LOAN_REPAY<-loanRepyInfo[contra_no!='NULL'&!is.na(contra_no)] 
#----------------------------------------------------------------------------------------------------- 
#handle date num vars
contra_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,NULL,'rpy_succ_tm','rpy_amt'     ,'contra_no',HP_DATE_GAPS,'loan_contractAmtRepay_')
contra_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,contra_RepayInfo,'rpy_succ_tm','cdt_rpy_prin','contra_no',HP_DATE_GAPS,'loan_contractPrinRepay_')
contra_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,contra_RepayInfo,'rpy_succ_tm','cdt_rpy_inst','contra_no',HP_DATE_GAPS,'loan_contractInstRepay_')
contra_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,contra_RepayInfo,'rpy_succ_tm','cdt_rpy_penalty_inst','contra_no',HP_DATE_GAPS,'loan_contractPenaRepay_')
contra_RepayInfo <- dateVarFE(CONTRACT_LOAN_REPAY,contra_RepayInfo,'rpy_succ_tm','contra_no','loan_contractAmtRepay_')
#-----------------------------------------------------------------------------------------------------
cust_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,NULL,'rpy_succ_tm','rpy_amt'     ,'union_id',HP_DATE_GAPS,'loan_customerAmtRepay_')
cust_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,cust_RepayInfo,'rpy_succ_tm','cdt_rpy_prin','union_id',HP_DATE_GAPS,'loan_customerPrinRepay_')
cust_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,cust_RepayInfo,'rpy_succ_tm','cdt_rpy_inst','union_id',HP_DATE_GAPS,'loan_customerInstRepay_')
cust_RepayInfo <- dateNumIntFE(CONTRACT_LOAN_REPAY,cust_RepayInfo,'rpy_succ_tm','cdt_rpy_penalty_inst','union_id',HP_DATE_GAPS,'loan_customerPenaRepay_')
cust_RepayInfo <- dateVarFE(CONTRACT_LOAN_REPAY,cust_RepayInfo,'rpy_succ_tm','union_id','loan_customerAmtRepay_')
#-----------------------------------------------------------------------------------------------------
#merge
CONTRACT_LOAN_REPAY <- contra_RepayInfo
CUSTOMER_LOAN_REPAY <- cust_RepayInfo
#-----------------------------------------------------------------------------------------------------
 #save datas
batchSaveOBJ(CONTRACT_LOAN_REPAY,HP_RESULT_DIR,'contra_no')
batchSaveOBJ(CUSTOMER_LOAN_REPAY,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: loan Repay info [",time_diff,"secs ]")%>%print
 
