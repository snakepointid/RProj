#record time
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
CONTRACT_LOAN_WITHDRAW<-loanWithdrawInfo 
#-----------------------------------------------------------------------------------------------------
#preprocess
CONTRACT_LOAN_WITHDRAW[,'lend_tm']<-CONTRACT_LOAN_WITHDRAW$lend_tm%>%str_sub(.,1,10)
#----------------------------------------------------------------------------------------------------- 
#handle date num vars
contra_WithdrawInfo <- dateNumIntFE(CONTRACT_LOAN_WITHDRAW,NULL,'lend_tm','lend_amt','contra_no',HP_DATE_GAPS,'loan_contractWithdraw_')
contra_WithdrawInfo <- dateVarFE(CONTRACT_LOAN_WITHDRAW,contra_WithdrawInfo,'lend_tm','contra_no','loan_contractWithdraw_')
#-----------------------------------------------------------------------------------------------------
custorm_WithdrawInfo<- dateNumIntFE(CONTRACT_LOAN_WITHDRAW,NULL,'lend_tm','lend_amt','union_id',HP_DATE_GAPS,'loan_custormerWithdraw_')
custorm_WithdrawInfo<- dateVarFE(CONTRACT_LOAN_WITHDRAW,custorm_WithdrawInfo,'lend_tm','union_id','loan_custormerWithdraw_')
#-----------------------------------------------------------------------------------------------------
#unique 
CONTRACT_LOAN_WITHDRAW   <- CONTRACT_LOAN_WITHDRAW[,.(union_id=first(union_id),
                                                loan_withdraw_ordUsage_cate=first(ord_usage),
                                                loan_withdraw_contraAmt_money=first(contra_amt)),.(contra_no)]
#-----------------------------------------------------------------------------------------------------
#merge
CONTRACT_LOAN_WITHDRAW <- merge(CONTRACT_LOAN_WITHDRAW,contra_WithdrawInfo,by='contra_no')
CUSTOMER_LOAN_WITHDRAW <- custorm_WithdrawInfo
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CONTRACT_LOAN_WITHDRAW,HP_RESULT_DIR,'contra_no')
batchSaveOBJ(CUSTOMER_LOAN_WITHDRAW,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: loan withdraw info [",time_diff,"secs ]")%>%print
