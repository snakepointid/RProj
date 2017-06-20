#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#preprocess
loanInfo<-miss_handle(loanInfo,c('loan_prin_bal','loan_remain_term','loan_should_pay_m','loan_ovdue_term','loan_ovdue_amt'),0)
CUSTOMER_CREDIT_LOANINFO<-loanInfo[,.(union_id)]%>%unique
#-----------------------------------------------------------------------------------------------------
#handle category vars
DFT_cateValue<-c('商业银行','信托投资公司','小额信贷公司','消费金融有限公司')
CUSTOMER_CREDIT_LOANINFO<-cateVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_org_type','union_id','credit_loanInfo_OrgType',DFT=DFT_cateValue)

DFT_cateValue<-c('个人消费贷款','个人经营性贷款','其他贷款','农户贷款','个人住房贷款','个人汽车贷款')
CUSTOMER_CREDIT_LOANINFO<-cateVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_type','union_id','credit_loanInfo_Type',DFT=DFT_cateValue)

DFT_cateValue<-c('按月归还','按季归还','不定期归还','一次性归还')
CUSTOMER_CREDIT_LOANINFO<-cateVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_rpy_way'    ,'union_id','credit_loanInfo_RpyWay' ,DFT=DFT_cateValue)

DFT_cateValue<-c('正常','结清','逾期')
CUSTOMER_CREDIT_LOANINFO<-cateVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_acct_status','union_id','credit_loanInfo_AcctStatus',DFT=DFT_cateValue)
 
CUSTOMER_CREDIT_LOANINFO<-cateVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_five_clsfct','union_id','credit_loanInfo_FiveCls'   )
#-----------------------------------------------------------------------------------------------------
#handle num vars
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_amt'         ,'union_id','credit_loanInfo_Amt')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_rpy_term'    ,'union_id','credit_loanInfo_Term')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_prin_bal'    ,'union_id','credit_loanInfo_Principal')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_remain_term' ,'union_id','credit_loanInfo_RemainTerm')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_should_pay_m','union_id','credit_loanInfo_ShouldPayAmt')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_real_pay_m'  ,'union_id','credit_loanInfo_RealPayAmt')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_ovdue_amt'   ,'union_id','credit_loanInfo_OvdueAmt')
CUSTOMER_CREDIT_LOANINFO<-numVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_ovdue_term'  ,'union_id','credit_loanInfo_OvdueTerm')
#-----------------------------------------------------------------------------------------------------
#handle date vars
CUSTOMER_CREDIT_LOANINFO<-dateVarFE(loanInfo,CUSTOMER_CREDIT_LOANINFO,'loan_tm','union_id','credit_loanTime')
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_LOANINFO,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: handle loan info [",time_diff,"secs ]")%>%print
