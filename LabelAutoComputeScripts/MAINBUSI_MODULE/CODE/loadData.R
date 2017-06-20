#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#read datas
loanWithdrawInfoFields<-'union_id,instal_type,ord_usage,lend_status,lend_tm,contra_amt,lend_amt'
loanRepyInfoFields    <-'union_id,rpy_succ_tm,rpy_amt,cdt_rpy_prin,cdt_rpy_inst,cdt_rpy_penalty_inst'
#-----------------------------------------------------------------------------------------------------
loanWithdrawInfo  <- loadDATA(loanWithdrawInfoFields ,'contra_no','dt','dsst.gdl_loanlend_det',HP_LAST_DATE,HP_LIMIT_SAMP)
loanRepyInfo      <- loadDATA(loanRepyInfoFields ,'contra_no','dt','dsst.gdl_loanrpy_det',HP_LAST_DATE,HP_LIMIT_SAMP)

loanAcctInfo      <- loadDATA('#ALL#' ,'contra_no','dt','dsst.gdl_acct_contra_det',HP_LAST_DATE,HP_LIMIT_SAMP)
loanAcctInfo_add  <- loadDATA('appl_sbm_tm' ,'contra_no','dt','dsst.gdl_aprvadt_appl',HP_LAST_DATE,HP_LIMIT_SAMP,F,'and contra_no is not null')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: load business related raw datas [",time_diff,"secs ]")%>%print

 