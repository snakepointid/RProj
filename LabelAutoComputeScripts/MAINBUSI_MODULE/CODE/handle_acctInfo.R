# record time -------------------------------------------------------------
start_time<-Sys.time()
# preprocess --------------------------------------------------------------
loanAcctInfo_add[,c('dt'):=NULL]
if(loanAcctInfo_add$contra_no%>%length!=loanAcctInfo_add$contra_no%>%unique%>%length)
  stop("contract number must be unique")
loanAcctInfo<-onePerOneRecord(loanAcctInfo,'contra_no','dt')
CONTRACT_LOAN_ACCT<-merge(loanAcctInfo,loanAcctInfo_add,all.x = T)
# time compute -----------------------------------------------------------
#time truncate and map
CONTRACT_LOAN_ACCT[,'open_acct_tm']<-CONTRACT_LOAN_ACCT$open_acct_tm%>%str_sub(.,1,10)
CONTRACT_LOAN_ACCT[,'appl_sbm_tm'] <-CONTRACT_LOAN_ACCT$appl_sbm_tm %>%str_sub(.,1,10)
CONTRACT_LOAN_ACCT[,'acct_expr_tm']<-CONTRACT_LOAN_ACCT$acct_expr_tm%>%str_sub(.,1,10)
CONTRACT_LOAN_ACCT[,'lend_succ_dt']<-CONTRACT_LOAN_ACCT$lend_succ_dt%>%str_sub(.,1,10)

CONTRACT_LOAN_ACCT[,'open_acct_tm']<-CONTRACT_LOAN_ACCT$open_acct_tm%>%dateMap
CONTRACT_LOAN_ACCT[,'appl_sbm_tm'] <-CONTRACT_LOAN_ACCT$appl_sbm_tm %>%dateMap
CONTRACT_LOAN_ACCT[,'acct_expr_tm']<-CONTRACT_LOAN_ACCT$acct_expr_tm%>%dateMap
CONTRACT_LOAN_ACCT[,'lend_succ_dt']<-CONTRACT_LOAN_ACCT$lend_succ_dt%>%dateMap
#compute time difference
CONTRACT_LOAN_ACCT[,'loan_acctInfo_openAcct_timeDiff']        <-CONTRACT_LOAN_ACCT$open_acct_tm-CONTRACT_LOAN_ACCT$appl_sbm_tm
CONTRACT_LOAN_ACCT[,'loan_acctInfo_remValid_timeDiff']        <-CONTRACT_LOAN_ACCT$acct_expr_tm-Sys.time()%>%as.Date
CONTRACT_LOAN_ACCT[,'loan_acctInfo_firWithdraw2Appl_timeDiff']<-CONTRACT_LOAN_ACCT$lend_succ_dt-CONTRACT_LOAN_ACCT$appl_sbm_tm
CONTRACT_LOAN_ACCT[,'loan_acctInfo_firWithdraw2open_timeDiff']<-CONTRACT_LOAN_ACCT$lend_succ_dt-CONTRACT_LOAN_ACCT$open_acct_tm
# rate compute ------------------------------------------------------------
CONTRACT_LOAN_ACCT[,'loan_acctInfo_balLim_rate'] <-CONTRACT_LOAN_ACCT$curt_bal/CONTRACT_LOAN_ACCT$crdt_lim
CONTRACT_LOAN_ACCT[,'loan_acctInfo_remPrin_diff']<-CONTRACT_LOAN_ACCT$instal_init_prin-CONTRACT_LOAN_ACCT$prin_bal
# rename ------------------------------------------------------------------
CONTRACT_LOAN_ACCT<-rename(CONTRACT_LOAN_ACCT,
        loan_acctInfo_termnTyep_cate           = sub_termn_type    
       ,loan_acctInfo_collabID_cate            = collab_id 
       ,loan_acctInfo_loanPurp_cate            = loan_purp
       ,loan_acctInfo_creditLim_money          = crdt_lim
       ,loan_acctInfo_prodCD_cate              = loan_prod_cd
       ,loan_acctInfo_loanType_cate            = loan_mold
       ,loan_acctInfo_initPrin_money           = instal_init_prin
       ,loan_acctInfo_instalTerm_num           = instal_term
       ,loan_acctInfo_currtTerm_num            = curt_term
       ,loan_acctInfo_yearIterst_rate          = inst_rate
       ,loan_acctInfo_servChar_money           = cust_srv_charge
       ,loan_acctInfo_lifeInsPlan_is           = if_life_insure_plan
       ,loan_acctInfo_preRepay_is              = if_prerpy
       ,loan_acctInfo_contraStat_cate          = contra_status
       ,loan_acctInfo_curActBal_money          = curt_bal
       ,loan_acctInfo_curPriBal_money          = prin_bal
       ,loan_acctInfo_lendSucc_num             = lend_succ_cnt
       ,loan_acctInfo_acctAge_num              = aging_cd
       ,loan_acctInfo_curPaidPrni_money        = curt_paid_prin
       ,loan_acctInfo_curPaidInst_money        = curt_paid_inst
       ,loan_acctInfo_curPaidPena_money        = curt_paid_penalty
       ,loan_acctInfo_curPaidPenaInst_money    = curt_paid_penalty_inst
       ,loan_acctInfo_curPaidOvdueFine_money   = curt_paid_ovdue_fine
       ,loan_acctInfo_curPaidSerCharge_money   = curt_paid_srv_charge
       ,loan_acctInfo_curPaidLifeInsu_money    = curt_paid_life_insure
       ,loan_acctInfo_curPaidWithdrawFee_money = curt_paid_withdraw_fee
       ,loan_acctInfo_curPaidPreRepFee_money   = curt_paid_prerpy_fee
)
CONTRACT_LOAN_ACCT<-select(CONTRACT_LOAN_ACCT,c(contra_no,union_id,starts_with("loan_acct")))
#--------------------------------------------------------------------------
#save datas
batchSaveOBJ(CONTRACT_LOAN_ACCT,HP_RESULT_DIR,'contra_no')
#-------------------------------------------------------------------------- 
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: loan acct info [",time_diff,"secs ]")%>%print