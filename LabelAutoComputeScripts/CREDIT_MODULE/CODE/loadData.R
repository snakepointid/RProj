#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#read datas
negInfoFields    <- "natnlty,birth,edu_highest,marg_status,hreg_addr_prov,hreg_addr_city,hreg_addr_district,
                    large_judg_result, crime_drug_judg_cd,gender_cd"
liveInfoFields   <- "num,home_addr,living_situation"
workInfoFields   <- "num,unit_name,unit_addr,career,unit_inds_cat,title,join_year,emp_position"
loanInfoFields   <- "loan_tm,loan_org_type,loan_org,loan_amt,loan_type,loan_rpy_term,loan_rpy_way,
loan_acct_status,loan_five_clsfct,loan_prin_bal,loan_remain_term,loan_should_pay_m,loan_real_pay_m,
loan_ovdue_term,loan_ovdue_amt,rpt_id,trans_no,last_rpt_tm"
cardInfoFields   <- "ccard_iss_tm,ccard_curr,ccard_lim,acct_status,share_lim,od_lim,
use_lim_6m,should_rpy_m,real_rpy_m"
ovdueInfoFields  <- "ovdue_mon,ovdue_last_mon,ovdue_amt"
houseFinInfoFields<-'hf_area,hf_start_mo,hf_end_mo,hf_status,hf_pay_amt,hf_person_percent,hf_emp_percent,hf_pay_emp'
brSpeInfoFields<-'match_rsn,bad_behavior_area,bad_behavior_flag,short_ovdue_flag,cheat_flag,lost_flag,refuse_flag,executor_flag,valid_flag'

#-----------------------------------------------------------------------------------------------------
negInfo  <- loadDATA(negInfoFields ,'union_id','create_tm','dsst.fdl_aprvcrdt_negat_info'    ,HP_LAST_DATE,HP_LIMIT_SAMP)
liveInfo <- loadDATA(liveInfoFields,'union_id','create_tm','dsst.fdl_aprvcrdt_cust_live_info',HP_LAST_DATE,HP_LIMIT_SAMP)
workInfo <- loadDATA(workInfoFields,'union_id','create_tm','dsst.fdl_aprvcrdt_cust_emp_info' ,HP_LAST_DATE,HP_LIMIT_SAMP)
loanInfo <- loadDATA(loanInfoFields,'union_id','create_tm','dsst.fdl_aprvcrdt_ln_trans_det'  ,HP_LAST_DATE,HP_LIMIT_SAMP)
cardInfo <- loadDATA(cardInfoFields,'union_id','inst_tm','dsst.fdl_aprvcrdt_card_trans_det' ,HP_LAST_DATE,HP_LIMIT_SAMP)
ovdueInfo<- loadDATA(ovdueInfoFields,'union_id','create_tm','dsst.fdl_aprvcrdt_ovdue_rec_det',HP_LAST_DATE,HP_LIMIT_SAMP,T)
hsFinInfo<- loadDATA(houseFinInfoFields,'union_id','create_tm','dsst.fdl_aprvcrdt_hf_ei_det' ,HP_LAST_DATE,HP_LIMIT_SAMP)
brSpeInfo<- loadDATA(brSpeInfoFields,'union_id','create_tm','dsst.fdl_aprvcrdt_br_special'   ,HP_LAST_DATE,HP_LIMIT_SAMP,T)
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: load aprv credit raw datas [",time_diff,"secs ]")%>%print