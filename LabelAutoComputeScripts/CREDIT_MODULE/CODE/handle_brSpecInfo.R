#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#preprocess
brSpeInfo<-brSpeInfo[valid_flag==1]
CUSTOMER_CREDIT_BRSPEC<-brSpeInfo[,.(union_id)]%>%unique
#-----------------------------------------------------------------------------------------------------
#handle category vars
DFT_cateValue<-c('0','1','2','N')
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'bad_behavior_flag','union_id','credit_brSpec_badBehFlag',DFT=DFT_cateValue,F)
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'short_ovdue_flag','union_id','credit_brSpec_shortOvdueFlag',DFT=DFT_cateValue,F)
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'cheat_flag','union_id','credit_brSpec_cheatFlag',DFT=DFT_cateValue,F)
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'lost_flag','union_id','credit_brSpec_lostFlag',DFT=DFT_cateValue,F)
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'refuse_flag','union_id','credit_brSpec_refuseFlag',DFT=DFT_cateValue,F)
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'executor_flag','union_id','credit_brSpec_executeFlag',DFT=DFT_cateValue,F)
DFT_cateValue<-c('cell','id')
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'match_rsn','union_id','credit_brSpec_matchRsn',DFT=DFT_cateValue)
DFT_cateValue<-c('bank','court','credit','insurance','p2p','phone')
CUSTOMER_CREDIT_BRSPEC<-cateVarFE(brSpeInfo,CUSTOMER_CREDIT_BRSPEC,'bad_behavior_area','union_id','credit_brSpec_badBehArea',DFT=DFT_cateValue)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_BRSPEC,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: handle bai rong specific info [",time_diff,"secs ]")%>%print
