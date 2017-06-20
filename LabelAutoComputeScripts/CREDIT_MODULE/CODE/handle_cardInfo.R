#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#preprocess
cardInfo<-miss_handle(cardInfo,c('od_lim','use_lim_6m','should_rpy_m','real_rpy_m'),0)
CUSTOMER_CREDIT_CARDINFO<-cardInfo[,.(union_id)]%>%unique
#-----------------------------------------------------------------------------------------------------
#handle category vars
DFT_cateValue<-c('人民币账户','美元账户')
CUSTOMER_CREDIT_CARDINFO<-cateVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'ccard_curr','union_id','credit_cardInfo_CurrType',DFT=DFT_cateValue)

DFT_cateValue<-c('未激活','正常','销户')
CUSTOMER_CREDIT_CARDINFO<-cateVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'acct_status','union_id','credit_cardInfo_Status',DFT=DFT_cateValue)
#-----------------------------------------------------------------------------------------------------
#handle num vars
CUSTOMER_CREDIT_CARDINFO<-numVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'ccard_lim'   ,'union_id','credit_cardInfo_Lim')
CUSTOMER_CREDIT_CARDINFO<-numVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'share_lim'   ,'union_id','credit_cardInfo_ShareLim')
CUSTOMER_CREDIT_CARDINFO<-numVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'od_lim'      ,'union_id','credit_cardInfo_UsedLim')
CUSTOMER_CREDIT_CARDINFO<-numVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'use_lim_6m'  ,'union_id','credit_cardInfo_6mUsedLim')
CUSTOMER_CREDIT_CARDINFO<-numVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'should_rpy_m','union_id','credit_cardInfo_ShouldRpy')
CUSTOMER_CREDIT_CARDINFO<-numVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'real_rpy_m'  ,'union_id','credit_cardInfo_RealRpy')
#-----------------------------------------------------------------------------------------------------
#handle date vars
CUSTOMER_CREDIT_CARDINFO<-dateVarFE(cardInfo,CUSTOMER_CREDIT_CARDINFO,'ccard_iss_tm','union_id','credit_cardInfo_IssTime')
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CREDIT_CARDINFO,HP_RESULT_DIR,'union_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: handle card info [",time_diff,"secs ]")%>%print
