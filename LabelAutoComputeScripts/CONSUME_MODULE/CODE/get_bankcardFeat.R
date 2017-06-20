#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#set default 
DFT_bankName  <-c('ccb','icbc','abc','boc','citic','cmb','post','smallBank')
DFT_card_type <-c('储蓄卡','信用卡')
#truncate cate
bandcardInfo[,'bank_name']<-ifelse(bandcardInfo$bank_name%in%DFT_bankName,bandcardInfo$bank_name,'smallBank')
#aggregated features
CUSTOMER_CRAWLER_JD_BANK<-bandcardInfo[,getBankFeat(bank_name,tail_num,card_type,owner_name,phone),.(custorm_id)]
featName  <-c('crawl_jd_bandOfccb_num','crawl_jd_bandOficbc_num','crawl_jd_bandOfabc_num','crawl_jd_bandOfboc_num','crawl_jd_bandOfcitic_num'
              ,'crawl_jd_bandOfcmb_num','crawl_jd_bandOfpost_num','crawl_jd_bandOfsmallBanks_num','crawl_jd_creditCard_num','crawl_jd_depositCard_num'
              ,'crawl_jd_mostBank_cate','crawl_jd_mostBank_prop','crawl_jd_mostBank_num','crawl_jd_mostCardType_cate','crawl_jd_mostCardType_prop',
              'crawl_jd_mostCardType_num','crawl_jd_diffCard_num','crawl_jd_diffCardOwner_num','crawl_jd_diffCardPhone_num')
CUSTOMER_CRAWLER_JD_BANK<-splitTheFeature(CUSTOMER_CRAWLER_JD_BANK,'V1',featName)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_BANK,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get bank card features [",time_diff,"secs ]")%>%print
