#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load address info
PROVINCE_CITY_DISTRICT_INFO  <- loadOBJ(PROVINCE_CITY_DISTRICT_INFO,HP_SHARE_DIR)
#-----------------------------------------------------------------------------------------------------
#get product price info 
addressInfo[,'province']<-str_extract(addressInfo$region,paste(PROVINCE_CITY_DISTRICT_INFO$provinces,collapse = '|'))%>%unlist
addressInfo[,'city']    <-str_extract(addressInfo$region,paste(PROVINCE_CITY_DISTRICT_INFO$cities,collapse = '|'))%>%unlist
addressInfo[,'xian']    <-str_extract(addressInfo$region,'县')%>%unlist
addressInfo[,'zhen']    <-str_extract(addressInfo$region,'镇')%>%unlist
addressInfo[,'cun']     <-str_extract(addressInfo$region,'村')%>%unlist
#get feat
CUSTOMER_CRAWLER_JD_ADDRESS<-addressInfo[,getAddFeat(receiver,province,city,xian,zhen,cun),.(custorm_id)]
#transfer into data table
featName<-c('crawl_jd_diffReceiver_num','crawl_jd_diffRecProv_num','crawl_jd_diffRecCity_num',
            'crawl_jd_mostRecProv_cate','crawl_jd_mostRecProv_prop','crawl_jd_mostRecProv_num',
            'crawl_jd_mostRecCity_cate','crawl_jd_mostRecCity_prop','crawl_jd_mostRecCity_num',
            'crawl_jd_liveInXian_has','crawl_jd_liveInZhen_has','crawl_jd_liveInCun_has')
CUSTOMER_CRAWLER_JD_ADDRESS<-splitTheFeature(CUSTOMER_CRAWLER_JD_ADDRESS,'V1',featName)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_ADDRESS,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get user receiver addresss features [",time_diff,"secs ]")%>%print