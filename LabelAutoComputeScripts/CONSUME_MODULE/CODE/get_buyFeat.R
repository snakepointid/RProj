#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load item class sets
JD_PRODUCT_INFO  <- loadOBJ(JD_PRODUCT_INFO,HP_SHARE_DIR)
popColors        <- loadOBJ(popColors,HP_SHARE_DIR)
#------------------------------------------------------------------------------------------------------------------------------------------------
orderProduct <-merge(orderProduct,JD_PRODUCT_INFO,by.x='product_id',by.y='产品编号',all.x=T)
orderProduct <-orderProduct[二级分类!='通讯充值'&product_id!=1]
orderProduct[,'颜色']<-str_extract(orderProduct$name,paste(popColors$x,collapse = '|'))%>%unlist
#------------------------------------------------------------------------------------------------------------------------------------------------
CUSTOMER_CRAWLER_JD_BUY <- orderProduct[,getBuyedFeat(一级分类,二级分类,三级分类,品牌,颜色),.(custorm_id)]
featName<-c('crawl_jd_firClsFavor_cate','crawl_jd_firFavor_prop','crawl_jd_firFavor_num',
            'crawl_jd_secClsFavor_cate','crawl_jd_secFavor_prop','crawl_jd_secFavor_num',
            'crawl_jd_thdClsFavor_cate','crawl_jd_thdFavor_prop','crawl_jd_thdFavor_num',
            'crawl_jd_brdFavor_cate','crawl_jd_brdFavor_prop','crawl_jd_brdFavor_num',
            'crawl_jd_colorFavor_cate','crawl_jd_colorFavor_prop','crawl_jd_colorFavor_num')
CUSTOMER_CRAWLER_JD_BUY<-splitTheFeature(CUSTOMER_CRAWLER_JD_BUY,'V1',featName)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_BUY,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get user item favor features [",time_diff,"secs ]")%>%print