#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#set fileds
authInfoFields    <- c('channel','finan_serv')
bandcardInfoFields<- c('bank_name','tail_num','card_type','owner_name','phone')
baiTiaoInfoFields <- c('credit_score','overdraft','quota')
orderDetailFields <- c('name_rec', 'time_order','amt_order','type_pay')
orderProductFields<- c('price','name','product_id')
addressInfoFields <- c('receiver','region','addr')
userInfoFields    <- c('sex','birthday','email','real_name','id_card','wechat_bound','qq_bound','account_grade','account_type')
#read data
authInfo     <-loadDATA(authInfoFields    ,'custorm_id','dt','crawler.crawl_jd_auth_info_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
bandcardInfo <-loadDATA(bandcardInfoFields,'custorm_id','dt','crawler.crawl_jd_bankcard_info_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
baiTiaoInfo  <-loadDATA(baiTiaoInfoFields ,'custorm_id','dt','crawler.crawl_jd_bt_info_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
orderProduct <-loadDATA(orderProductFields,'custorm_id','dt','crawler.crawl_jd_order_product_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
addressInfo  <-loadDATA(addressInfoFields ,'custorm_id','dt','crawler.crawl_jd_receive_addr_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
userInfo     <-loadDATA(userInfoFields    ,'custorm_id','dt','crawler.crawl_jd_user_info_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
orderDetail  <-loadDATA(orderDetailFields ,'custorm_id','dt','crawler.crawl_jd_order_detail_hive',HP_LAST_DATE,HP_LIMIT_SAMP)
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: load consume related raw datas [",time_diff,"secs ]")%>%print