#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#select subset 
userInfo<-userInfo[,.(recordNum = c(1:.N),sex,birthday,email,real_name,id_card
                      ,wechat_bound,qq_bound,account_grade,account_type),.(custorm_id)] 
#keep customer id unique
userInfo<-userInfo[recordNum==1,]
CUSTOMER_CRAWLER_JD_USER<-data.table(custorm_id = userInfo$custorm_id)
#get features
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_birthday_date']    <-userInfo$birthday
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_email_has']        <-1-(userInfo$email%>%is.na)    %>%as.numeric
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_realname_has']     <-1-(userInfo$real_name%>%is.na)%>%as.numeric
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_idCard_has']       <-1-(userInfo$id_card%>%is.na)  %>%as.numeric
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_wetchat_has']      <-1-(userInfo$wechat_bound%>%is.na)%>%as.numeric
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_qq_has']           <-1-(userInfo$qq_bound%>%is.na) %>%as.numeric
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_sex_cate']         <-userInfo$sex
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_accountGrade_cate']<-userInfo$account_grade 
CUSTOMER_CRAWLER_JD_USER[,'crawl_jd_accountType_cate'] <-userInfo$account_type
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_USER,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get user jd account features [",time_diff,"secs ]")%>%print