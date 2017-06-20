ch <- odbcConnect("impalaodbc", uid = "changlue.she", pwd = "9f671e71a33b3454b9593d4f32f6ff18")
sqlTables(ch)
sql<- paste("select max(custorm_id)as custorm_no,login_name as phone from crawler.crawl_mobile_call_record_hive group by login_name",sep="")
PHONE_CUST_INFO <- sqlQuery(ch,sql,as.is=T)%>%data.table

#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(PHONE_CUST_INFO,HP_SHARE_DIR,'phone')

