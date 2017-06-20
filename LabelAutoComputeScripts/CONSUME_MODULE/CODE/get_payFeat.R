#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
orderDetail[,'amt_order']<-suppressWarnings(orderDetail$amt_order%>%as.numeric) 
#mapping time
orderDetail[,"day"]     <-orderDetail[,substr(time_order,1,10)] %>%as.character
orderDetail[,"hour"]    <-orderDetail[,substr(time_order,12,13)]%>%as.numeric
orderDetail[,"minute"]  <-orderDetail[,substr(time_order,15,16)]%>%as.numeric
orderDetail[,"dayZone"] <-cut(orderDetail$hour,c(0,6,12,18,24),right=F,labels = c("dawn","morning","afternoon","everning"))%>%as.character
 
earlday <-orderDetail$day%>%unique%>%as.Date%>%min(.,na.rm=T)
lastday <-orderDetail$day%>%unique%>%as.Date%>%max(.,na.rm=T)
dateSpan<-seq(earlday,lastday,1)
weekDay <-dateSpan%>%weekdays
names(dateSpan) <-dateSpan 
names(weekDay)  <-dateSpan
#maping
orderDetail[,"weekday"]  <-orderDetail[,weekDay[day]]
orderDetail[,"day"]      <-orderDetail[,dateSpan[day]]
#preprocess
DFT_payType <-c("在线支付" ,"货到付款","分期付款","上门自提","邮局汇款")
DFT_dayZone <-c('dawn' ,'morning' ,'afternoon' ,'everning')
DFT_week    <-c("Saturday" , "Thursday",  "Tuesday"  , "Friday"  ,  "Monday" ,   "Sunday"  ,  "Wednesday" )
defalutDF   <-suppressWarnings(data.table(custorm_id='-1',amt_order=-1,day=as.Date('2024-04-30'),type_pay=DFT_payType,dayZone=DFT_dayZone,weekday=DFT_week))
FEDF        <-rbind(orderDetail[,.(custorm_id,amt_order,day,type_pay,dayZone,weekday)],defalutDF)
#overall feat
moneyFeat   <-dcast(FEDF[!amt_order%>%is.na], custorm_id  ~., fun=list(sum,mean,max,sd), value.var=c("amt_order"),fill=-1)
orderFeat   <-dcast(FEDF[!day%>%is.na],       custorm_id  ~., fun=getOrderFeat, value.var=c("day"))
typeFeat    <-dcast(FEDF[type_pay%in%DFT_payType], custorm_id  ~type_pay,fun=length, value.var=c("custorm_id"))
dayZoneFeat <-dcast(FEDF[dayZone %in%DFT_dayZone], custorm_id  ~dayZone, fun=length, value.var=c("custorm_id"))
weekdayFeat <-dcast(FEDF[weekday %in%DFT_week],    custorm_id  ~weekday, fun=length, value.var=c("custorm_id"))
typePropFeat     <-dcast(FEDF,    custorm_id  ~., fun=getVecFeat, value.var=c("type_pay"))
dayZonePropFeat  <-dcast(FEDF,    custorm_id  ~., fun=getVecFeat, value.var=c("dayZone"))
weekdayPropFeat  <-dcast(FEDF,    custorm_id  ~., fun=getVecFeat, value.var=c("weekday"))
#rename
names(moneyFeat)  <- c('custorm_id','crawl_jd_orderMoney_sum'  ,'crawl_jd_orderMoney_avg','crawl_jd_orderMoney_max','crawl_jd_orderMoney_std')
names(typeFeat)   <- c('custorm_id','crawl_jd_selfPickPay_num' ,'crawl_jd_installationPay_num','crawl_jd_onLinePay_num','crawl_jd_onDoorPay_num','crawl_jd_mailPay_num')    
names(dayZoneFeat)<- c('custorm_id','crawl_jd_afternoonBuy_num','crawl_jd_dawnBuy_num','crawl_jd_everningBuy_num','crawl_jd_morningBuy_num')    
names(weekdayFeat)<- c('custorm_id','crawl_jd_FridayBuy_num'   ,'crawl_jd_MondayBuy_num','crawl_jd_SaturdayBuy_num','crawl_jd_SundayBuy_num','crawl_jd_ThursdayBuy_num','crawl_jd_TuesdayBuy_num','crawl_jd_WednesdayBuy_num')    
orderFeat<-splitTheFeature(orderFeat[,.(custorm_id,V1=.)],'V1',c('crawl_jd_orderGap_mean','crawl_jd_orderGap_std'))
typePropFeat<-splitTheFeature(typePropFeat[,.(custorm_id,V1=.)],'V1',c('crawl_jd_mostPayType_cate','crawl_jd_mostPayType_prop','crawl_jd_mostPayType_num'))
dayZonePropFeat<-splitTheFeature(dayZonePropFeat[,.(custorm_id,V1=.)],'V1',c('crawl_jd_mostBuyDayzone_cate','crawl_jd_mostBuyDayzone_prop','crawl_jd_mostBuyDayzone_num'))
weekdayPropFeat<-splitTheFeature(weekdayPropFeat[,.(custorm_id,V1=.)],'V1',c('crawl_jd_mostBuyWeek_cate','crawl_jd_mostBuyWeek_prop','crawl_jd_mostBuyWeek_num'))
#merge
CUSTOMER_CRAWLER_JD_PAY <- merge(moneyFeat,typeFeat,by='custorm_id',all = T)
CUSTOMER_CRAWLER_JD_PAY <- merge(CUSTOMER_CRAWLER_JD_PAY,dayZoneFeat,by='custorm_id',all = T)
CUSTOMER_CRAWLER_JD_PAY <- merge(CUSTOMER_CRAWLER_JD_PAY,weekdayFeat,by='custorm_id',all = T)
CUSTOMER_CRAWLER_JD_PAY <- merge(CUSTOMER_CRAWLER_JD_PAY,orderFeat,by='custorm_id',all = T)
CUSTOMER_CRAWLER_JD_PAY <- merge(CUSTOMER_CRAWLER_JD_PAY,typePropFeat,by='custorm_id',all = T)
CUSTOMER_CRAWLER_JD_PAY <- merge(CUSTOMER_CRAWLER_JD_PAY,dayZonePropFeat,by='custorm_id',all = T)
CUSTOMER_CRAWLER_JD_PAY <- merge(CUSTOMER_CRAWLER_JD_PAY,weekdayPropFeat,by='custorm_id',all = T)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTOMER_CRAWLER_JD_PAY,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get user pay features [",time_diff,"secs ]")%>%print
 
