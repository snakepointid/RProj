#get pop app names
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#change data types
packagesInfo$app_name   <-packagesInfo$app_name%>%as.character
packagesInfo$custorm_id <-packagesInfo$custorm_id%>%as.character()
#drop null samples
packagesInfo<-packagesInfo[!is.na(packagesInfo$app_name),]
#norm the characters
packagesInfo$app_name<-packagesInfo$app_name%>%tolower()
#drop duplicated samples
packagesInfo<-unique(packagesInfo)
#-----------------------------------------------------------------------------------------------------
#pop apps
popAPP<-c('天猫','马上金融',"中国银行","中国建设银行","中国工商银行","农行掌上银行",'手机京东','手机淘宝','支付宝',
          '滴滴出行','美团','今日头条','高德地图','优酷','微博','微信','qq','去哪儿旅行',"大码美衣","优衣库",
          "穿衣搭配","京致衣橱","唯品会","蘑菇街","聚美优品","美丽说","明星衣橱","百度团购","美团","大众点评" 
          ,"百度糯米","团贷网",'苏宁','手机京东','华为商城','国美','亚马逊','微信','qq','陌陌','探探','飞信',
          '叮叮','易信','旺信','企业qq','百合网','世纪佳缘','珍爱网','途家','携程','飞猪','春秋旅游','艺龙旅行'
          ,'途牛','去哪儿旅行','驴妈妈','安居客','链家','自如','我爱我家','58同城','拉钩','猎聘网','智联招聘',
          '前程无忧','脉脉','滴滴','易道','优步','首汽约车')
#find topic relate app
KEYWORDS<-c("银行","信用卡","借|贷","现金|钱包","金融","地图|导航","外卖|饿了","头条|新闻|资讯|报道","阅读|小说|读书"
            ,"知乎|豆瓣|贴吧","直播","优酷|影音|爱奇艺","音乐|电台","邮箱","租房|租车|公寓","药|医|健康|大夫"
            ,"美图|相册","出行|打车")
#get different topical pop app
for(KEYWORD in KEYWORDS){
  MATCH_APPS<-packagesInfo$app_name[str_detect(packagesInfo$app_name, KEYWORD)]
  tmpPop<-(MATCH_APPS%>%table%>%sort(.,T)%>%names)
  tmpPop<-tmpPop[1:min(tmpPop%>%length,HP_NUM_POPappNumbers)]
  popAPP<-c(popAPP,tmpPop)
}
#save pop app
popAPP<-data.table(popAPP)
saveOBJ(popAPP,HP_SHARE_DIR)
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
paste("have finished submodule: get pop app names and save [",time_diff,"secs ]")%>%print