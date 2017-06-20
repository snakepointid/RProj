#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#initial money term key word
moneyTermKeyWord <- c('余额',
                      '消费','快捷金额','付款','支付',
                      '支出','取款','支取','取现',
                      '收入','转入','存入','存款',
                      '充值',
                      '费用','手续费','缴费','交费',
                      '利息',
                      '转账','转出',
                      '还款',
                      '欠费','透支',
                      '借','贷')
moneyTermPattern<-paste(moneyTermKeyWord,collapse='|')
#-----------------------------------------------------------------------------------------------------
#get money related subset
moneyRelateInfo<-messageInfo[str_detect(messageInfo$content,'\\w{2,7}\\d+[\\.\\d+]*元|\\w{2,7}RMB\\d+[\\.\\d+]*|\\w{2,7}￥\\d+[\\.\\d+]*')]
#-----------------------------------------------------------------------------------------------------
#get money term entity
moneyTerms<-str_extract_all(moneyRelateInfo$content,'\\w{2,7}\\d+[\\.\\d+]*元|\\w{2,7}RMB\\d+[\\.\\d+]*|\\w{2,7}￥\\d+[\\.\\d+]*')
moneyTerms<-lapply(moneyTerms,function(moneyTerm)paste(paste(str_extract(moneyTerm,moneyTermPattern),
                                                             str_extract(moneyTerm,'\\d+[\\.\\d+]*(?=元)|(?<=RMB)\\d+[\\.\\d+]*|(?<=￥)\\d+[\\.\\d+]*'),sep='#'),collapse = '_'))%>%unlist
#get orgnization entity
PHONE_ORG_INFO<-loadOBJ(PHONE_ORG_INFO,HP_SHARE_DIR)
orgTerms  <-merge(moneyRelateInfo,PHONE_ORG_INFO,by=c('phone'),all.x = T)$org
#put together
messageDF <-data.table(custorm_id = moneyRelateInfo$custorm_id,moneyTerm=moneyTerms,orgTerm=orgTerms,sms_time=moneyRelateInfo$sms_time)
messageDF <-messageDF[order(custorm_id,sms_time)]
#-----------------------------------------------------------------------------------------------------
#split data
messageDF <-messageDF[,.(moneyTerm=str_split(moneyTerm,'_')%>%unlist),.(custorm_id,orgTerm,sms_time)]
tmp       <- do.call(rbind,str_split(messageDF$moneyTerm,'#'))%>%data.table
names(tmp)<- c('moneyTerm','money')
messageDF[,c('moneyTerm'):=NULL]
messageDF<-cbind(messageDF,tmp)
messageDF[moneyTerm=='NA','moneyTerm']<-'ukMoneyTerm'
messageDF[orgTerm%>%is.na,'orgTerm']  <-'ukOrgTerm'
#-----------------------------------------------------------------------------------------------------
#truncate the categories
moneyTermKeyWordMapper<-c('余额',
                          '消费','消费','消费','消费',
                          '支出','支出','支出','支出',
                          '收入','收入','收入','收入',
                          '充值',
                          '费用','费用','费用','费用',
                          '利息',
                          '转出','转出',
                          '还款',
                          '透支','透支',
                          '借款','借款',
                          'ukMoneyTerm')
names(moneyTermKeyWordMapper)<-c(moneyTermKeyWord,'ukMoneyTerm')
messageDF[,'moneyTerm']<-moneyTermKeyWordMapper[messageDF$moneyTerm]
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: text entity regnization [",time_diff,"secs ]")%>%print



