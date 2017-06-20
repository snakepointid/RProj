#load app raw datas
start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#exact orgnization usage term
orgTerms  <-str_extract(messageInfo$content,'\\[[\u4e00-\u9fa5]+\\]|\\【[\u4e00-\u9fa5]+\\】|中国移动|中国联通|中国电信')
orgTerms  <-str_extract(orgTerms,'[\u4e00-\u9fa5]+')
orgFreq   <-orgTerms%>%table 
#relate phone number
PHONE_ORG_INFO <- data.table(phone=messageInfo$phone,org=orgTerms)
PHONE_ORG_INFO <- PHONE_ORG_INFO[!org%>%is.na]%>%unique
PHONE_ORG_INFO <- PHONE_ORG_INFO[,.(org=org[which.max(orgFreq[org])]),.(phone)]
#save
batchSaveOBJ(PHONE_ORG_INFO,HP_SHARE_DIR,'phone')
#-----------------------------------------------------------------------------------------------------
#exact money usage term
moneyTerms<-str_extract_all(messageInfo$content,'\\w{2,7}\\d+[\\.\\d+]*元|\\w{2,7}RMB\\d+[\\.\\d+]*|\\w{2,7}￥\\d+[\\.\\d+]*')%>%unlist
moneyTerms<-str_extract(moneyTerms,'[\u4e00-\u9fa5]+')
moneyTerms%>%table%>%sort(.,T)
#-----------------------------------------------------------------------------------------------------
#segment
segWork     <- worker()
segWordList <-segWork <= moneyTerms
segWordList%>%table%>%sort(.,T)

moneyTermKeyWord <- c('余额',
                      '消费','快捷金额','付款','支付',
                      '支出','取款','支取',
                      '收入',
                      '转入','存入','存款',
                      '充值',
                      '费用','手续费','缴费','交费',
                      '利息',
                      '转账','转出',
                      '还款额','还款',
                      '欠费','透支')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get money term key words[",time_diff,"secs ]")%>%print



