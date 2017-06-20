#load app raw datas
start_time <-Sys.time()
#-----------------------------------------------------------------------------------------------------
#regnize the money term and orgnization term entities
source('~/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/moneyOrgEntity_regnization.R')
#-----------------------------------------------------------------------------------------------------
#resort the data
messageDF  <-messageDF[order(custorm_id,sms_time)]
#define data types
messageDF[,'money']<-suppressWarnings(messageDF$money%>%as.character%>%as.numeric) 
#add default
DFT_MoneyTerm  <- moneyTermKeyWordMapper%>%unique
DFT_NUM        <- length(DFT_MoneyTerm)
DFT_DT <-data.table(custorm_id=rep('-1',DFT_NUM)
                    ,orgTerm =rep('-1',DFT_NUM)
                    ,money =rep(-1,DFT_NUM)
                    ,sms_time =rep('-1',DFT_NUM)
                    ,moneyTerm=DFT_MoneyTerm)
messageDF<-rbind(messageDF,DFT_DT)
#-----------------------------------------------------------------------------------------------------
#aggregate funcs
#overall statistics
overallMoneyStat   <-dcast(messageDF,custorm_id~.,fun=list(sum,mean,max),value.var = 'money')
overallFreqOrgStat <-messageDF[,getVecFeat(orgTerm),.(custorm_id)]
#money orgnization term group statistics
groupMoneyStat   <-dcast(messageDF,custorm_id~moneyTerm,fun=list(sum,mean,max,sd),value.var = 'money')
groupFreqOrgStat <-messageDF[,getVecFeat(orgTerm),.(custorm_id,moneyTerm)]

CustOrgMoneyStat <-messageDF[,.(maxMoney=max(money),sumMoney=sum(money),avgMoney=mean(money)),.(custorm_id,orgTerm,moneyTerm)]

groupMaxMoneyStat<-CustOrgMoneyStat[,.(orgTerm=orgTerm[which.max(maxMoney)],maxMoney=max(maxMoney)),.(custorm_id,moneyTerm)]
groupMaxMoneyStat<-dcast(groupMaxMoneyStat,custorm_id~moneyTerm,fun=first,value.var=c('orgTerm','maxMoney'),fill=-1)

groupSumMoneyStat<-CustOrgMoneyStat[,.(orgTerm=orgTerm[which.max(sumMoney)],sumMoney=max(sumMoney)),.(custorm_id,moneyTerm)]
groupSumMoneyStat<-dcast(groupSumMoneyStat,custorm_id~moneyTerm,fun=first,value.var=c('orgTerm','sumMoney'),fill=-1)

groupAvgMoneyStat<-CustOrgMoneyStat[,.(orgTerm=orgTerm[which.max(avgMoney)],avgMoney=max(avgMoney)),.(custorm_id,moneyTerm)]
groupAvgMoneyStat<-dcast(groupAvgMoneyStat,custorm_id~moneyTerm,fun=first,value.var=c('orgTerm','avgMoney'),fill=-1)
#-----------------------------------------------------------------------------------------------------
#rename
moneyTerms  <-c('ukTerm','balance','loan','recharge','interest','withdraw','deposit','consume','fare','rollOut','repay','overdraft')

names(overallMoneyStat)<-c('custorm_id','crawl_sms_overallCashFlow_sum','crawl_sms_overallCashFlow_avg','crawl_sms_overallCashFlow_max')

featNames   <-setFeatNames(list(c('sms'),paste(moneyTerms,'CashFlow',sep=""),c('sum','avg','max','std')),c(2,3,1))
names(groupMoneyStat)  <-c('custorm_id',featNames)

featNames   <-setFeatNames(list(c('sms'),paste(moneyTerms,'OneTrancMax',sep=""),c('cate','max')),c(2,3,1))
names(groupMaxMoneyStat)  <-c('custorm_id',featNames)

featNames   <-setFeatNames(list(c('sms'),paste(moneyTerms,'allTrancMax',sep=""),c('cate','sum')),c(2,3,1))
names(groupSumMoneyStat)  <-c('custorm_id',featNames)

featNames   <-setFeatNames(list(c('sms'),paste(moneyTerms,'avgTrancMax',sep=""),c('cate','avg')),c(2,3,1))
names(groupAvgMoneyStat)  <-c('custorm_id',featNames)

overallFreqOrgStat<-splitTheFeature(overallFreqOrgStat,'V1',c('crawl_sms_mostFreqOrgTerm_cate','crawl_sms_mostFreqOrgTerm_prop','crawl_sms_mostFreqOrgTerm_num'))

groupFreqOrgStat  <-splitTheFeature(groupFreqOrgStat,'V1',c('v1','v2','v3'))
groupFreqOrgStat  <-dcast(groupFreqOrgStat,custorm_id~moneyTerm,fun=first,value.var=c('v1','v2','v3'),fill=-1)
featNames <-setFeatNames(list(c('sms'),paste(moneyTerms,'MostFreqOrgTerm',sep=""),c('cate','prop','num')),c(2,3,1))
names(groupFreqOrgStat)  <-c('custorm_id',featNames)
#-----------------------------------------------------------------------------------------------------
#merge
CUSTORMER_CRAWLER_SMS <- merge(overallMoneyStat,overallFreqOrgStat ,by='custorm_id',all = T)
CUSTORMER_CRAWLER_SMS <- merge(CUSTORMER_CRAWLER_SMS,groupMoneyStat   ,by='custorm_id',all = T)
CUSTORMER_CRAWLER_SMS <- merge(CUSTORMER_CRAWLER_SMS,groupMaxMoneyStat,by='custorm_id',all = T)
CUSTORMER_CRAWLER_SMS <- merge(CUSTORMER_CRAWLER_SMS,groupSumMoneyStat,by='custorm_id',all = T)
CUSTORMER_CRAWLER_SMS <- merge(CUSTORMER_CRAWLER_SMS,groupAvgMoneyStat,by='custorm_id',all = T)
CUSTORMER_CRAWLER_SMS <- merge(CUSTORMER_CRAWLER_SMS,groupFreqOrgStat ,by='custorm_id',all = T)
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(CUSTORMER_CRAWLER_SMS,HP_RESULT_DIR,'custorm_id')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-start_time%>%as.numeric)%>%round
if(HP_SUBMUDULE_TIME)paste("have finished submodule: get Aggregated features [",time_diff,"secs ]")%>%print

