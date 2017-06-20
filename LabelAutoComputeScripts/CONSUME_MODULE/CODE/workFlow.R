#jd module work flow
md_start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load relate funcs
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')
#load datas
source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/loadData.R')
#-----------------------------------------------------------------------------------------------------
#get buyed items
if(orderProduct$price%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_buyFeat.R')
#get user info
if(userInfo$birthday%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_userinfo.R')
#get address info
if(addressInfo$receiver%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_addFeat.R')
#get pay info
if(orderDetail$name_rec%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_payFeat.R')
#get authentication info
if(authInfo$channel%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_authFeat.R')
#get credit info
if(baiTiaoInfo$credit_score%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_creditFeat.R')
#get bank card info
if(bandcardInfo$bank_name%>%length)
  source('~/project/LabelAutoComputeScripts/CONSUME_MODULE/CODE/get_bankcardFeat.R')
#-----------------------------------------------------------------------------------------------------
#compute the running time
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-md_start_time%>%as.numeric)%>%round
paste("have finished module: consume related topic [",time_diff,"secs ]")%>%print
#-----------------------------------------------------------------------------------------------------
#free memory
rls<-ls()
rls<-rls[!rls%>%str_detect(.,'HP_')]
rm(list=rls)
gc()
