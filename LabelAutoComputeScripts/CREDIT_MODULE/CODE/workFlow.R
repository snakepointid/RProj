#app module work flow
md_start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load relate funcs
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')
#-----------------------------------------------------------------------------------------------------
#load datas
source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/loadData.R')
#-----------------------------------------------------------------------------------------------------
#get negtive information
if(negInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/clean_negtiveInfo.R')
#-----------------------------------------------------------------------------------------------------
#get live information
if(liveInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/clean_liveInfo.R')
#-----------------------------------------------------------------------------------------------------
#get work information
if(workInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/clean_workInfo.R')
#-----------------------------------------------------------------------------------------------------
#get loan detail information
if(loanInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/handle_loanInfo.R')
#-----------------------------------------------------------------------------------------------------
#get card detail information
if(cardInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/handle_cardInfo.R')
#-----------------------------------------------------------------------------------------------------
#get ovdue  information
if(ovdueInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/handle_ovdueInfo.R')
#-----------------------------------------------------------------------------------------------------
#get house finance  information
if(hsFinInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/clean_houseFinInfo.R')
#-----------------------------------------------------------------------------------------------------
#get bai rong specific  information
if(brSpeInfo$union_id%>%length)
  source('~/project/LabelAutoComputeScripts/CREDIT_MODULE/CODE/handle_brSpecInfo.R')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-md_start_time%>%as.numeric)%>%round
paste("have finished module: credit query related topic[",time_diff,"secs ]")%>%print
#-----------------------------------------------------------------------------------------------------
# free memory
rls<-ls()
rls<-rls[!rls%>%str_detect(.,'HP_')]
rm(list=rls)
gc()
