#app module work flow
md_start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load relate funcs
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')
#-----------------------------------------------------------------------------------------------------
#load datas
source('~/project/LabelAutoComputeScripts/MAINBUSI_MODULE/CODE/loadData.R')
#-----------------------------------------------------------------------------------------------------
#get loan withdraw information
if(loanWithdrawInfo$contra_no%>%length)
  source('~/project/LabelAutoComputeScripts/MAINBUSI_MODULE/CODE/handle_withdrawInfo.R')
#get loan withdraw information
if(loanAcctInfo$contra_no%>%length)
  source('~/project/LabelAutoComputeScripts/MAINBUSI_MODULE/CODE/handle_acctInfo.R')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-md_start_time%>%as.numeric)%>%round
paste("have finished module: main business related topic [",time_diff,"secs ]")%>%print
#-----------------------------------------------------------------------------------------------------
# free memory
rls<-ls()
rls<-rls[!rls%>%str_detect(.,'HP_')]
rm(list=rls)
gc()