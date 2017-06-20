#app module work flow
md_start_time<-Sys.time()
#-----------------------------------------------------------------------------------------------------
#load relate funcs
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')
#load datas
source('~/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/loadData.R')

#-----------------------------------------------------------------------------------------------------
#get app labels
if(packagesInfo$app_name%>%length)
  #source('~/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/get_pop_app.R')
  source('~/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/appFeatAggregate.R')
#-----------------------------------------------------------------------------------------------------
#aggregate the txt features
if(messageInfo$custorm_id%>%length)
  source('~/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/textFeatAggregate.R')
#-----------------------------------------------------------------------------------------------------
#aggregate the call features
if(callInfo$custorm_id%>%length)
  source('~/project/LabelAutoComputeScripts/SOCIAL_MODULE/CODE/callFeatAggregate.R')
#-----------------------------------------------------------------------------------------------------
end_time<-Sys.time()
time_diff<-(end_time%>%as.numeric-md_start_time%>%as.numeric)%>%round
paste("have finished module: social related topic[",time_diff,"secs ]")%>%print
#-----------------------------------------------------------------------------------------------------
# free memory
rls<-ls()
rls<-rls[!rls%>%str_detect(.,'HP_')]
rm(list=rls)
gc()
