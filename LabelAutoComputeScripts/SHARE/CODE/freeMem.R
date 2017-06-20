#-----------------------------------------------------------------------------------------------------
# free memory
rls<-ls()
rls<-rls[!rls%>%str_detect(.,'HP_')]
rm(list=rls)
gc()
#-----------------------------------------------------------------------------------------------------
#reload relate funcs
source('~/project/LabelAutoComputeScripts/SHARE/CODE/basicFuncs.R')