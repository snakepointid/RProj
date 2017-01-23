#########################################分段后变量筛选
iv<-get_var_info(p2p.model,names(p2p.model),"def",get_IV)
useful<-names(iv[iv>0.02])
disp2p<-para_sep_vec(list(p2p.model,useful,500),sep_wrapper)

for(i in useful){
  table(disp2p[,i])%>%print()
  print(i)
}

iv<-get_var_info(disp2p,useful,"def",get_IV)
useful<-names(iv[iv>0.02])
temp.inf<-info_matr(useful,disp2p,"def",way=get_MI)
useful<-keep.or.drop(temp.inf[[1]],50)
######################################################## 