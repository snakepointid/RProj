test<-list()
test[[1]]<-useful
for(i in 2:100){
  temp<-test[[i-1]]
  test[[i]]<-variable_select_bw(para_C50,p2p.model,temp,0.003,controls.C50)[[1]]
  paste("这是第",i,"次循环了")
}
use<-test[[17]]
para_C50(p2p.model,useful,cv.list,ks,controls.C50)

