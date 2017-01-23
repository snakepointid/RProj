library(rpart)
########################################参数取值
minsplit =c(1:10*30)
cp =c(0.001,0.01,0.1)
maxcompete =1
maxsurrogate =1
usesurrogate=1
xval =4
surrogatestyle =1
maxdepth = c(2,3,4,5,6)


########################################数据选择
df<-traindata
var.list<-useful
########################################开始调优
oldks<-0
for(a in minsplit){
  for(b in cp){
    for(c in maxcompete){
      for(d in maxsurrogate){
        for(e in usesurrogate){
          for(f in xval){
            for(g in surrogatestyle){
              for(h in maxdepth){
                kss<-para_rpart(df,var.list,cv.list,ks,controls=rpart.control(minsplit = a,                                                                         maxdepth = h))
                print(kss[[1]])
                para<-c(a,b,c,d,e,f,g,h)
                names(para)<-c("minsplit","cp","maxcompete","maxsurrogate",
                               "usesurrogate","xval","surrogatestyle","maxdepth")
                newks<-kss[[1]]
                if(newks>oldks){
                  oldks<-newks
                  bestpara<-para
                  print(bestpara)
                }
              }
            }
          }
        }
      }
    }
  }
}


#####################################目前最有参数
controls.rpart<-rpart.control(minsplit =5,
                              cp =0.001,
                              maxcompete =1,
                              maxsurrogate =1,
                              usesurrogate=1,
                              xval =4,
                              surrogatestyle =1,
                              maxdepth = 8)
