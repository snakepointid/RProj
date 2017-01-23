df<-dtrain
depth<-c(8,2,3,4,5,7,6,9,10,11,12)
eta<-c(0.02,0.04,0.07,0.1,0.3,0.7,0.001)
subsample<-c(0.9,0.5,1,0.7)
gamma<-c(0.01,1,0,0.1,0.3)
colsample_bytree<-c(0.5,0.7,0.9,0.2,1)
min_child_weight<-c(0.1,1,10,100,0.01)
lambda=c(1,0,100,500,1000)
scale_pos_weight=c(0.3,0.2,0.267,0.4,0.12)
old<-0
trees<-100
m.v<--9999
########################################################
for(dep in depth){
  for(et in eta){
    for(sub in subsample){
      for(lr in lambda){
        for(min in min_child_weight){
          for(scl in scale_pos_weight){
            for(gm in gamma){
              for(cb in colsample_bytree){
                para<-list(max.depth =dep,scale_pos_weight=scl,lambda=lr,gamma=gm,
                           colsample_bytree=cb,min_child_weight=min,subsample=sub,
                           eta = et,objective="binary:logistic",nthread=4,eval_metric=evalerror)
                set.seed(123)
                bsts <- xgb.cv(params=para, data=dtrain,nround =trees,verbose = F,nfold=4)
                test<-bsts[[3]]
                train<-bsts[[1]]
                mp<-which.max(test)
                new<-test[mp]
                paste("test score is",new,"train score is",train[mp])%>%print()
                if(new>old){
                  eta<-et
                  depth<-dep
                  subsample<-sub
                  min_child_weight<-min
                  lambda<-lr
                  scale_pos_weight<-scl
                  gamma<-gm
                  colsample_bytree<-cb
                  
                  old<-new
                  best.para<-para
                  print(para)
                }
              }
            }
          }
        }
      }
    }
  }}
#######################################################################
#######################################################################
eta<-c(0.02,0.04,0.07,0.1,0.3,0.7,0.001)
subsample<-c(0.9,0.5,1,0.7)
gamma<-c(0.01,1,0,0.1,0.3)
colsample_bytree<-c(0.5,0.7,0.9,0.2,1)
min_child_weight<-c(0.1,1,10,100,0.01)
lambda=c(1,0,100,500,1000)
scale_pos_weight=c(0.3,0.2,0.267,0.4)
########################################################
for(cb in colsample_bytree){
  for(gm in gamma){
    for(scl in scale_pos_weight){
      for(min in min_child_weight){
        for(lr in lambda){
          for(sub in subsample){
            for(et in eta){
              for(dep in depth){
                para<-list(max.depth =dep,scale_pos_weight=scl,lambda=lr,gamma=gm,
                           colsample_bytree=cb,min_child_weight=min,subsample=sub,
                           eta = et,objective="binary:logistic",nthread=4,eval_metric=evalerror)
                set.seed(123)
                bsts <- xgb.cv(params=para, data=dtrain,nround =trees,verbose = F,nfold=4)
                test<-bsts[[3]]
                train<-bsts[[1]]
                mp<-which.max(test)
                new<-test[mp]
                paste("test score is",new,"train score is",train[mp])%>%print()
                if(new>old){
                  eta<-et
                  depth<-dep
                  subsample<-sub
                  min_child_weight<-min
                  lambda<-lr
                  scale_pos_weight<-scl
                  gamma<-gm
                  colsample_bytree<-cb
                  
                  old<-new
                  best.para<-para
                  print(para)
                }
              }
            }
          }
        }
      }
    }
  }}
