library(C50)
########################################参数取值
subset = TRUE
earlyStopping =FALSE
fuzzyThreshold = FALSE
winnow =FALSE
noGlobalPruning =TRUE
CF = 0.5
minCases = 10
sample = 0.9
########################################数据选择
df<-p2p
var.list<-useful

########################################开始调优
oldks<-0
for(sub in subset){
  for(ear in earlyStopping){
    for(fuzz in fuzzyThreshold){
      for(win in winnow){
        for(nog in noGlobalPruning){
          for(cf in CF){
            for(min in minCases){
              for(sam in sample){
                
                kss<-para_C50(df,var.list,cv.list,ks,control=C5.0Control(
                              subset = sub,winnow =win,noGlobalPruning = nog, 
                               CF = cf,minCases = min,fuzzyThreshold = fuzz, 
                               sample = sam,earlyStopping = ear,seed = 1948))
                print(kss[[1]])
                para<-c(sub,ear,fuzz,win,nog,cf,min,sam)
                names(para)<-c("subset","earlyStopping",
                               "fuzzyThreshold","winnow","noGlobalPruning","CF","minCases","sample")
                newks<-kss[[2]]
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
controls.C50<-C5.0Control(subset = TRUE, 
            bands = 0, 
            winnow =FALSE, 
            noGlobalPruning = TRUE, 
            CF = 0.5, 
            minCases = 10, 
            fuzzyThreshold = FALSE, 
            sample = 0.9, 
            seed = 1948,  
            earlyStopping = FALSE,
            label = "outcome")
