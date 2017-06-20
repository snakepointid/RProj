library(rvest)
library(RCurl)
library(stringr)
library(data.table)
HP_JD_Crawler<-c(T,T,F)
JD_PRODUCT_INFO<-NULL
curse<-1
productID_n<-names(productIDs)
for(idx in c(curse:length(productID_n))){
  productID<-productID_n[idx]
  # get item class info -----------------------------------------------------
  records<-c(productID)
  names(records)<-c('产品编号')
  if(HP_JD_Crawler[1]){
    itemClass<-rep(NA,5)
    tryCatch({
      url<-paste("https://item.jd.com/",productID,".html",sep="")
      item_page <- read_html(url)
      itemClass <- item_page %>% html_node("body div#crumb-wrap div.w")%>% html_node("div")%>%html_nodes("div.item") 
      item<-html_attr(itemClass, "title")
      item<-item[!item%>%is.na]
      item<-item[item%>%length]
      itemClass<-html_text(itemClass%>%html_node('a'))
      itemClass<-c(itemClass[!itemClass%>%is.na][1:4],item)}, error = function(e)  a<-1)
    itemClass<-itemClass[1:5]
    names(itemClass)<-c('一级分类','二级分类','三级分类','品牌','商品名称')
    if(itemClass[itemClass%>%is.na]%>%length==5)
      next
    records<-c(records,itemClass)
  }
  if(HP_JD_Crawler[2]){
    # get product comments info -----------------------------------------------
    
    commentList<-rep(NA,5)
    tryCatch({
      url<-paste('https://club.jd.com/comment/productPageComments.action?callback=fetchJSON_comment98vv30364&productId=',productID,'&score=0&sortType=5&page=0&pageSize=10&isShadowSku=0&fold=1',sep='')
      comments=getURL(url,.encoding="gbk2203")
      averageScore<-(comments%>%str_extract_all(.,'(?<=\"averageScore\":)\\d+' ))[[1]]
      afterCount  <-(comments%>%str_extract_all(.,'(?<=\"afterCount\":)\\d+' ))[[1]]
      goodCount   <-(comments%>%str_extract_all(.,'(?<=\"goodCount\":)\\d+' ))[[1]]
      poorCount   <-(comments%>%str_extract_all(.,'(?<=\"poorCount\":)\\d+' ))[[1]]
      generalCount<-(comments%>%str_extract_all(.,'(?<=\"generalCount\":)\\d+' ))[[1]]
      commentList<-c(averageScore,afterCount,goodCount,poorCount,generalCount)
    }, error = function(e) a<-1)
    commentList<-commentList[1:5]
    names(commentList)<-c('平均评分','追加评论数','好评评论数','差评评论数','中评评论数')
    records<-c(records,commentList)
  }
  if(HP_JD_Crawler[3]){
    # get question info -------------------------------------------------------
    quesList<-NA
    tagList<-NA
    tryCatch({url<-paste('https://question.jd.com/question/getQuestionAnswerList.action?callback=jQuery3397783&page=1&productId=',productID,sep='')
    question = getURL(url,.encoding="utf-8")
    quesList<-question%>%str_extract_all(.,'(?<=\"content\":\")[\u4e00-\u9fa5\\,\\，\\?\\？a-zA-Z0-9]+(?=\",\"ip)')%>%unlist%>%paste(.,collapse='---')
    url<-paste('https://club.jd.com/comment/productPageComments.action?callback=fetchJSON_comment98vv30364&productId=',productID,'&score=0&sortType=5&page=0&pageSize=10&isShadowSku=0&fold=1',sep='')
    comments=getURL(url,.encoding="utf-8")
    tagList     <-comments%>%str_extract_all(.,'\"name\":\"[\u4e00-\u9fa5]+\"\\,\"status')%>%str_extract_all(.,'[\u4e00-\u9fa5]+')%>%unlist%>%paste(.,collapse='#')
    }, error = function(e)  a<-1)
    names(quesList)<-'关键问题'
    names(tagList) <-'热门标签'
    records<-c(records,quesList,tagList)
  }
  if(records[!records%>%is.na]%>%length<=1)
    next
  # put together ------------------------------------------------------------
  if(JD_PRODUCT_INFO%>%is.null)
    JD_PRODUCT_INFO<-records else
      JD_PRODUCT_INFO<-rbind(JD_PRODUCT_INFO,records)
  if(rnorm(1)>3)
    print(JD_PRODUCT_INFO[,1]%>%length)
  curse<-idx
}
JD_PRODUCT_INFO<-JD_PRODUCT_INFO%>%data.table
#-----------------------------------------------------------------------------------------------------
#save datas
batchSaveOBJ(JD_PRODUCT_INFO,HP_SHARE_DIR,'产品编号')