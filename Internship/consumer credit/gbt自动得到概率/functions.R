handle_logic<-function(comp){
  type<-c()
  if('I'%in%comp)type='d'
  if(length(which(names(agg.types)%in%comp))>0&"ac"%in%comp&!'I'%in%comp)type='agg.m'
  if(length(which(names(agg.types)%in%comp))>0&("w"%in%comp|"t"%in%comp)&!'times'%in%comp)type='agg.b'
  if(length(which(comp=='m'))==1&'def'%in%comp&'times'%in%comp)type='d.t.m'
  if(length(which(comp=='m'))==2&'def'%in%comp&'times'%in%comp)type='d.t.s'
  if('card'%in%comp&'use'%in%comp&'type'%in%comp)type='c.u.t'
  if(('credit'%in%comp|'debit'%in%comp)&'use'%in%comp)type='u'
  if('time'%in%comp&'recent'%in%comp)type='w.m'
  if('tr'%in%comp)type='tr'
  if(length(which(names(agg.types)%in%comp))>0&("w"%in%comp|"t"%in%comp)&'times'%in%comp)type='b.sum'
  if('MatchType'%in%comp)type='mat.ty'
  if('auth'%in%comp&'stab'%in%comp)type='a.s'
  if('age'%in%comp)type='age'
  if('gender'%in%comp|"sex"%in%comp)type='sex'
  if('is'%in%comp&"m"%in%comp&("media"%in%comp|"cons"%in%comp))type='mocs'
  if('cons'%in%comp&"media"%in%comp)type='c&m'
  if('tot'%in%comp)type='cn'
  if("max"%in%comp&"cons"%in%comp)type='max_beha'
  if("max"%in%comp&"time"%in%comp)type='max_ac'
  if(!"max"%in%comp&"time"%in%comp&'recent'%in%comp)type='rec_ac'
  if("cont"%in%comp&"cons"%in%comp)type='cc'
  if("mobile"%in%comp)type='cell'
  if("cell"%in%comp&"dis"%in%comp)type='cd'
  if("prop"%in%comp)type='prop'
  if("mthsnc"%in%comp)type='mthsnc'
  if("if"%in%comp&'ac'%in%comp)type='sum.b'
  if("ac"%in%comp&'num'%in%comp)type='b.sum2'
  if(length(which(!comp%in%c("num","media","cons",'pay',"m","is",'v','visits')))>0&'is'%in%comp)type='oct'
  
  return(type)
}

get_deri<-function(deri.var,df){
  agg.types<<-list("avg"=mean,"max"=max,'wmax'=which.max,"sum"=sum,'m'=sum)
  comp<-unlist(str_extract_all(deri.var,"[a-z,A-Z]+"))
  date<-unlist(str_extract_all(deri.var,"\\d"))%>%as.numeric()
  if(handle_logic(comp)=="d"){
    print("handling logic is decode the code")
    comp<-unlist(str_extract_all(deri.var,"[a-z,A-Z,0-9]+"))
    comp<-comp[!comp%in%c("I")]
    ori<-cat_var(comp)
    deri.vec<-decode_type(df,ori)
  }else if(handle_logic(comp)=='agg.m'){
    print("handling logic is get aggregate value")
    dates<-dates_handle(date)
    if(!"def"%in%comp)
      comp<-unique(c("I",comp))
    rep<-comp[comp%in%names(agg.types)][1]
    oris<-get_origvar(dates,comp,repla = rep)
    if(length(dates)>6){
      vars<-match_oris(oris[1],names(df),3)
      oris<-c()
      for(i in vars){
        temp<-unlist(str_extract_all(i,"\\d+"))%>%as.numeric()
        if(max(temp)<=length(dates)&(length(temp)==1|min(temp)>6))
          oris<-c(oris,i)
      }}
    deri.vec<-agg_type(df,oris,rep)
  }else if(handle_logic(comp)=='agg.b'){
    print("handling logic is get bino aggregate value")
    temp<-unlist(str_extract(deri.var,"\\d+[wt]"))
    temp<-unlist(str_extract(temp,"\\d+"))%>%as.numeric()
    if('w'%in%comp)
      money<-10000*temp else if('t'%in%comp)
        money<-1000*temp else
          money<-0
    dates<-dates_handle(date[1:2])
    oris<-get_origvar(dates,comp,repla = c('m','sum'),drop=c('w'),add = 'I',loc=1)
    temp<-data.frame(agg_type(df,oris,'sum'))
    temp[is.na(temp)]<-0
    deri.vec<-c2d_type(temp,names(temp),c(0,money))
  }else if(handle_logic(comp)=='d.t.m'){
    print("handling logic is get month def timmes")
    dates<-dates_handle(date)
    oris<-get_origvar(dates,comp,repla = 'm',drop='times',add='credit',loc=3)
    deri.vec<-agg_type(df,oris,'sum')
  }else if(handle_logic(comp)=='d.t.s'){
    print("handling logic is get season def timmes")
    dates<-c()
    for(i in 1:(date[2]%/%3)){
      temp<-paste("sum",(i-1)*3+1,i*3,sep='')
      dates<-c(dates,temp)
    }
    oris<-get_origvar(dates,comp,repla = 'm',drop='times',add='credit',loc=3)
    ndf<-c()
    for(i in oris){
      vec<-get_deri(i,df)
      ndf<-cbind(ndf,vec)
    }
    ndf<-data.frame(ndf)
    ndf<-c2d_type(ndf,names(ndf),0)
    deri.vec<-agg_type(ndf,names(ndf),'sum')
  }else if(handle_logic(comp)=='c.u.t'){
    print("handling logic is combine type")
    if(length(date)==2)
      date<-paste(date[1],date[2],sep='')
    oris<-paste(c("credit_use","debit_use"),date,sep="")
    ndf<-data.frame(get_deri(oris[2],df),get_deri(oris[1],df))
    deri.vec<-combine_type(ndf,names(ndf),0)
  }else if(handle_logic(comp)=='u'){
    print("handling logic is use type")
    if(length(date)==2)
      date<-paste('sum',1,date[1],date[2],sep="") else
        date<-paste('sum',1,date,sep="")
      comp<-c("ac",date,comp)
      oris<-get_origvar(c("out","in"),comp,repla = 'use')
      ndf<-data.frame(get_deri(oris[1],df),get_deri(oris[2],df))
      deri.vec<-agg_type(ndf,names(ndf),'sum')
  }else if(handle_logic(comp)=='max_ac'){
    print("handling logic is which max")
    dates<-dates_handle(c(1,date))
    if("cout"%in%comp)cc<-"credit_out"
    if("cin"%in%comp)cc<-"credit_in"
    if("dout"%in%comp)cc<-"debit_out"
    if("din"%in%comp)cc<-"debit_in"
    if("dance"%in%comp)cc<-"debit_balance"
    comp<-c("I_ac",'m',cc)
    oris<-get_origvar(dates,comp,repla = 'm')
    deri.vec<-agg_type(df,oris,'wmax')
  }else if(handle_logic(comp)=='rec_ac'){
    print("handling logic is which max")
    dates<-dates_handle(c(1,date))
    if("cout"%in%comp)cc<-"credit_out"
    if("cin"%in%comp)cc<-"credit_in"
    if("dout"%in%comp)cc<-"debit_out"
    if("din"%in%comp)cc<-"debit_in"
    if("dance"%in%comp)cc<-"debit_balance"
    comp<-c("I_ac",'m',cc)
    oris<-get_origvar(dates,comp,repla = 'm')
    deri.vec<-agg_type(df,oris,'wrec')
  }else if(handle_logic(comp)=='tr'){
    print("handling logic is tr")
    dates<-c()
    for(i in 1:(date[2]%/%3)){
      dates<-c(dates,paste("sum",(i-1)*3+1,i*3,sep=""))
    }
    oris<-get_origvar(dates,comp,repla = 'm',drop="tr")
    temp<-data.frame(get_deri(oris[1],df),get_deri(oris[2],df))
    deri.vec<-tr_type(temp,names(temp))
  }else if(handle_logic(comp)=='b.sum'){
    print("handling logic is get discrete sum")
    dates<-dates_handle(date[1:2])
    if("cout"%in%comp)cc<-"credit_out"
    if("cin"%in%comp)cc<-"credit_in"
    if("dout"%in%comp)cc<-"debit_out"
    if("din"%in%comp)cc<-"debit_out"
    comp<-c("I_ac",'m',cc)
    oris<-get_origvar(dates,comp,repla = 'm')
    temp<-unlist(str_extract(deri.var,"\\d+[wt]"))
    temp<-unlist(str_extract(temp,"\\d+"))%>%as.numeric()
    if('w'%in%comp)
      money<-10000*temp else if('t'%in%comp)
        money<-1000*temp else
          money<-0
    temp<-c2d_type(df,oris,money)
    deri.vec<-agg_type(temp,names(temp),'sum')
  }else if(handle_logic(comp)=='b.sum2'){
    print("handling logic is get discrete sum")
    dates<-dates_handle(date[1:2])
    oris<-get_origvar(dates,c("I",comp),repla = 'num')
    temp<-c2d_type(df,oris,0)
    deri.vec<-agg_type(temp,names(temp),'sum')
  }else if(handle_logic(comp)=='sum.b'){
    print("handling logic is get discrete sum")
    if(length(date)==2)
      dates<-paste('sum',date[1],date[2],sep='')else
        dates<-paste('sum',date[1],date[2],date[3],sep='')
      
      if("cout"%in%comp)cc<-"credit_out"
      if("cin"%in%comp)cc<-"credit_in"
      if("dout"%in%comp)cc<-"debit_out"
      if("din"%in%comp)cc<-"debit_in"
      comp<-c("ac",'m',cc)
      oris<-get_origvar(dates,comp,repla = 'm')
      temp<-unlist(str_extract(deri.var,"\\d+[wt]"))
      temp<-unlist(str_extract(temp,"\\d+"))%>%as.numeric()
      if('w'%in%comp)
        money<-10000*temp else if('t'%in%comp)
          money<-1000*temp else
            money<-0
      temp<-get_deri(oris,df)
      temp[is.na(temp)]<-0
      deri.vec<-as.numeric(temp>money)
  }else if(handle_logic(comp)=='mthsnc'){
    print("handling logic is mthsnc")
    cc<-c()
    if("inre"%in%comp)cc<-c("in","repay")
    if("inout"%in%comp)cc<-c("in","out")
    if("outcash"%in%comp)cc<-c("out","cash")
    if(length(cc)==2){
      comps<-c("I_ac",'m',comp[2],cc[1])
      oris1<-get_origvar("m1",comps,repla = 'm')
      comps<-c("I_ac",'m',comp[2],cc[2])
      oris2<-get_origvar("m1",comps,repla = 'm')
      temp1<-match_oris(oris1,names(df),3)
      temp1<-temp1[order(temp1)]
      temp2<-match_oris(oris2,names(df),3)
      temp2<-temp2[order(temp2)]
      ndf<-c()
      dates<-c()
      for(i in 1:length(temp1)){
        temp<-df[,temp1[i]]-df[,temp2[i]]
        date<-mean(unlist(str_extract_all(temp1[i],"\\d+"))%>%as.numeric())
        dates<-c(dates,date)
        ndf<-cbind(ndf,temp)
      }
    }else{
      comps<-c("I_ac",'m',comp[2],comp[3])
      oris<-get_origvar("m1",comps,repla = 'm')
      temp<-match_oris(oris,names(df),3)
      temp<-temp[order(temp)]
      ndf<-c()
      dates<-c()
      for(i in 1:length(temp)){
        date<-mean(unlist(str_extract_all(temp[i],"\\d+"))%>%as.numeric())
        dates<-c(dates,date)
        ndf<-cbind(ndf,df[,temp[i]])
      }
    }
    ndf<-data.frame(ndf)
    temp<-agg_type(ndf,names(ndf),'wrec.b')
    deri.vec<-dates[temp]
  }else if(handle_logic(comp)=='mat.ty'){
    print("handling logic is matchtype")
    na<-c("I","C","M")
    names(na)<-c("auth_id","auth_cell","auth_mail")
    deri.vec<-c()
    for(i in names(na)){
      vec<-df[,i]%>%as.character()
      vec[vec=="1"]<-na[i]
      vec[vec=="0"]<-""
      deri.vec<-paste(deri.vec,vec,sep="")
    }
    deri.var[deri.var==""]<-"000"
  }else if(handle_logic(comp)=='a.s'){
    print("handling logic is matchtype")
    vec1<-df[,cat_var(comp[-2])]
    vec2<-df[,cat_var(c(comp[-1],"num"))]
    deri.vec<-df[,1]%>%as.character()
    deri.vec[vec1==1&vec2==1]<-"AS1"
    deri.vec[vec1==1&vec2>1]<-"AS2"
    deri.vec[vec1==0&vec2==0]<-"AS3"
    deri.vec[vec1==0&vec2>0]<-"AS4"
    deri.vec[is.na(vec1)|is.na(vec2)]<-"AS5"
  }else if(handle_logic(comp)=='age'){
    print("handling logic is get info from id")
    l<-unlist(lapply(df[,"id"], nchar))
    by<-str_extract(df[,"id"],"\\d{0,8}\\#")
    by<-str_extract(by,"\\d{0,4}")%>%as.numeric()
    ay<-str_extract(df[,"apply_date"],"\\d{0,4}")%>%as.numeric()
    deri.vec<-ay-by
    deri.vec[l!=18&l!=15]<--1
  }else if(handle_logic(comp)=='prop'){
    print("handling logic is prop")
    dates<-dates_handle(date)
    oris1<-get_origvar(dates,comp,drop="prop",repla = "m")
    oris2<-get_origvar(dates,comp,drop=c("prop","maxpay"),repla = "m",add="pay",loc=4)
    deri.vec<-df[,oris1]/df[,oris2]
    deri.vec[is.infinite(deri.vec)]<-NA
  }else if(handle_logic(comp)=='cd'){
    print("handling logic is get info from id")
    ay<-str_extract(df[,"apply_date"],"\\d{0,4}")%>%as.numeric()
    deri.vec<-ay-df[,"stab_cell_firsttime"]
  }else if(handle_logic(comp)=='cell'){
    print("handling logic is get info from cell")
    a<-(str_extract_all(df[,"cell"],as.character(date)))
    deri.vec<-unlist(lapply(a,length))
  }else if(handle_logic(comp)=='sex'){
    print("handling logic is get info from id")
    l<-unlist(lapply(df[,"id"], nchar))
    sex<-str_extract(df[,"id"],"\\#\\d\\#")
    sex<-str_extract(sex,"\\d")%>%as.numeric()
    deri.vec<-sex%%2
    deri.vec[l!=18&l!=15]<-0.5
  }else if(handle_logic(comp)=='mocs'){
    print("handling logic is sum all categories")
    dates<-dates_handle(date,type='cons')
    if("media"%in%comp)
      oris<-get_origvar(dates,comp,drop="is",repla = "m",add="xxx_visitdays",loc=4)else if("num"%in%comp)
        oris<-get_origvar(dates,comp,drop=c("is","num"),repla = "m",add="xxx_num",loc=4)else if("pay"%in%comp)
          oris<-get_origvar(dates,comp,drop=c("is","pay"),repla = "m",add="xxx_pay",loc=4)else if("visits"%in%comp)
            oris<-get_origvar(dates,comp,drop=c("is","visits"),repla = "m",add="xxx_visits",loc=4)else
              oris<-get_origvar(dates,comp,drop="is",repla = "m",add="xxx_visits",loc=4)
    
    vars<-match_oris(oris,names(df),3)
    deri.vec<-agg_type(df,vars,'sum')
    deri.vec[is.na(deri.vec)]<-0
    deri.vec[deri.vec>0]<-1
  }else if(handle_logic(comp)=='oct'){
    print("handling logic is sum all categories")
    dates<-dates_handle(date,type='cons')
    if('JJQC'%in%comp){
      c1<-comp
      c2<-comp
      c1[4]<-"JJJC"
      c2[4]<-"QCYP"
      ori1<-get_origvar(dates,c1,drop="is",repla = "m")
      ori2<-get_origvar(dates,c2,drop="is",repla = "m")
      vec<-as.numeric(agg_type(df,c(ori1,ori2),'sum')>0)
    }else{
      oris<-get_origvar(dates,comp,drop="is",repla = "m")
      vec<-as.numeric(df[,oris]>0)}
    vec.n<-rep("W3",length(vec))
    vec.n[vec>0]<-'W1'
    vec.n[vec==0]<-'W2'
    deri.vec<-vec.n
  }else if(handle_logic(comp)=='c&m'){
    print("handling logic is cons and media behave mode")
    dates<-dates_handle(date)
    oris1<-paste("is_media_",dates,sep="")
    if('n'%in%comp)
      oris2<-paste("is_cons_",dates,"_num",sep="")else
        oris2<-paste("is_cons_",dates,sep="")
    vec1<-get_deri(oris1,df)
    vec2<-get_deri(oris2,df)
    deri.vec<-rep(NA,length(vec1))
    deri.vec[vec1==1&vec2==1]<-"M1"
    deri.vec[vec1==1&vec2!=1]<-"M2"
    deri.vec[vec1!=1&vec2==1]<-"M3"
    deri.vec[vec1!=1&vec2!=1]<-"M4"
  }
  else if(handle_logic(comp)=='cn'){
    print("handling logic is count record categories")
    if(length(date)==3)
      dates<-dates_handle(date[1],type='cons')else if(length(date)==4)
        dates<-dates_handle(date[1:2],type='cons')else
          dates<-dates_handle(date,type='cons')
        if(length(date)>2){
          oris<-get_origvar(dates,comp,drop=c("tot","catenum","v",'n'),repla = "m",add="xxx_visits",loc=4)
          oris<-c(oris,oris<-get_origvar(dates,comp,drop=c("tot","catenum","v",'n'),repla = "m",add="xxx_num",loc=4))
          
          deri.vec<-rep(NA,length(df[,1]))
          all<-c()
          for(i in oris){
            temp<-match_oris(i,names(df),3)
            all<-c(all,temp)
          }
          for(j in temp){
            tem<-match_oris(j,all,4)
            tem<-tem[order(tem)]
            tem1<-match_oris(tem[1],all,2)
            
            tem1<-agg_type(df,tem1,'sum')
            tem2<-match_oris(tem[2],all,2)
            if(length(tem2)==1)
              tem2<-df[,tem2] else
                tem2<-agg_type(df,tem2,'sum')
            tem1[tem1>0]<-1
            tem2[tem2>0]<-1
            mp<-which(is.na(tem1)&is.na(tem2))
            temp<-rep(0,length(tem1))
            temp[mp]<-NA
            temp[tem1==date[3]&tem2==date[2]]<-1
            ndf<-data.frame(deri.vec,temp)
            deri.vec<-agg_type(ndf,names(ndf),'sum')
          }  
        }
        else if("consmedia"%in%comp){
          oris1<-paste("tot_media_m",date,"_catenum",sep="")
          if("vd"%in%comp)
            oris2<-paste("tot_cons_m",date,"_v_catenum",sep="")else
              oris2<-paste("tot_cons_m",date,"_n_catenum",sep="")
            vec1<-get_deri(oris1,df)
            vec2<-get_deri(oris2,df)
            deri.vec<-vec1+vec2
        }else{
          if("media"%in%comp){
            oris<-get_origvar(dates,comp,drop=c("tot","catenum"),repla = "m",add="xxx_visitdays",loc=4)
          }else if("cons"%in%comp){
            if("v"%in%comp|'visits'%in%comp)
              oris<-get_origvar(dates,comp,drop=c("tot","catenum","v",'visits'),repla = "m",add="xxx_visits",loc=4)else if('n'%in%comp|"num"%in%comp)
                oris<-get_origvar(dates,comp,drop=c("tot","catenum",'n',"num",'visits'),repla = "m",add="xxx_num",loc=4) else if('pay'%in%comp)
                  oris<-get_origvar(dates,comp,drop=c("tot","catenum","pay"),repla = "m",add="xxx_pay",loc=4) 
          }
          vars<-match_oris(oris,names(df),3)
          deri.vec<-agg_type(df,vars,'sum')
        }
  }else if(handle_logic(comp)=='max_beha'){
    print("handling logic is get max category")
    dates<-dates_handle(date,type='cons')
    if("media"%in%comp){
      oris<-get_origvar(dates,comp,drop=c("max","cate"),repla = "m",add="xxx_visitdays",loc=4)
    }else if("cons"%in%comp){
      if("v"%in%comp)
        oris<-get_origvar(dates,comp,drop=c("max","v","cate"),repla = "m",add="xxx_visits",loc=4)else if("paycate"%in%comp)
          oris<-get_origvar(dates,comp,drop=c("max","paycate"),repla = "m",add="xxx_pay",loc=4) else if("numcate"%in%comp)
            oris<-get_origvar(dates,comp,drop=c("max","numcate"),repla = "m",add="xxx_num",loc=4)
    }
    
    vars<-match_oris(oris,names(df),3)
    nam<-sapply(vars,function(s)unlist(str_extract_all(s,"[a-z,A-Z]+"))[3])
    temp<-agg_type(df,vars,"wmax")
    nam<-encode_cate(nam)
    names(nam)<-c()
    deri.vec<-nam[temp]
  }else if(handle_logic(comp)=='cc'){
    print("handling logic is cont cons")
    vec1<-get_deri("tot_cons_m3_num",df)
    vec2<-get_deri("tot_cons_m6_num",df)
    vec3<-get_deri("tot_cons_m12_num",df)
    deri.vec<-rep(NA,length(df[,1]))%>%as.character()
    deri.vec[vec1>0]<-"M5"
    deri.vec[vec1>0&vec2==vec1&vec3==vec2]<-"M1"
    deri.vec[vec1>0&vec2==vec1&vec3>vec2]<-"M2"
    deri.vec[vec1>0&vec2>vec1&vec3==vec2]<-"M3"
    deri.vec[vec1>0&vec2>vec1&vec3>vec2]<-"M4"
  }
  return(deri.vec)
}

decode_type<-function(df,ori){
  temp<-df[,ori]
  temp[df[,ori]==99]<-NA
  temp[df[,ori]==56]<-15000000
  temp[df[,ori]%in%47:55]<-(temp[df[,ori]%in%47:55] -47)*1000000+1500000
  temp[df[,ori]%in%38:46]<-(temp[df[,ori]%in%38:46] -38)*100000+150000
  temp[df[,ori]%in%20:37]<-(temp[df[,ori]%in%20:37] -20)*5000+12500
  temp[df[,ori]%in%0:19]<-(temp[df[,ori]%in%0:19] -0)*500+250
  return(temp)
}

combine_type<-function(df,oris,a=NA){
  temp<-df
  temp[is.na(temp[,oris])]<-0
  temp[,oris[1]]<-as.numeric(temp[,oris[1]]>a)
  temp[,oris[2]]<-as.numeric(temp[,oris[2]]>a)
  t.t<-paste(temp[,oris[1]],temp[,oris[2]],sep='')
  tt<-t.t
  tt[which(t.t=='01')]<-1
  tt[which(t.t=='10')]<-2
  tt[which(t.t=='11')]<-3
  tt[which(t.t=='00')]<-4
  return(tt%>%as.numeric())
}

agg_type<-function(df,oris,type){
  if(length(oris)==1){
    temp<-df[,oris]
    return(temp)
  }else{
    temp<-apply(df[,oris], 1, function(s)length(which(is.na(s))))
    mp<-which(temp==length(oris))
    if(type=='wmax'|type=='wmin'){
      df<-data.frame(df[,oris],min(df[,oris],na.rm = T)-1)
      temp<-unlist(apply(df,1,function(s)agg.types[[type]](s)))
      temp[temp==length(oris)+1]<-NA
    }else if(type=='wrec'){
      temp<-apply(df[,oris],1,function(s)min(which(!is.na(s))))
      temp<-temp-1
    }else if(type=='wrec.b'){
      temp<-apply(df[,oris],1,function(s)min(which(s>0)))
      temp<-temp
    }else{
      temp<-apply(df[,oris],1,function(s)agg.types[[type]](s,na.rm=T))
    }
    temp[mp]<-NA
    temp[is.infinite(temp)]<-NA
    return(temp)}
}

tr_type<-function(df,oris){
  vec1<-df[,oris[1]] 
  vec2<-df[,oris[2]] 
  temp<-rep(0,length(vec1))
  temp[which(is.na(vec1)&!is.na(vec2))]<-1
  temp[which(!is.na(vec1)&is.na(vec2))]<-2
  temp[which(vec2/vec1<1)]<-3
  temp[which(vec2/vec1==1)]<-4
  temp[which(vec2/vec1>1)]<-5
  return(temp)
}
c2d_type<-function(df,oris,a){
  if(length(a)>1){
    a<-a[order(a)]
    for(i in oris){
      vec<-df[,i]
      temp<-vec
      vec[temp<=a[1]]<-2
      vec[temp>a[1]&temp<a[2]]<-0
      vec[temp>=a[2]]<-1
      df[,i]<-vec
    }}else{
      for(i in oris){
        df[,i]<-as.numeric(df[,i]>a)
      }
    }
  return(df[,oris])
}

encode_cate<-function(vars,type='cons'){
  code<-c()
  codes<-c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11",
           "C12","C13","C14","C15","C16","C17","C18","C19","C20","C21",
           "C22","C23","C24","C25","C26","C27","C28","C29")
  names(codes)<-c("BDSH","BXLC","CCLY","CWSH","DNBG","FC","FZPS","GHHZ","JJJC",
                  "JJJF","JYDQ","JYPX","MSTC","MYYP",
                  "QCYP","QT","RYBH","SC","SJSJPJ","SM","TX","WHYL","WLYXXNWP"
                  ,"X","XB","YDHW","YLBJ","ZBGJS","ZBSS")
  if(type=='cons'){
    for(i in vars){
      code<-c(code,codes[i])
    }
  }
  return(code)
}

cat_var<-function(vars){
  temp<-vars[1]
  for(i in vars[-1]){
    temp<-paste(temp,i,sep ="_")
  }
  return(temp)
}


dates_handle<-function(date,se=NA,type="ac"){
  if(length(date)==4)
    date<-c(paste(date[1],date[2],sep="")%>%as.numeric(),paste(date[3],date[4],sep="")%>%as.numeric())
  if(length(date)==3)
    date<-c(date[1],paste(date[2],date[3],sep="")%>%as.numeric())
  if(length(date)==2&date[2]%in%c(2,8))
    date<-paste(date[1],date[2],sep="")%>%as.numeric()
  if(!is.na(se)){
    dates=paste('m',seq(1,max(date),se),'m',seq(se,max(date),se),sep="")
  }else{
    if(type=='ac')
      dates<-min(date):max(date)else
        dates<-date
      dates<-paste('m',dates,sep='')
  }
  return(dates)
}

get_origvar<-function(dates,comp,repla="m",drop="",add="",loc=1){
  comp<-unique(comp)
  comp[(loc+length(add)):(length(comp)+length(add))]<-comp[loc:length(comp)]
  comp[loc:(loc+length(add)-1)]<-add
  comp<-comp[!is.na(comp)]
  comp<-comp[!comp%in%c(drop,"")]
  ori<-c()
  for(i in dates){
    temp<-comp
    temp[temp%in%repla]<-i
    ori<-c(ori,cat_var(temp))
  }
  return(ori)
}

match_oris<-function(oris,vars,l){
  comp<-unlist(str_extract_all(oris,"[a-z,A-Z,0-9]+"))
  loc<-1:length(comp)
  loc<-loc[-l]
  for(i in vars){
    compi<-unlist(str_extract_all(i,"[a-z,A-Z,0-9]+"))
    if(length(compi)!=length(comp))
      vars<-vars[vars!=i]else{
        for(j in loc){
          if(comp[j]!=compi[j])vars<-vars[vars!=i]
        }}
  }
  return(vars)
}

define_types_by_predoc<-function (df, pre.types) 
{
  unpre <- c()
  for (i in names(df)) {
    tryCatch({
      df[, i] <- as.character(df[, i])
      mp <- which(pre.types[, 1] == i)
      if (length(mp) == 1) {
        type <- pre.types[mp, 2]
        if (type %in% c("numeric", "int", "float", "double", 
                        0)) {
          df[, i] <- as.numeric(df[, i])
        }
        else if (type %in% c("category", "discrete", 
                             "string", 1)) {
          if (length(unique(df[, i])) > 40) {
            unpre <- c(unpre, i)
            warning(print("something wrong"))
          }
          df[, i] <- as.factor(df[, i])
        }
        else {
          stop("predefined data type is wrong")
        }
      }
      else {
        unpre <- c(unpre, i)
      }
    }, warning = function(w) {
      unpre <- c(unpre, i)
      print(paste(i, "'s variable type is wrong", sep = ""))
    })
  }
  if (length(unpre) > 0) {
    idx <- 1
    for (j in unpre) {
      mp <- which(!is.na(df[, j]))[1:5]
      l <- length(unique(df[, j]))
      paste(j, "'s unique values is", l, "the num is", 
            idx) %>% print()
      print(df[mp, j])
      idx <- 1 + idx
    }
    print(unpre)
  }
  return(df)
}

miss_handle<-function (df, miss.value = NA) 
{
  df <- data.frame(df)
  for (i in names(df)) {
    if (is.factor(df[, i])) {
      df[, i] <- as.character(df[, i])
      mp <- is.na(df[, i]) %>% which()
      df[mp, i] <- miss.value
      df[, i] <- as.factor(df[, i])
    }
    else {
      temp <- df[, i]
      mp <- is.na(df[, i]) %>% which()
      df[mp, i] <- miss.value
    }
  }
  if (length(df) == 1) 
    df <- df[, 1]
  return(df)
}
xg_predict<-function (df, useful, bst) 
{
  train.mat <- sparse.model.matrix(~., data = df[, useful])
  dtrain <- xgb.DMatrix(data = train.mat, missing = m.v)
  set.seed(123)
  prob <- predict(bst, newdata = dtrain)
  return(prob)
}