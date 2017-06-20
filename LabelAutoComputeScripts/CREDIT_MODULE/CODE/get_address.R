#get unique address
provinces<-negInfo$hreg_addr_prov%>%unique%>%str_replace_all(.,'省|市|自治区|壮族|维吾尔|回族','')
cities   <-negInfo$hreg_addr_city%>%unique%>%str_replace_all(.,'市','')
district <-negInfo$hreg_addr_district%>%unique
#clean the address
provinces<-provinces[provinces!='NA'&provinces!='NULL'&!is.na(provinces)&!is.null(provinces)]
cities   <-cities[cities!='NA'&cities!='NULL'&!is.na(cities)&!is.null(cities)]
district <-district[district!='NA'&district!='NULL'&!is.na(district)&!is.null(district)]
#save the address
addressSets<-suppressWarnings(data.table(provinces,cities,district)) 
saveOBJ(addressSets,HP_SHARE_DIR)

 
