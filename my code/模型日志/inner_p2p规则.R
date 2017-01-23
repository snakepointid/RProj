temp<-filter(missp2p, max_cons_m3_paycate=="C21"|
               max_cons_m3_paycate=="C3"|
               max_cons_m3_paycate=="C28",
               auth_key_relation==0|
               auth_key_relation==1|
               auth_key_relation==110,
               tot_cons_m12_visits< 96)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()
table(missp2p[,"def"])
13/1316
temp<-filter(missp2p,max_cons_m12_paycate=="C3"|
               max_cons_m12_paycate=="C21"|
               max_cons_m12_paycate=="C28",
             max_cons_m6_num_prop>=0.5857)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()
14/1316
temp<-filter(missp2p,cons_m6_TX_num>=19.5)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()



temp<-filter(missp2p, max_cons_m3_visits< 5.5,
             MatchType=="C"|
               MatchType=="I"|
               MatchType=="M",
             max_cons_m3_pay< 0.75,
             stab_cell_firsttime>2012)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()

temp<-filter(missp2p,max_cons_m3_visits< 5.5,
             MatchType=="C"|
               MatchType=="I"|
               MatchType=="M",
             max_cons_m3_pay>=150)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()

temp<-filter(missp2p,  max_cons_m3_pay< 0.25,
             auth_key_relation==0|
               auth_key_relation==1,
             stab_cell_firsttime>2012)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()
43/1316
temp<-filter(missp2p,  max_cons_m3_visits< 5.5,
             auth_key_relation==0|
               auth_key_relation==1,
             max_cons_m3_pay>=150)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()
22/1316
temp<-filter(missp2p, max_cons_m3_visits< 5.5,
             auth_key_relation==0|
               auth_key_relation==1,
             stab_cell_firsttime>2012)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()

################################################id_flag=N
temp<-filter(missp2p,tot_cons_m12_p_catenum< 3.5,
             tot_cons_m6_num>15.5)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()
table(missp2p[,"def"])%>%prop.table()
length(missp2p[,"def"])
18/647
temp<-filter(missp2p, max_cons_m3_paycate=="C21"|
               max_cons_m3_paycate=="C88",
             stab_cell_firsttime>2012)
table(temp[,"def"])
table(temp[,"def"])%>%prop.table()

################################################name_flag=N
temp<-filter(missp2p,max_cons_m3_paycate=="C3"|
               max_cons_m3_paycate=="C21"|
               max_cons_m3_paycate=="C23",
             tot_consmedia_m12_vd_catenum< 8.5)
table(missp2p[,"def"])%>%prop.table()
table(temp[,"def"])%>%prop.table()
table(temp[,"def"])
length(missp2p[,"def"])
13/573
