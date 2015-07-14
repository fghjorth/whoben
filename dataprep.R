setwd("~/GitHub/whoben") #set working directory here

#####################################
# READ IN DATA
#####################################

require(foreign)
#read in data
gd<-read.spss("rawdata/prel_s2_hjorth_20140505.sav",use.value.labels=F,to.data.frame=T)
gdlabs<-read.spss("rawdata/prel_s2_hjorth_20140505.sav",use.value.labels=T,to.data.frame=T)

#get var labels
varnames<-as.data.frame(attr(read.spss("prel_s2_hjorth_20140505.sav", to.data.frame=FALSE, use.value.labels=FALSE), "variable.labels"))

#####################################
# RECODE VARIABLES
#####################################

#treatment variable
table(gdlabs$s2gr1)
table(as.numeric(gd$s2gr1))
gd$bulg01<-(as.numeric(gd$s2gr1)-2)*-1
gd$bulgfac<-factor(gd$bulg01,labels=c("Dutch","Bulgarian"))

#was prejudice scale administered before questions about support?
table(gdlabs$s2gr4)
table(as.numeric(gd$s2gr4))
gd$prej.bef01<-(as.numeric(gd$s2gr4)-2)*-1

#outcome vars
table(gdlabs$s2_q44)
table(as.numeric(gd$s2_q44))
gd$oppose<-(as.numeric(gd$s2_q44)-1)/4
gd$oppose01<-ifelse(as.numeric(gd$s2_q44)>3,1,0)

#support reduced benefits for all?
gd$cutall<-(as.numeric(gd$s2_q45)-5)/-4
table(gd$cutall)

#kids
table(gdlabs$s2gr2)
table(as.numeric(gd$s2gr2))
gd$nkids<-as.numeric(gd$s2gr2)

#kids stay home
table(gdlabs$s2gr3)
table(as.numeric(gd$s2gr3))
gd$stayhome<-(as.numeric(gd$s2gr3)-1)

#prejudice
table(gdlabs$prejudice2)
table(as.numeric(gd$prejudice2))
require(psy)

gd$prej<-((as.numeric(gd$prejudice1)+as.numeric(gd$prejudice2)-as.numeric(gd$prejudice3)-as.numeric(gd$prejudice4)-as.numeric(gd$prejudice5)-as.numeric(gd$prejudice6))-6)*(-1/24)
prejalpha<-cronbach(data.frame(cbind(as.numeric(gd$prejudice1),as.numeric(gd$prejudice2),-as.numeric(gd$prejudice3),-as.numeric(gd$prejudice4),-as.numeric(gd$prejudice5),-as.numeric(gd$prejudice6))))$alpha

gd$prejred<-(-as.numeric(gd$prejudice2)+as.numeric(gd$prejudice4)+as.numeric(gd$prejudice5)+3)*(1/12)
prejredalpha<-cronbach(data.frame(cbind(as.numeric(gd$prejudice2),-as.numeric(gd$prejudice4),-as.numeric(gd$prejudice5))))$alpha

#economic ideology
table(gd$s2_q32_5)
table(as.numeric(gd$s2_q32_1))
gd$eid1<-ifelse(as.numeric(gd$s2_q32_1)<6,(as.numeric(gd$s2_q32_1)-5)/-4)
gd$eid2<-ifelse(as.numeric(gd$s2_q34_1)<6,(as.numeric(gd$s2_q34_1)-5)/-4)
gd$eid3<-ifelse(as.numeric(gd$s2_q34_4)<6,(as.numeric(gd$s2_q34_4)-1)/4)
gd$eid4<-ifelse(as.numeric(gd$s2_q34_5)<6,(as.numeric(gd$s2_q34_5)-5)/-4)
gd$eid5<-ifelse(as.numeric(gd$s2_q32_5)<6,(as.numeric(gd$s2_q32_5)-1)/4)
require(psych)
eidalpha<-alpha(data.frame(a=gd$eid1,b=gd$eid2,c=gd$eid3,d=gd$eid4,e=gd$eid5))$total$std.alpha #this gives a nice alpha
gd$eid<-(gd$eid1+gd$eid2+gd$eid3+gd$eid4+gd$eid5)/5
hist(gd$eid)

#demographics
table(gdlabs$sex)
table(gd$sex)
gd$female<-(as.numeric(gd$sex))
table(gdlabs$edu)
table(gd$age2013)
table(as.numeric(gd$edu))
gd$edulvl<-(as.numeric(gd$edu)-1)/8
gd$someuni<-ifelse(as.numeric(gd$edu)>6,1,0)
table(gdlabs$lmsit)
table(gd$lmsit)
gd$onlabmkt<-ifelse(gd$lmsit==1,1,0)
table(gdlabs$income_own_m)
gd$income<-ifelse(gd$income_own_m<14,(gd$income_own_m-1)/12,NA)

#####################################
# SAVE DATA
#####################################

saveRDS(gd,"whobenefits_data.rds")