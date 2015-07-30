setwd("~/GitHub/whoben") #set working directory here

#####################################
# LOAD PACKAGES
#####################################

require(stargazer)
require(xtable)
require(ggplot2)
require(effects)
require(foreign)
require(dplyr)


#####################################
# READ IN SAVED DATA
#####################################

gd<-readRDS("whobenefits_data.rds")


#####################################
# DESCRIPTIVE STATISTICS
#####################################

# Table 3
demtab<-data.frame(var=c("Age (mean)","Gender (pct. female)","Education (pct. some tertiary)"),sample=NA,pop=NA,pval=NA)
demtab$var<-as.character(demtab$var)
str(demtab)
demtab$sample[1]<-mean(gd$age2013,na.rm=T)
demtab$sample[2]<-100*prop.table(table(gd$female))[2]
demtab$sample[3]<-100*prop.table(table(gd$edu>6))[2]
#demtab$sample[4]<-prop.table(table(gd$income_own_m>8))[2]
#avg age of all swedes is 41.2 - but we need avgs age of voting age pop, ie 18+. we don't have the exact number, but let's make the simplifying assumption that avg age among 0-17 is 9. then voting age pop age avg is:
votingpopageavg<-(41.2*9645-1952*9)/(9645-1952)
demtab$pop<-c(votingpopageavg,100*.500001,100*.253)
demtab$pval<-as.numeric(c(t.test(gd$age2013,mu=demtab$pop[1])$p.value,t.test(gd$female==0,mu=demtab$pop[2])$p.value,t.test(gd$edu>6,mu=demtab$pop[3])$p.value))+.00001
demtabsg<-stargazer(demtab[,1:3],summary=F,digits=1,digits.extra=2,covariate.labels=c("Variable","Sample","Population"),title="Sample demographics compared with Swedish voting-age population",label="demtab",font.size="footnotesize",align=T)
writeLines(demtabsg,con="tables/demtab.txt")


#####################################
# REGRESSION MODELS
#####################################

# Main results: Table 4
summary(m07<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid,data=gd))
summary(m09<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+stayhome+bulg01+nkids,data=gd))
summary(m10<-lm(oppose~female+age2013+someuni+onlabmkt+eid+stayhome+prej*bulg01+prej*nkids,data=gd))
summary(m11<-lm(oppose~female+age2013+someuni+onlabmkt+prej+stayhome+eid*bulg01+eid*nkids,data=gd))

regtab1<-stargazer(m07,m09,m10,m11,style="apsr",title="Models of welfare chauvinism",intercept.bottom=F,digits=3,dep.var.labels="Opposed to cross-border welfare rights",dep.var.labels.include=T,font.size="footnotesize",label="regtab1",column.sep.width="5pt",covariate.labels=c("Intercept","Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","Economic conservatism (EC)","Children stay","Nationality: Bulgarian","No. of children (NC)","EP$\\times$Nationality: Bulgarian","EP$\\times$NC","EC$\\times$Nationality: Bulgarian","EC$\\times$NC"),omit.stat=c("f","ser"),star.cutoffs=c(.05,.01,.001),align=T)
writeLines(regtab1,con="tables/whobenefits_regtab1.txt")     

#Appendix table 1: Tests for sequence effect
summary(mseqeff1<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+prej.bef01+stayhome+bulg01+nkids,data=gd))
summary(mseqeff2<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+prej.bef01+stayhome+bulg01+nkids+prej.bef01:stayhome,data=gd))
summary(mseqeff3<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+prej.bef01+stayhome+bulg01+nkids+prej.bef01:bulg01,data=gd))
summary(mseqeff4<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+prej.bef01+stayhome+bulg01+nkids+prej.bef01:nkids,data=gd))

summary(mseqeff5<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+prej.bef01+stayhome+bulg01+nkids+prej.bef01:prej,data=gd))

regtabseqeff<-stargazer(mseqeff1,mseqeff5,mseqeff2,mseqeff3,mseqeff4,style="apsr",title="Tests of sequence effects",
                        intercept.bottom=F,digits=3,dep.var.labels="Opposed to cross-border welfare rights",
                        dep.var.labels.include=T,font.size="footnotesize",label="regtabseqeff",column.sep.width="1pt",
                        omit.stat=c("f","ser"),star.cutoffs=c(.05,.01,.001),align=T,
                        covariate.labels=c("Intercept","Gender (f)","Age","Education (some uni.)","Employed",
                                           "Ethnic prejudice (EP)","Economic conservatism (EC)","EP scale before probe (Before)",
                                           "Children stay home (SH)","Nationality: Bulgarian","No. of children (NC)",
                                           "Before$\\times$EP","Before$\\times$SH","Before$\\times$Bulgarian","Before$\\times$NC"))

writeLines(regtabseqeff,con="tables/whobenefits_regtabseqeff.txt")


#Appendix table 3: Logits
summary(m07.logit<-glm(oppose01~female+age2013+someuni+onlabmkt+prej+eid,data=gd,family=binomial()))
summary(m09.logit<-glm(oppose01~female+age2013+someuni+onlabmkt+prej+eid+stayhome+bulg01+nkids,data=gd,family=binomial()))
summary(m10.logit<-glm(oppose01~female+age2013+someuni+onlabmkt+eid+stayhome+prej*bulg01+prej*nkids,data=gd,family=binomial()))
summary(m11.logit<-glm(oppose01~female+age2013+someuni+onlabmkt+prej+stayhome+eid*bulg01+eid*nkids,data=gd,family=binomial()))

regtablogits<-stargazer(m07.logit,m09.logit,m10.logit,m11.logit,style="apsr",title="Models of welfare chauvinism, logistic regressions",intercept.bottom=F,digits=3,dep.var.labels="Opposed to cross-border welfare rights",dep.var.labels.include=T,font.size="footnotesize",label="regtablogits",column.sep.width="5pt",covariate.labels=c("Intercept","Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","Economic conservatism (EC)","Children stay","Nationality: Bulgarian","No. of children (NC)","EP$\\times$Nationality: Bulgarian","EP$\\times$NC","EC$\\times$Bulgarian","EC$\\times$NC"),omit.stat=c("f","ser"),star.cutoffs=c(.05,.01,.001),align=T)
writeLines(regtablogits,con="tables/whobenefits_regtablogits.txt") 

#Appendix table 4: Reduced measure of ethnic prejudice
summary(m07.prejred<-lm(oppose~female+age2013+someuni+onlabmkt+prejred+eid,data=gd))
summary(m09.prejred<-lm(oppose~female+age2013+someuni+onlabmkt+prejred+eid+stayhome+bulg01+nkids,data=gd))
summary(m10.prejred<-lm(oppose~female+age2013+someuni+onlabmkt+eid+stayhome+prejred*bulg01+prejred*nkids,data=gd))
summary(m11.prejred<-lm(oppose~female+age2013+someuni+onlabmkt+prejred+stayhome+eid*bulg01+eid*nkids,data=gd))

regtabprejred<-stargazer(m07.prejred,m09.prejred,m10.prejred,m11.prejred,style="apsr",title="Models of welfare chauvinism, reduced measure of ethnic prejudice",intercept.bottom=F,digits=3,dep.var.labels="Opposed to cross-border welfare rights",dep.var.labels.include=T,font.size="footnotesize",label="regtabprejred",column.sep.width="5pt",covariate.labels=c("Intercept","Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","Economic conservatism (EC)","Children stay","Nationality: Bulgarian","No. of children (NC)","EP$\\times$Bulgarian","EP$\\times$NC","EC$\\times$Bulgarian","EC$\\times$NC"),omit.stat=c("f","ser"),star.cutoffs=c(.05,.01,.001),align=T)
writeLines(regtabprejred,con="tables/whobenefits_regtabprejred.txt") 

#Appendix table 5: full factorial models
gd$nkidsfac<-factor(gd$nkids)
summary(mfullfac1<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+bulg01*nkidsfac,data=subset(gd,stayhome==1)))
summary(mfullfac2<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+bulg01*nkidsfac,data=subset(gd,stayhome==0)))

regtabfullfac<-stargazer(mfullfac1,mfullfac2,style="apsr",title="Models of welfare chauvinism, full factorial models",
                         intercept.bottom=F,digits=3,dep.var.labels="Opposed to cross-border welfare rights",
                         dep.var.labels.include=T,font.size="footnotesize",label="regtabfullfac",column.sep.width="5pt",
                         omit.stat=c("f","ser"),star.cutoffs=c(.05,.01,.001),align=T,
                         column.labels=c("Children stay","Not mentioned","b"),
                         covariate.labels=c("Intercept","Gender (f)","Age","Education (some uni.)","Employed",
                                            "Ethnic prejudice (EP)","Economic conservatism (EC)","Nationality: Bulgarian",
                                            "No. of children (NC)=2","NC=3","NC=4","NC=5","Bulgarian$\\times$NC=2",
                                            "Bulgarian$\\times$NC=3","Bulgarian$\\times$NC=4","Bulgarian$\\times$NC=5"))

writeLines(regtabfullfac,con="tables/whobenefits_regtabfullfac.txt") 

summary(mfullfac3<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+bulg01*nkids+stayhome,data=gd))
summary(mfullfac4<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+bulg01*stayhome+nkids,data=gd))
summary(mfullfac5<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+stayhome*nkids+bulg01,data=gd))

regtabfullfaclin<-stargazer(mfullfac3,mfullfac4,mfullfac5,style="apsr",title="Models of welfare chauvinism, full factorial models",
                            intercept.bottom=F,digits=3,dep.var.labels="Opposed to cross-border welfare rights",
                            dep.var.labels.include=T,font.size="footnotesize",label="regtabfullfac",column.sep.width="5pt",
                            omit.stat=c("f","ser"),star.cutoffs=c(.05,.01,.001),align=T,
                            column.labels=c("Children stay","Not mentioned","b"),order=c(1:8,10,12,9,11,13),
                            covariate.labels=c("Intercept","Gender (f)","Age","Education (some uni.)","Employed",
                                               "Ethnic prejudice (EP)","Economic conservatism (EC)","Nationality: Bulgarian",
                                               "No. of children (NC)","Children stay home (SH)","SH$\\times$NC",
                                               "SH$\\times$Bulgarian","NC$\\times$Bulgarian"))

writeLines(regtabfullfaclin,con="tables/whobenefits_regtabfullfaclin.txt")

#Appendix tables 6-9: Full factorial anovas
summary(fullfacanova<-aov(oppose~female+age2013+someuni+onlabmkt+prej+eid+bulg01*nkids*stayhome,data=gd))
summary(fullfacanova2<-aov(oppose~female+age2013+someuni+onlabmkt+prej+eid+bulg01*factor(nkids)*stayhome,data=gd))

summary(fullfacanovah3a<-aov(oppose~female+age2013+someuni+onlabmkt+prej*factor(nkids)+prej*bulg01+eid+bulg01*factor(nkids)*stayhome,data=gd))
summary(fullfacanovah3b<-aov(oppose~female+age2013+someuni+onlabmkt+prej+eid*factor(nkids)+eid*bulg01+bulg01*factor(nkids)*stayhome,data=gd))

anovatab<-xtable(fullfacanova,caption="Full factorial ANOVA on all experimental conditions with covariates (number of children as interval scale)",label="fullfac")
rownames(anovatab)<-c("Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","Economic conservatism (EC)","Bulgarian","No. of children (NC)","Children stay (CS)","Bulgarian*NC","Bulgarian * CS","NC * Children stay","Bulgarian * NC * SC","Residuals")
writeLines(print(anovatab),con="tables/whobenefits_anovatab.txt")

anovatab2<-xtable(fullfacanova2,caption="Full factorial ANOVA on all experimental conditions with covariates (number of children as nominal scale)",label="fullfac2")
rownames(anovatab2)<-c("Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","Economic conservatism (EC)","Bulgarian","No. of children (NC)","Children stay (CS)","Bulgarian*NC","Bulgarian * CS","NC * Children stay","Bulgarian * NC * SC","Residuals")
writeLines(print(anovatab2),con="tables/whobenefits_anovatab2.txt")

anovatabh3a<-xtable(fullfacanovah3a,caption="Full factorial ANOVA, interactions with ethnic prejudice",label="fullfach3a")
rownames(anovatabh3a)<-c("Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","No. of children (NC)","Bulgarian","Economic conservatism (EC)","Children stay (CS)","EP * NC","EP * Bulgarian","Bulgarian * NC","Bulgarian * CS","NC * Children stay","Bulgarian * NC * SC","Residuals")
writeLines(print(anovatabh3a),con="tables/whobenefits_anovatabh3a.txt")

anovatabh3b<-xtable(fullfacanovah3b,caption="Full factorial ANOVA, interactions with economic conservatism",label="fullfach3b")
rownames(anovatabh3b)<-c("Gender (f)","Age","Education (some uni.)","Employed","Ethnic prejudice (EP)","Economic conservatism (EC)","No. of children (NC)","Bulgarian","Children stay (CS)","EC * NC","EC * Bulgarian","Bulgarian * NC","Bulgarian * CS","NC * Children stay","Bulgarian * NC * SC","Residuals")
writeLines(print(anovatabh3b),con="tables/whobenefits_anovatabh3b.txt")

# is number of kids nonlinear?
summary(m09kidssq<-lm(oppose~female+age2013+someuni+onlabmkt+prej+eid+stayhome+bulg01+nkids+I(nkids^2),data=gd))
print(xtable(anova(m09,m09kidssq)))


#####################################
# PLOTS
#####################################

# Code for preparing figures 2-4

#predicted effects for interaction btw cues and prej
prejintbulgeffs<-Effect(c("prej","bulg01"),m10,se=T,xlevels=list(prej=0:1,bulg01=0:1))$fit
prejintkideffs<-Effect(c("prej","nkids"),m10,se=T,xlevels=list(prej=0:1))$fit

#subtract minimum predicted values to get only treatment effects
prejintbulgeffs[c(1,3)]<-prejintbulgeffs[c(1,3)]-prejintbulgeffs[1]
prejintbulgeffs[c(2,4)]<-prejintbulgeffs[c(2,4)]-prejintbulgeffs[2]
prejintkideffs[c(1,3,5,7,9)]<-prejintkideffs[c(1,3,5,7,9)]-prejintkideffs[1]
prejintkideffs[c(2,4,6,8,10)]<-prejintkideffs[c(2,4,6,8,10)]-prejintkideffs[2]

prejintexpcoefs<-data.frame(cond=c("Dutch","Dutch","Bulgarian","Bulgarian",1,1,2,2,3,3,4,4,5,5),
                            condorder=c(1,1,2,2,1,1,2,2,3,3,4,4,5,5),
                            est=c(prejintbulgeffs,prejintkideffs),
                            se=c(Effect(c("prej","bulg01"),m10,se=T,xlevels=list(prej=0:1,bulg01=0:1))$se[1:4],Effect(c("prej","nkids"),m10,se=T,xlevels=list(prej=0:1))$se[1:10]),
                            exp=c(rep("Nationality",4),rep("Number of children",10)),
                            prej=rep(c("Minimum ethnic prejudice","Maximum ethnic prejudice"),7))

#predicted effects for interaction btw cues and eid
eidintbulgeffs<-Effect(c("eid","bulg01"),m11,se=T,xlevels=list(eid=0:1,bulg01=0:1))$fit
eidintkideffs<-Effect(c("eid","nkids"),m11,se=T,xlevels=list(eid=0:1))$fit

#subtract minimum predicted values to get only treatment effects
eidintbulgeffs[c(1,3)]<-eidintbulgeffs[c(1,3)]-eidintbulgeffs[1]
eidintbulgeffs[c(2,4)]<-eidintbulgeffs[c(2,4)]-eidintbulgeffs[2]
eidintkideffs[c(1,3,5,7,9)]<-eidintkideffs[c(1,3,5,7,9)]-eidintkideffs[1]
eidintkideffs[c(2,4,6,8,10)]<-eidintkideffs[c(2,4,6,8,10)]-eidintkideffs[2]

eidintexpcoefs<-data.frame(cond=c("Dutch","Dutch","Bulgarian","Bulgarian",1,1,2,2,3,3,4,4,5,5),
                           condorder=c(1,1,2,2,1,1,2,2,3,3,4,4,5,5),
                           est=c(eidintbulgeffs,eidintkideffs),
                           se=c(Effect(c("eid","bulg01"),m10,se=T,xlevels=list(eid=0:1,bulg01=0:1))$se[1:4],Effect(c("eid","nkids"),m10,se=T,xlevels=list(eid=0:1))$se[1:10]),
                           exp=c(rep("Nationality",4),rep("Number of children",10)),
                           eid=rep(c("Minimum economic conservatism","Maximum economic conservatism"),7))

t90<-1.65
t95<-1.96

# Figure 2

ggplot(expcoefs,aes(x=reorder(cond,condorder),y=est)) +
  geom_hline(yintercept=0,linetype="dashed",color="dark gray") +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymax=est+t95*se,ymin=est-t95*se),width=0) +
  geom_errorbar(aes(ymax=est+t90*se,ymin=est-t90*se),width=0,size=1.2) +
  facet_grid(.~exp,scales="free",space="free_x") +
  ylab("Effect on expressed Welfare chauvinism") +
  xlab("") +
  ylim(c(-0.05,.15)) +
  theme_bw()
ggsave(file="figures/expplot.pdf",height=4,width=10)

# Figure 3

ggplot(prejintexpcoefs,aes(x=reorder(cond,condorder),y=est)) +
  geom_hline(yintercept=0,linetype="dashed",color="dark gray") +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymax=est+t95*se,ymin=est-t95*se),width=0) +
  geom_errorbar(aes(ymax=est+t90*se,ymin=est-t90*se),width=0,size=1.2) +
  facet_grid(prej~exp,scales="free_x",space="free_x") +
  ylab("Effect on expressed Welfare chauvinism") +
  xlab("") +
  ylim(c(-0.1,.25)) +
  theme_bw()
ggsave(filename="figures/prejintexpplot.pdf",height=6,width=10)

# Figure 4

ggplot(eidintexpcoefs,aes(x=reorder(cond,condorder),y=est)) +
  geom_hline(yintercept=0,linetype="dashed",color="dark gray") +
  geom_point(size=2.5) +
  geom_errorbar(aes(ymax=est+t95*se,ymin=est-t95*se),width=0) +
  geom_errorbar(aes(ymax=est+t90*se,ymin=est-t90*se),width=0,size=1.2) +
  facet_grid(eid~exp,scales="free_x",space="free_x") +
  ylab("Effect on expressed Welfare chauvinism") +
  xlab("") +
  ylim(c(-0.1,.25)) +
  theme_bw()
ggsave(filename="figures/eidintexpplot.pdf",height=6,width=10)

# Write function for margins plot
margins.twoway<-function(model, xterm, zterm, zseq){
  coefs<-coef(model)
  cov<-vcov(model)
  intterm<-ifelse(is.na(coefs[paste(xterm,zterm,sep=":")]),paste(zterm,xterm,sep=":"),paste(xterm,zterm,sep=":"))
  dy.dx<-coefs[xterm]+coefs[intterm]*zseq
  se.dy.dx<-sqrt(cov[xterm,xterm]+zseq^2*cov[intterm,intterm]+zseq*2*cov[xterm,intterm])
  margins<-data.frame(z=zseq,dydx=dy.dx,se=se.dy.dx)
  return(margins)
}

prejnkidsmargins<-margins.twoway(model=m10,xterm="nkids",zterm="prej",zseq=seq(from=0,to=1,by=.05))
eidnkidsmargins<-margins.twoway(model=m11,xterm="nkids",zterm="eid",zseq=seq(from=0,to=1,by=.05))
nkidsmargins<-rbind(prejnkidsmargins,eidnkidsmargins)
nkidsmargins$zterm<-c(rep("Ethnic prejudice",nrow(prejnkidsmargins)),rep("Economic conservatism",nrow(eidnkidsmargins)))

# Appendix figure 1
ggplot(gd,aes(x=oppose,y=cutall)) +
  geom_point(position=position_jitter(w=.07,h=.07),alpha=.2,size=2) +
  geom_smooth(method="lm",color="black",alpha=.9) +
  theme_bw() +
  geom_abline(intercept=0, slope=1,linetype="dashed") +
  xlab("Oppose cross-border welfare rights") +
  ylab("Support cuts for all citizens") +
  scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) +
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1"))

ggsave(file="figures/cutscatter.pdf",width=10,height=4)
ggsave(file="figures/cutscatter.png",width=8,height=4)
ggsave(file="figures/cutscatter_small.png",width=140,height=80,units="mm")


# Appendix figure 2

ggplot(nkidsmargins,aes(x=z,y=dydx)) +
  geom_hline(yintercept=0,linetype=2,color="dark gray") +
  geom_smooth(aes(ymin=dydx-t90*se,ymax=dydx+t90*se),stat="identity",alpha=.3,color="black") +
  geom_smooth(aes(ymin=dydx-t95*se,ymax=dydx+t95*se),stat="identity",alpha=.2,linetype=0) +
  #  geom_rug(data=gd,aes(x=prej,y=0),position="jitter",sides="b",alpha=.1) +
  #  ylim(c(-0.03,.06)) +
  xlab("") +
  ylab("Marginal effect of number of children cue") +
  facet_grid(.~zterm) +
  scale_x_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c("0",".25",".5",".75","1")) +
  theme_bw() 
ggsave(filename="figures/nkidsmargins.pdf",width=7,height=4)

#Appendix figure 3

bulgeffs<-Effect("bulg01",m09,se=T,xlevels=2)$fit-Effect("bulg01",m09,se=T,xlevels=2)$fit[1]
kideffs<-Effect("nkids",m09,se=T)$fit-Effect("nkids",m09,se=T)$fit[1]

expcoefs<-data.frame(cond=c("Dutch","Bulgarian",1:5),
                     condorder=c(1,2,1:5),
                     est=c(bulgeffs,kideffs),
                     se=c(Effect("bulg01",m09,se=T,xlevels=2)$se[1:2],Effect("nkids",m09,se=T)$se[1:5]),
                     exp=c(rep("Nationality",2),rep("Number of children",5)))

prejfullfac<-rbind(as.data.frame(Effect(c("bulg01","nkidsfac"),mfullfac1,se=T,xlevels=list(bulg01=0:1,nkidsfac=1:5))),
                   as.data.frame(Effect(c("bulg01","nkidsfac"),mfullfac2,se=T,xlevels=list(bulg01=0:1,nkidsfac=1:5))))
prejfullfac$model<-c(rep("Children stay",10),rep("Not mentioned",10))
prejfullfac$bulg01<-factor(prejfullfac$bulg01,labels=c("Dutch","Bulgarian"))

ggplot(prejfullfac,aes(x=nkidsfac,y=fit,group=bulg01,color=bulg01)) +
  geom_point(position=position_dodge(width=0.6)) +
  geom_line(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0,position=position_dodge(width=0.6)) +
  facet_grid(.~model) +
  theme_bw() +
  xlab("Number of children") +
  ylab("Expressed welfare chauvinism") +
  theme(legend.title=element_blank()) +
  scale_color_manual(values=c("#bbbbbb","#333333"))

ggsave(file="figures/whobenefits_fullfac.pdf",width=8,height=4)

### ESS PLOT
ess6<-read.dta(file="ESS6e02.dta") # due to file size, the ESS6 is not included in the replication materials, but can be obtained at http://www.europeansocialsurvey.org/data/download.html?r=6

ess6$cultenrich<-ifelse(as.numeric(ess6$imueclt)<12,(as.numeric(ess6$imueclt)-1)/10,NA)
ess6$goodecon<-ifelse(as.numeric(ess6$imbgeco)<12,(as.numeric(ess6$imbgeco)-1)/10,NA)
ess6$betplace<-ifelse(as.numeric(ess6$imwbcnt)<12,(as.numeric(ess6$imwbcnt)-1)/10,NA)


immsumc<-ess6 %.%
  group_by(cntry) %.%
  summarise(avg=mean(cultenrich,na.rm=T),se=sd(cultenrich,na.rm=T)/sqrt(length(cultenrich)),var="cultenrich")
immsumg<-ess6 %.%
  group_by(cntry) %.%
  summarise(avg=mean(goodecon,na.rm=T),se=sd(goodecon,na.rm=T)/sqrt(length(goodecon)),var="goodecon")
immsumb<-ess6 %.%
  group_by(cntry) %.%
  summarise(avg=mean(betplace,na.rm=T),se=sd(betplace,na.rm=T)/sqrt(length(betplace)),var="betplace")

rm(ess6)

immsum<-as.data.frame(rbind(immsumc,immsumg,immsumb))

immsum$lwr95<-immsum$avg-t95*immsum$se
immsum$lwr90<-immsum$avg-t90*immsum$se
immsum$upr90<-immsum$avg+t90*immsum$se
immsum$upr95<-immsum$avg+t95*immsum$se
immsum$cntry<-reorder(immsum$cntry,immsum$avg)
immsum$var<-factor(immsum$var,labels=c("Makes country better place","Enriches culture","Good for economy"))
factor(immsum$var)

ggplot(subset(immsum,!(cntry %in% c("AL","IL","NO","RU","XK","UA","CH","IS"))),aes(x=avg,y=cntry,colour=factor(cntry=="SE"))) +
  geom_point() +
  geom_errorbarh(aes(xmin=lwr95,xmax=upr95),size=.5,width=.01,height=0) +
  #  geom_errorbarh(aes(xmin=lwr90,xmax=upr90),size=.6,height=.01) +
  theme_bw() +
  theme(legend.position="none") +
  facet_grid(.~var) +
  xlab("Avg. agreement with statement about immigration") +
  #  scale_colour_brewer(palette="Paired") +
  scale_colour_manual(values=c("gray","black")) +
  ylab("") +
  theme(axis.text.y = element_text(colour="black"))

ggsave("figures/essplot.pdf",height=4,width=9)
