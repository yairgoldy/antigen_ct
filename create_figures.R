source('libraries.R')




# 'create_fig_table' calculates the proportion estimates presented in the figures
create_fig_table<-function(ct){

  ct$all=1
  
  M1=aggregate(ct$AgResult ,by=list(ct$ct1),FUN=sum) #number of positive AG grouped by ct
  colnames(M1)<-c('ct','p')
  
  M2=aggregate(ct$all,by=list(ct$ct1),FUN=sum)      #number of AG tests grouped by ct
  colnames(M2)<-c('ct','n')
  
  M=merge(M1,M2,by='ct',all.x = TRUE, all.y = TRUE)
  M$lower=NA
  M$upper=NA
  M$prob=M$p/M$n  #proportion estimate: #(positive AG)/#(number of AG tests)
  

  #Confidence interval
  for(i in 1:dim(M)[1]){
    temp=BinomCI(M$p[i], M$n[i],conf.level = 0.95,method = "clopper-pearson")
    M$lower[i]=temp[2]
    M$upper[i]=temp[3]
  }
  
  M$ct1=M$ct
  
  model <- glm(AgResult ~ ct,family=binomial(link='logit'),data=ct)
  c1=as.numeric(model$coefficients[1])
  c2=as.numeric(model$coefficients[2])
  M$prediction=1-1/(1+exp(c1+c2*M$ct1))  
  return(M)
}






txtsize=13.5
load('Data\\ctdemo.R')
load('Data\\nonAgdemo.R')

 


#include only individuals whose AG test was taken before PCR result was obtained
indx5=which(ct$AgTakeMinusPcrTake<ct$PCRresultMinusPcrTake)
ct=ct[indx5,]

 





#-------------------------------------------------------------------------------
#             fig S1: compare populations
#-------------------------------------------------------------------------------


ct1=ct
ct1$population='AG population'
ct1=ct1[,c('ct','population')]

nonAg$population='General population'
nonAg=nonAg[,c('ct','population')]

ct1=rbind(ct1,nonAg)   #ct table with a column indicating either AG population or General population


#create plot
cbPalette=c('gray0','gray30')
pS1=ggplot(ct1,aes(x=ct,color=population))+geom_density(bw=1,size=1.5)+theme_bw()+
  theme(legend.position="top")+xlab('Ct-level')+ylab('p') + theme(legend.title=element_blank())+
  theme(text = element_text(size=txtsize))+ theme(axis.text=element_text(size=txtsize))
pS1=pS1 + theme(legend.position = c(0.1, 0.9))+theme(legend.key.size = unit(1, 'cm'))
pS1=pS1 + scale_color_grey() 
pS1=pS1 + scale_colour_manual(values = c("red", "blue"))
pS1=pS1+theme(text = element_text(size=15))
plot(pS1)






#-------------------------------------------------------------------------------
#             fig S2: proportion estimate plots by covariates
#-------------------------------------------------------------------------------

 
pos1=0.3
pos2=0.4
interval_=2
ct$ct1=ct$ct
ct$ct1[which(ct$ct1<=15)]=15
linesize=1


#proportion estimate by month
ct$month=factor(ct$month,levels = c('JAN','FEB','MAR','APR'))



#calculate proportion estimate for each month
months_=c('JAN','FEB','MAR','APR')
for(i in 1:length(months_)){
  M=create_fig_table(ct[which(ct$month==months_[i]),])
  M$month=months_[i]
  if(i==1){Mfull=M}else{Mfull=rbind(Mfull,M)}
}
Mfull$month=factor(Mfull$month,levels = c('JAN','FEB','MAR','APR'))

#plot
pS2_1<- ggplot(Mfull, aes(x=ct1, y=prob,color=month)) + geom_errorbar(data=Mfull[which(Mfull$ct1%%2==0),],   aes(x=ct1,ymin=lower, ymax=upper),size=linesize, width=.5,position=position_dodge(.9)) +geom_line(aes(x=ct1,y=prediction),size=1,linetype = "dashed")+
  theme(legend.position="top")+xlab('')+ylab('AG detection probability')+theme(legend.title=element_blank())+theme_bw()
pS2_1=pS2_1 + theme(legend.position = c(pos1, pos2))+theme(legend.key.size = unit(.6, 'cm'))+theme(legend.title = element_blank())+ylim(0,1)
pS2_1+xlab('Ct-level')+theme(legend.key.size = unit(1, 'cm'))

 




#proportion estimate by vaccination status
cohorts=c('Unvaccinated','1-dose','2-dose','3-dose','4-dose','Recovered','Recovered+vacc')

#calculate proportion estimate for each vaccination status
for(i in 1:length(cohorts)){
  M=create_fig_table(ct[which(ct$cohort==cohorts[i]),])
  M$cohorts=cohorts[i]
  if(i==1){Mfull=M}else{Mfull=rbind(Mfull,M)}
}
Mfull$cohorts=factor(Mfull$cohorts,levels=cohorts)

#plot
pS2_2<- ggplot(Mfull, aes(x=ct1, y=prob,color=cohorts)) + geom_errorbar(data=Mfull[which(Mfull$ct1%%2==0),],   aes(x=ct1,ymin=lower, ymax=upper),size=linesize, width=.5,position=position_dodge(.9)) +geom_line(aes(x=ct1,y=prediction),size=1,linetype = "dashed")+
  theme(legend.position="top")+xlab('')+ylab('')+theme(legend.title=element_blank())+theme_bw()
pS2_2=pS2_2 + theme(legend.position = c(pos1, pos2))+theme(legend.key.size = unit(.6, 'cm'))+theme(legend.title = element_blank())+ylim(0,1)
pS2_2+xlab('Ct-level')+theme(legend.key.size = unit(1, 'cm'))





#proportion estimate by age category 
index=which(ct$age_category=='')
if(length(index)>0){ct=ct[-which(ct$age_category==''),]}
ages=c('0-2','3-4','5-11','12-15','16-39','40-59','60+')

#calculate proportion estimate for each age category
for(i in 1:length(ages)){
  M=create_fig_table(ct[which(ct$age_category==ages[i]),])
  M$ages=ages[i]
  if(i==1){Mfull=M}else{Mfull=rbind(Mfull,M)}
}
Mfull$ages=factor(Mfull$ages,levels=ages)

#plot
pS2_3<- ggplot(Mfull, aes(x=ct1, y=prob,color=ages)) + geom_errorbar(data=Mfull[which(Mfull$ct1%%2==0),],   aes(x=ct1, ymin=lower, ymax=upper),size=linesize, width=.5,position=position_dodge(.9)) +geom_line(aes(x=ct1,y=prediction),size=1,linetype = "dashed")+
  theme(legend.position="top")+xlab('Ct-level')+ylab('AG detection probability')+theme(legend.title=element_blank())+theme_bw()
pS2_3=pS2_3 + theme(legend.position = c(pos1, pos2))+theme(legend.key.size = unit(.6, 'cm'))+theme(legend.title = element_blank())+ylim(0,1)
pS2_3+xlab('Ct-level')+theme(legend.key.size = unit(1, 'cm'))



#proportion estimate by sex
ct=ct[which(ct$vsex=='F' |ct$vsex=='M' ),]
vsex=c('F','M')

#calculate proportion estimate for each sex
for(i in 1:length(vsex)){
  M=create_fig_table(ct[which(ct$vsex==vsex[i]),])
  M$vsex=vsex[i]
  if(i==1){Mfull=M}else{Mfull=rbind(Mfull,M)}
}
Mfull$vsex=factor(Mfull$vsex,levels=vsex)

#plot
pS2_4<- ggplot(Mfull, aes(x=ct1, y=prob,color=vsex)) + geom_errorbar(data=Mfull[which(Mfull$ct1%%2==0),],   aes(x=ct1,ymin=lower, ymax=upper),size=linesize, width=.5,position=position_dodge(.9)) +geom_line(aes(x=ct1,y=prediction),size=1,linetype = "dashed")+
  theme(legend.position="top")+xlab('Ct-level')+ylab('')+theme(legend.title=element_blank())+theme_bw()
pS2_4=pS2_4 + theme(legend.position = c(pos1, pos2))+theme(legend.key.size = unit(.6, 'cm'))+theme(legend.title = element_blank())+ylim(0,1)
pS2_4+xlab('Ct-level')+theme(legend.key.size = unit(1, 'cm'))


ggarrange(pS2_1,pS2_2,pS2_3,pS2_4)






#-------------------------------------------------------------------------------
#             fig S3: proportion estimate by lab
#-------------------------------------------------------------------------------

#proportion estimate by lab
index=which(ct$labname=='' | is.na(ct$labname)==T)
if(length(index)>0){ct=ct[-index,]}
labs=c('Lab 1', 'Lab 2', 'Lab 3', 'Lab 4', 'Lab 5')

#calculate proportion estimate for each age category
for(i in 1:length(labs)){
  M=create_fig_table(ct[which(ct$labname==labs[i]),])
  M$labs=labs[i]
  if(i==1){Mfull=M}else{Mfull=rbind(Mfull,M)}
}
Mfull$ages=factor(Mfull$labs,levels=labs)

#plot
pS3<- ggplot(Mfull, aes(x=ct1, y=prob,color=ages)) + geom_errorbar(data=Mfull[which(Mfull$ct1%%2==0),],   aes(x=ct1, ymin=lower, ymax=upper),size=linesize, width=.5,position=position_dodge(.9)) +geom_line(aes(x=ct1,y=prediction),size=1,linetype = "dashed")+
  theme(legend.position="top")+xlab('Ct-level')+ylab('AG detection probability')+theme(legend.title=element_blank())+theme_bw()
pS3=pS3 + theme(legend.position = c(pos1, pos2))+theme(legend.key.size = unit(.6, 'cm'))+theme(legend.title = element_blank())+ylim(0,1)
pS3+xlab('Ct-level')+theme(legend.key.size = unit(1, 'cm'))








#-------------------------------------------------------------------------------
#             fig 2: main figure
#-------------------------------------------------------------------------------



#calculate error bars and logistic line
load('Data\\ctdemo.R')


#include only individuals whose AG test was taken before PCR result was obtained
indx5=which(ct$AgTakeMinusPcrTake<ct$PCRresultMinusPcrTake)
ct=ct[indx5,]
 


ct$ct1=ct$ct
ct$ct1[which(ct$ct1<=15)]=15  #ct<15 change to 15
M=create_fig_table(ct)  #calculate proportion estimates




#calculations for spaghetti plot
load('Data\\ctdemo.R')

#include only individuals whose AG test was taken before PCR result was obtained
indx1=which(ct$AgTakeMinusPcrTake<ct$PCRresultMinusPcrTake)
ct=ct[indx1,]



#remove rows with missing covariates and factorize
ct=ct[which(ct$age_category!=''),]
ct=ct[which(ct$vsex=='M' | ct$vsex=='F'),]
ct=ct[which(ct$month=='JAN' | ct$month=='FEB' | ct$month=='MAR' | ct$month=='APR'),]
ct$month=factor(ct$month,levels = c('JAN','FEB','MAR','APR'))
ct$vsex=factor(ct$vsex,levels=c('F','M'))
ct$age_category=factor(ct$age_category, levels = c('0-2','3-4','5-11','12-15','16-39','40-59','60+'))
ct$cohort=factor(ct$cohort,levels = c('Unvaccinated','1-dose','2-dose','3-dose','4-dose','Recovered','Recovered+vacc'))
ct$epiweek=factor(ct$epiweek)
ct$age_category=factor(ct$age_category,levels = c('16-39','0-2','3-4','5-11','12-15','40-59','60+'   ))

#logistic regression
analysisN=glm(formula = "AgResult ~ ct + cohort+ vsex + age_category+labname+ epiweek",family = binomial(link = "logit"),data = ct)





#prediction on each individual for all possible ct's (other covariates stay fixed)
ctvec=seq(from=15, to=35, by=1)
ctm=as.data.frame(matrix(0,nrow = dim(ct)[1],ncol=0))

#loop over ct's
for(i in 1:length(ctvec)){
  print(i)
  cttemp=ct
  cttemp$ct=ctvec[i]
  uu=predict(analysisN, cttemp, type="response")
  uu=as.numeric(uu)
  ctm=cbind(ctm,uu)  
}



cti=as.data.frame(matrix(0,nrow = 0,ncol=2))
for (j in 1:dim(ctm)[2]){
  print(j)
  temp=cbind( replicate(dim(ctm)[1], j+14) ,ctm[,j],seq(from=1,to=dim(ctm)[1],by=1))
  cti=rbind(cti,temp)
}
cti=cti[!duplicated(cti[,1:2]),]



#plot
p2<- ggplot(M, aes(x=ct1, y=prob)) + 
  geom_line(data=cti,aes(x=V1,y=V2,group=factor(V3)),size=1,color='grey86' ,alpha = 0.4)+
  geom_errorbar(data=M,aes(ymin=lower, ymax=upper),size=1.5, width=.5,position=position_dodge(.9)) +
  geom_line(data=M,aes(x=ct1,y=prediction),size=1,linetype = "dashed")+
  theme(legend.position="none")+xlab('Ct-level')+ylab('AG detection probability')+
  theme(text = element_text(size=txtsize))+theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=txtsize))
plot(p2+theme(text = element_text(size=15)))












