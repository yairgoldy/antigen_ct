source('libraries.R')



#-------------------------------------------------------------------------------
#             Table 1: proportions 
#-------------------------------------------------------------------------------

#import
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




ct$n=1

#calculate proportion by month
#Table 1 columns:
#N: number of Ag tests taken at certain month and percentage among all tests
#AgPositive: percent of positive Ag tests
M1=aggregate(list(ct$n,ct$AgResult),by=list(ct$month),FUN=sum)
colnames(M1)<-c('category','N','AgPositive')
M1$variable='Month'
M1$AgPositive=paste0(round(100*M1$AgPositive/M1$N,1),'%')
percent_= round(M1$N/sum(M1$N)*100,1)
M1$N=paste0(M1$N,' (',percent_,'%)')
  
#by age
M2=aggregate(list(ct$n,ct$AgResult),by=list(ct$age_category),FUN=sum)
colnames(M2)<-c('category','N','AgPositive')
M2$variable='age'
M2$AgPositive=paste0(round(100*M2$AgPositive/M2$N,1),'%')
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')

  
  
M1=rbind(M1,M2)


#by sex
M2=aggregate(list(ct$n,ct$AgResult),by=list(ct$vsex),FUN=sum)
colnames(M2)<-c('category','N','AgPositive')
M2$variable='sex'
M2$AgPositive=paste0(round(100*M2$AgPositive/M2$N,1),'%')
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  
 
#by vaccination status
M1=rbind(M1,M2)
M2=aggregate(list(ct$n,ct$AgResult),by=list(ct$cohort),FUN=sum)
colnames(M2)<-c('category','N','AgPositive')
M2$variable='vaccination'
M2$AgPositive=paste0(round(100*M2$AgPositive/M2$N,1),'%')
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  

#by lab
M1=rbind(M1,M2)
M2=aggregate(list(ct$n,ct$AgResult),by=list(ct$labname),FUN=sum)  
colnames(M2)<-c('category','N','AgPositive')
M2$variable='lab'
M2$AgPositive=paste0(round(100*M2$AgPositive/M2$N,1),'%')
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  
M1=rbind(M1,M2)
MAg=M1[,c("category",'N',"AgPositive")]
  


#repeat previous calculation on the general population
load('Data\\nonAgdemo.R')

#rename months
nonAg$month=as.character(nonAg$month)
nonAg$month[which(nonAg$month=='1')]='JAN'
nonAg$month[which(nonAg$month=='2')]='FEB'
nonAg$month[which(nonAg$month=='3')]='MAR'
nonAg$month[which(nonAg$month=='4')]='APR'




#remove rows with missing covariates and factorize
nonAg=nonAg[which(nonAg$age_category!=''),]
nonAg=nonAg[which(nonAg$vsex=='M' | nonAg$vsex=='F'),]
nonAg=nonAg[which(nonAg$month=='JAN' | nonAg$month=='FEB' | nonAg$month=='MAR' | nonAg$month=='APR'),]
nonAg$month=factor(nonAg$month,levels = c('JAN','FEB','MAR','APR'))
nonAg$vsex=factor(nonAg$vsex,levels=c('F','M'))
nonAg$age_category=factor(nonAg$age_category, levels = c('0-2','3-4','5-11','12-15','16-39','40-59','60+'))
nonAg$cohort=factor(nonAg$cohort,levels = c('Unvaccinated','1-dose','2-dose','3-dose','4-dose','Recovered','Recovered+vacc'))



nonAg$n=1




M1=aggregate(list(nonAg$n),by=list(nonAg$month),FUN=sum)
colnames(M1)<-c('category','N')
M1$variable='Month'
percent_= round(M1$N/sum(M1$N)*100,1)
M1$N=paste0(M1$N,' (',percent_,'%)')
  


M2=aggregate(list(nonAg$n),by=list(nonAg$age_category),FUN=sum)
colnames(M2)<-c('category','N')
M2$variable='age'
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  

  
M1=rbind(M1,M2)
M2=aggregate(list(nonAg$n),by=list(nonAg$vsex),FUN=sum)
colnames(M2)<-c('category','N')
M2$variable='sex'
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  
  
M1=rbind(M1,M2)
M2=aggregate(list(nonAg$n),by=list(nonAg$cohort),FUN=sum)
colnames(M2)<-c('category','N')
M2$variable='vaccination'
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  


M1=rbind(M1,M2)
M2=aggregate(list(nonAg$n),by=list(nonAg$labname),FUN=sum)
colnames(M2)<-c('category','N')
M2$variable='lab'
percent_= round(M2$N/sum(M2$N)*100,1)
M2$N=paste0(M2$N,' (',percent_,'%)')
  

M=rbind(M1,M2)
M=M[,c("category",'N')]



M=cbind(M,MAg[,c('N','AgPositive')])


write.csv(M,'table1.csv')







#-------------------------------------------------------------------------------
#             Table 2: regression 
#-------------------------------------------------------------------------------


#factorize
ct$epiweek=factor(ct$epiweek)
ct$age_category=factor(ct$age_category,levels = c('16-39','0-2','3-4','5-11','12-15','40-59','60+'   ))



#logistic regression
analysisN=glm(formula = "AgResult ~ ct + cohort+ vsex + age_category+labname+ epiweek",family = binomial(link = "logit"),data = ct)


#calculate CI's and put them in a table
ci=confint(analysisN)
ci=as.data.frame(ci)
ci=round(ci,2)
analysisN=as.data.frame(summary(analysisN)$coefficients)
analysisN$Estimate=round(analysisN$Estimate,2)
for(i in 1:dim(analysisN)[1]){
  if(analysisN$`Pr(>|z|)`[i]<.05){analysisN$Estimate[i]=paste0(analysisN$Estimate[i],'*')}
}
analysisN$Estimate=paste0(analysisN$Estimate,' (',ci$`2.5 %`,',',ci$`97.5 %`,')')
analysisN$variables=rownames(analysisN)
M=analysisN[,c('variables',"Estimate")]
rownames(M)<-NULL
colnames(M)<-c('variables','N')


MM=c()
MM=rbind(MM,M[which(M$variables=='(Intercept)'),])
MM=rbind(MM,M[which(M$variables=='ct'),])
MM=rbind(MM,M[which(M$variables=='age_category0-2'),])
MM=rbind(MM,M[which(M$variables=='age_category3-4'),])
MM=rbind(MM,M[which(M$variables=='age_category5-11'),])
MM=rbind(MM,M[which(M$variables=='age_category12-15'),])
MM=rbind(MM,c('age_category16-39','ref.','ref.','ref.','ref.'))
MM=rbind(MM,M[which(M$variables=='age_category40-59'),])
MM=rbind(MM,M[which(M$variables=='age_category60+'),])
MM=rbind(MM,c('F','ref.','ref.','ref.','ref.'))
MM=rbind(MM,M[which(M$variables=='vsexM'),])
MM=rbind(MM,c('labnameLab 1','ref.','ref.','ref.','ref.'))
MM=rbind(MM,M[which(M$variables=='labnameLab 2'),])
MM=rbind(MM,M[which(M$variables=='labnameLab 3'),])
MM=rbind(MM,M[which(M$variables=='labnameLab 4'),])
MM=rbind(MM,M[which(M$variables=='labnameLab 5'),])
MM=rbind(MM,M[which(M$variables=='labnameLab 6'),])
MM=rbind(MM,c('unvaccinated','ref.','ref.','ref.','ref.'))
MM=rbind(MM,M[which(M$variables=='cohort1-dose'),])
MM=rbind(MM,M[which(M$variables=='cohort2-dose'),])
MM=rbind(MM,M[which(M$variables=='cohort3-dose'),])
MM=rbind(MM,M[which(M$variables=='cohort4-dose'),])
MM=rbind(MM,M[which(M$variables=='cohortRecovered'),])
MM=rbind(MM,M[which(M$variables=='cohortRecovered+vacc'),])
MM=rbind(MM,c('epiweek0','ref.','ref.','ref.','ref.'))
MM=rbind(MM,M[which(M$variables=='epiweek1'),])
MM=rbind(MM,M[which(M$variables=='epiweek2'),])
MM=rbind(MM,M[which(M$variables=='epiweek3'),])
MM=rbind(MM,M[which(M$variables=='epiweek4'),])
MM=rbind(MM,M[which(M$variables=='epiweek5'),])
MM=rbind(MM,M[which(M$variables=='epiweek6'),])
MM=rbind(MM,M[which(M$variables=='epiweek7'),])
MM=rbind(MM,M[which(M$variables=='epiweek8'),])

write.csv(MM,'table2.csv')







###apply logistic results to the general population
analysisN=glm(formula = "AgResult ~ ct + cohort+ vsex + age_category+labname+ epiweek",family = binomial(link = "logit"),data = ct)
nonAg$epiweek=factor(nonAg$epiweek)
nonAg=nonAg[,c('ct','cohort','vsex','age_category','labname','epiweek')]
pred=as.numeric(predict(analysisN,nonAg))
pred=1/(1+exp(-pred))
print(paste('Ag detection probability:',round(sum(pred)/length(pred),2)))







analysisN=glm(formula = "AgResult ~ ct + cohort+ vsex + age_category+labname+ epiweek",family = binomial(link = "logit"),data = ct)
nonAg$epiweek=factor(nonAg$epiweek)
nonAg=nonAg[,c('ct','cohort','vsex','age_category','labname','epiweek')]
pred=as.numeric(predict(analysisN,nonAg[which(nonAg$epiweek==0),]))
pred=1/(1+exp(-pred))
print(paste('Ag detection probability on epiweek 0:',round(sum(pred)/length(pred),2)))



analysisN=glm(formula = "AgResult ~ ct + cohort+ vsex + age_category+labname+ epiweek",family = binomial(link = "logit"),data = ct)
nonAg$epiweek=factor(nonAg$epiweek)
nonAg=nonAg[,c('ct','cohort','vsex','age_category','labname','epiweek')]
pred=as.numeric(predict(analysisN,nonAg[which(nonAg$epiweek==7),]))
pred=1/(1+exp(-pred))
print(paste('Ag detection probability on epiweek 7:',round(sum(pred)/length(pred),2)))



