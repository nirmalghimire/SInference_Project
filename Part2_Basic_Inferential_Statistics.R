#Part2_Basic Inferential Statistics
#A. Invoking Required Libraries
library(ggplot2)
library(dplyr)

#Invoking required datafile
data(ToothGrowth)

#1.	Basic Exploratory Data Analyses
#a. Quick Checking of Given Dataset
head(ToothGrowth)
#b. Checking the structure of the dataset
str(ToothGrowth)
#c. Summary of the Dataset
summary(ToothGrowth)

#2. Changing does as a factor 
ToothGrowth$dose<-as.factor(ToothGrowth$dose)
#a. Checking if that worked
head(ToothGrowth$dose)
#b. verifying the mean of the len variable by supply method
SupMean=split(ToothGrowth$len,ToothGrowth$supp)
sapply(SupMean,mean)
#c. Creating box plot of OJ and VC 
e<-ggplot(aes(x=supp,y=len),data=ToothGrowth)
#Experimenting the violin plot
e+ggtitle("Supplement Type and the Tooth Length")+
  geom_violin(trim=FALSE)+stat_summary(fun.data = "mean_sd1",fun.args=list(mult=1),
                                       geom="pointrange",color="black")
#Putting boxplot within the violin plot
e+geom_violin(aes(fill=supp),trim=FALSE)+
  geom_boxplot(width=0.4)+scale_fill_manual(values=c("#E7B800", "#FC4E07"))+ geom_jitter(width = 0.1)+xlab("Supplement Type")+ 
  ylab("Tooth Length")

#Checking the impact of Vitamin C on tooth length
Mean_dose=split(ToothGrowth$len,ToothGrowth$dose)
sapply(Mean_dose,mean)

#Plotting the findings
e+ggtitle("Box-Plot Showing Dose of Supplement Type and Tooth Length")+
  geom_boxplot(aes(fill=dose))+scale_fill_manual(values=c("#E7B800", 
              "#FC4E07","#00AFBB"))+xlab("Dose")+ylab("Tooth Length")

#Tooth Length and Delivery Method
ToothGrowth %>%
  group_by(supp,dose) %>%
  summarise(Q25th_len=quantile(len,0.25),
            Q50th_len=quantile(len,0.5),
            Q75th_len=quantile(len,0.75),
            average_lenth=mean(len),
            SD_len=sd(len))->newtable
newtable

#Calculating ttest to study the relationship between tooth length, supplement type, and dose
test=list()
dose=c(0.5,1,2)
for(m in dose){
  Moj=ToothGrowth$len[ToothGrowth$dose==m & ToothGrowth$supp=="OJ"]
  Mvc=ToothGrowth$len[ToothGrowth$dose==m & ToothGrowth$supp=="VC"]
  t<-t.test(Moj,Mvc)
  id<-paste0("OJ","-","VC",",",m)
  test<-rbind(test,list(id=id,p.value=t$p.value,CI.LOW=t$conf.int[1],
                          CI.HIGH=t$conf.int[2]))
}
test
