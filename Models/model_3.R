###Model-3: Discussion length model
Table <- read.csv('issues_model_data.csv')

#Remove top 2% outliers
top_outlier<-quantile(M$totalLatency,0.98)
Table_final<-subset(M,M$totalLatency<top_outlier)

#Get the linkLatency
Table_final$linkLatency<-Table_final$totalLatency-Table_final$finalLatency

#We used effect coding to set the contrasts of this three-way factor, 
#i.e., comparing each level to the grand mean of all three levels.
#In practice, we change the order of the three levels and run 
#the model twice to get the coefficients of all three types of links.
Table_final$linkPlace<-factor(Table_final$linkPlace,levels = c("within-project","within-ecosystem","cross-ecosystem"))
#Table_final$linkPlace<-factor(Table_final$linkPlace,levels = c("within-project","cross-ecosystem","within-ecosystem"))


#Run modeling
model_3 <- lm(scale(log(nComments+0.5))~
        scale(log(linkLatency+0.5)) 
        +scale(log(teamSize+0.5))
        +scale(log(nIssues+0.5))
        +factor(uContributor)
        +scale(log(textLen))
        +factor(hasAssignee)
        +factor(hasLabel)
        +factor(hasMilestone)
        +factor(linkClosed)
        +factor(linkPR)
        +scale(log(linkTextLen))
        +scale(textSim)
        +scale(nActors)
        +scale(ratioM)
        +scale(ratioCL)
        +scale(ratioCM)
        +scale(ratioCML)
        +factor(isDifficult)
        +linkPlace,data=Table_final,contrasts = list(linkPlace = "contr.sum"))

#Results
summary(model_3)
anova(model_3)