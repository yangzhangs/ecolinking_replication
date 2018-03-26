###Model-2: Proportion of links that across projects vs.
###overall issue resolution latency.
Table <- read.csv('projects_model_data.csv')


#Run modeling
model_2 <- lm(scale(log(avgIssueLatency+0.5))~
        scale(log(nLinksOfProject+0.5))
        +propOfWithinEcoLinks
        +propOfCrossEcoLinks,data=Table_final)

#Results
summary(model_2)
anova(model_2)