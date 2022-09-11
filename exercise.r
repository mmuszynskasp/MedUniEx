#####################################################################################################################################
########Aim: exercise for Med Uni
####################################################################################################################################
### IMPORTANT! corrected weight for case 10

library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(corrplot)
library(ggcorrplot)
library(psych)
library(RColorBrewer)
library(freqtables)
library(stargazer)

dev.off()
rm(list=ls())

datadir <- "C:\\Users\\Magdalena\\meduni\\exercise\\data"
setwd(datadir)


###prepare data in one file
masterf <- read.csv("master.csv", header=TRUE) %>%
  separate(duration.of.ventilation, sep = ":",
                  into = c("vent.hrs", "vent.min")) %>%
  separate(duration.of.intubation, sep=":", into=c("int.hrs","int.min"))%>%
  mutate(vent.dur=60*as.numeric(vent.hrs)+as.numeric(vent.min),
         int.dur=60*as.numeric(int.hrs)+as.numeric(int.min),
         user.experience=ifelse(user.experience==1,1,2))
    
studyid <- masterf$Study.ID

i=1
ourcase <- read.csv(paste("TARGET_0",studyid[i],".csv",sep=""),header = TRUE) %>%
  mutate(Study.ID=studyid[i]) %>%
  left_join(masterf, by="Study.ID")
write.table(ourcase, file="allcases.csv",sep=",",row.names = FALSE)

for (i in 2:length(studyid)){
  ourcase <- read.csv(paste("TARGET_0",studyid[i],".csv",sep=""),header = TRUE) %>%
    mutate(Study.ID=studyid[i]) %>%
    left_join(masterf, by="Study.ID")
  write.table(ourcase, file="allcases.csv",sep=",",row.names = FALSE, col.names=FALSE, append=TRUE)
}

#####read-in one file data
ourdata <- read.csv(file="allcases.csv",header=TRUE) %>%
  mutate(Tidal.volume..mL.kg.= as.numeric(Tidal.volume..mL.kg.)) %>%
  mutate(Vte=ifelse(Tidal.volume..mL.kg.>=4 & Tidal.volume..mL.kg.<=8,1,0),
         mask.leak=ifelse(Leakage....<35,1,0),
         PIP =ifelse(peak.inspiratory.pressure..cmH2O.<30,1,0),
         resp.rate=ifelse(Rate...min.>=30 & Rate...min.<=60,1,0),
         PEEP=ifelse(Post.end.expiratory.pressure..cmH2O.>=2 & Post.end.expiratory.pressure..cmH2O.<=5,1,0),
         all.par=Vte+mask.leak+PIP+resp.rate+PEEP,
         all.par=ifelse(all.par==5,1,0),
         user.experience=ifelse(user.experience==1,1,2))

########### test for normality of distributions
Ntest <- ourdata %>%
  select(Weight.at.intervention, Tidal.volume..mL.kg.,Leakage....,peak.inspiratory.pressure..cmH2O.,Rate...min.,Post.end.expiratory.pressure..cmH2O.) %>%
  summarise_all(.funs = funs(p.value = shapiro.test(.)$p.value))

############ test for normality of distributions without the newborn - the same result as above
Ntestno <- ourdata %>%
#  filter(intervention==1) %>%
  select(Tidal.volume..mL.kg.,Leakage....,peak.inspiratory.pressure..cmH2O.,Rate...min.,Post.end.expiratory.pressure..cmH2O.) %>%
  summarise_all(.funs = funs(p.value = shapiro.test(.)$p.value))


########### table 1 for intervention/ maneuvers and tools
tab1a <- masterf %>%
  group_by(User.type, Ventilation.device) %>%
  summarize(nv=n()) %>%
  left_join(masterf %>%
              group_by(User.type) %>% summarize(nt=n())) %>%
  mutate(percv=nv/nt)


tab1b <- masterf %>%
  filter(!is.na(mask.size)) %>%
  group_by(user.experience, mask.size) %>%
  summarize(nv=n()) %>%
  left_join(masterf %>%
              group_by(user.experience) %>% summarize(nt=n())) %>%
  mutate(percv=nv/nt)

tab1c <- masterf %>%
  filter(!is.na(PIP.adjustment)) %>%
  group_by(user.experience, PIP.adjustment) %>%
  summarize(nv=n()) %>%
  left_join(masterf %>%
              group_by(user.experience) %>% summarize(nt=n())) %>%
  mutate(percv=nv/nt)

####################################################################################################
### weight according to provider

t.test(masterf$Weight.at.intervention,masterf$User.type, alternative = "greater")
t.test(masterf$Weight.at.intervention,masterf$user.experience, alternative = "greater")


dev.off()
pdf(file = "C:/Users/Magdalena/meduni/exercise/figures/userweight.pdf",  width = 10, height = 5, onefile=FALSE) 

ggarrange(ggplot(data=masterf, aes(x=as.factor(User.type), y=Weight.at.intervention,group=User.type)) + 
  geom_violin()+ stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",colour = "red")+
  scale_y_continuous(name="Weight") + scale_x_discrete(name="User", labels=c("Assistenzarzt","Oberarzt"))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
  ggtitle("User Type") + theme(plot.title = element_text(face="bold")),
ggplot(data=masterf, aes(x=as.factor(user.experience), y=Weight.at.intervention,group=user.experience)) + 
  geom_violin()+ stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",colour = "red")+
  scale_y_continuous(name="Weight") + scale_x_discrete(name="User", labels=c("Small","Normal+Expert"))+
  theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.line = element_line(colour = "black", size = 0.2, linetype = "solid"))+
  ggtitle("User Experience") + theme(plot.title = element_text(face="bold")),
nrow=1,ncol=2)

dev.off()

###################################################################################################################
##### resuscitation intervention outcome
newdata <- ourdata %>%  
  select(Tidal.volume..mL.kg.,Leakage....,peak.inspiratory.pressure..cmH2O.,Rate...min.,Post.end.expiratory.pressure..cmH2O.) %>%
  filter(!is.na(Tidal.volume..mL.kg.)) %>%
  rename("Tidal Volume"="Tidal.volume..mL.kg.", 
         "Mask Leakage"="Leakage....",
         "Peak Inspiratory Pressure"="peak.inspiratory.pressure..cmH2O.",
         "Ventilation Rate"="Rate...min.",
         "Post Expiratory Pressure"="Post.end.expiratory.pressure..cmH2O.")



dev.off()
pdf(file = "C:/Users/Magdalena/meduni/exercise/figures/corendvar.pdf",  width = 7, height = 7, onefile=FALSE) 

corrplot(cor(newdata),method="color",type='lower', col = rep(rev(brewer.pal(n=8, name="RdYlBu")), 2), 
         mar = c(1, 0, 1, 0), tl.col="black", addCoef.col = "black", diag=FALSE, 
         number.cex=0.8, p.mat =cor(newdata), sig.level = 0.1) 
dev.off()

### distribution and tests for binary-integer outcome variables, according to the user type
ourdata %>%
  mutate(intubation=ifelse(intubation==-1,0,intubation),
         intubation=ifelse(intubation==0,2,1)) %>%
  filter(!is.na(Tidal.volume..mL.kg.)) %>%
  freq_table(intubation,all.par) %>%
  select(1:6)


prop.test(x = c(26,69), 
          n = c(861,1547), 
          alternative = "less")

################ logistic regression
ourdata2 <- ourdata %>%
  mutate(intubation=ifelse(intubation==-1,0,intubation))

Vtemodel5d <- glm(Vte ~User.type+ user.experience, data = ourdata2, family = binomial)
summary(Vtemodel5d)$coefficients

Vtemodel5e <- glm(Vte ~User.type+ user.experience+Weight..g., data = ourdata2, family = binomial)
summary(Vtemodel5e)$coefficients

Vtemodel5f <- glm(Vte ~User.type+ user.experience+Weight..g.+intubation, data = ourdata2, family = binomial)
summary(Vtemodel5f)$coefficients

stargazer(Vtemodel5d, Vtemodel5e,Vtemodel5f)


