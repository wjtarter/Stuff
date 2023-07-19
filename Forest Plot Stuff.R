library(tidyverse)
dat<- data.frame( 
  
  outcome_var = c("Post-Access", "Race: White", "Race: Other","Ethnicity: Not Hispanic or Latino", "Age", "Asthma:Yes", "Allergies:Yes", "T1DM:Yes", "Epilepsy_Seizures:Yes"),
  
  IRR= c(0.66, 1.18, 1.0, 1.30,0.95, 2.04, 1.35, 0.61, 1.09),
  
  LL=c(0.53,0.65,0.52, 0.52, 0.90, 1.40, 0.89, 0.20, 0.72),
  
  UL=c(0.83,2.14,1.92, 1.92, 0.99, 3.08, 2.05, 1.89, 1.66)
  
) %>%
  mutate(Model = "ED Visit IRR")


dat2<- data.frame(
  
  outcome_var = c("Post-Access", "Race: White", "Race: Other","Ethnicity: Not Hispanic or Latino", "Age", "Asthma:Yes", "Allergies:Yes", "T1DM:Yes", "Epilepsy_Seizures:Yes"),
  
  IRR= c(0.56, 1.45, 1.36, 1.73, 0.97, 1.61, 1.75, 0.81, 1.17),
  
  LL=c(0.44,0.81,0.73, 1.12, 0.93, 1.07, 1.17, 0.27, 0.78),
  
  UL=c(0.72, 2.61, 2.64, 2.66, 1.02, 2.42, 2.64, 2.40, 1.75)
  
) %>%
  mutate(Model="Hospitalizations IRR")

dat_full<-rbind(dat,
                dat2)

ggplot(dat_full,aes(y=outcome_var,x= IRR , col=Model))+
  
  geom_point(shape = 18, size = 5, position = position_dodge(width = 0.5)) + 
  
  geom_errorbarh(aes(xmin = LL, xmax = UL), 
                 height = 0.25,position = position_dodge(width = 0.5)) +
  labs(title="Regression: Hospitalizations and Patient Outcome Variables", 
       y= "Patient Outcome Variables", x= "Hospitalizations IRR")+
  scale_color_manual(values = c("black","dark grey"))+
  facet_grid(.~Model)
