library(tidyverse)
library(ggplot2)
library(lme4)

#requires percept_repeat_data dataframe from main script
#adds up responses for summary dataframe
percept_only_data <- percept_repeat_data %>% 
  select(Response, Participant.External.Session.ID, display, ANSWER_TestStim, block_num) %>%
  mutate(ID = Participant.External.Session.ID) %>%
  mutate(isBeer=ifelse(Response=="Beer", 1, 0)) %>%
  mutate(isPier=ifelse(Response=="Pier", 1, 0)) %>%
  group_by(Participant.External.Session.ID) %>%
  mutate(totalBeer=sum(isBeer)) %>%
  mutate(totalPier=sum(isPier)) %>%
  group_by(Participant.External.Session.ID) %>%
  mutate(totalResp=totalBeer+totalPier) %>% 
  group_by(display, ANSWER_TestStim) %>%
  mutate(group_ci_low_beer=mean(isBeer)+(sd(isBeer)/sqrt(length(isBeer)))*2) %>% 
  mutate(group_ci_high_beer=mean(isBeer)-(sd(isBeer)/sqrt(length(isBeer)))*2) %>%
  mutate(group_ci_low_pier=mean(isPier)+(sd(isPier)/sqrt(length(isPier)))*2) %>% 
  mutate(group_ci_high_pier=mean(isPier)-(sd(isPier)/sqrt(length(isPier)))*2) %>%
  group_by(ID) %>% 
  filter(ANSWER_TestStim=="HighF0" | ANSWER_TestStim=="LowF0")

#graph - BEER
ggplot(percept_only_data, aes(x=ANSWER_TestStim, y=isBeer, group=block_num))+
  geom_point(stat="summary", size=2, aes(color=display))+
  labs(title="BEER Responses",x="TARGET", y = "Percentage", color = "Condition")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %  
  geom_errorbar(aes(ymax=group_ci_low_beer, ymin=group_ci_high_beer, color=as.factor(display)), size=1, width=.9)

#graph - Pier
ggplot(percept_only_data, aes(x=ANSWER_TestStim, y=isPier, group=block_num))+
  geom_point(stat="summary", size=2, aes(color=display))+
  labs(title="PIER Responses",x="TARGET", y = "Percentage", color = "Condition")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %  
  geom_errorbar(aes(ymax=group_ci_low_pier, ymin=group_ci_high_pier, color=as.factor(display)), size=1, width=.9)


percept_only_data.lme4 <- lmer(isBeer ~ display + ANSWER_TestStim + display*ANSWER_TestStim + (1|ID), data=percept_only_data)
summary(percept_only_data.lme4)