repetition_data <- percept_repeat_data %>%
  select(display, Resp.z.score, ANSWER_TestStim) %>%
  group_by(display, ANSWER_TestStim) %>%
  mutate(group_ci_low=mean(Resp.z.score)+(sd(Resp.z.score)/sqrt(length(Resp.z.score)))*2) %>% 
  mutate(group_ci_high=mean(Resp.z.score)-(sd(Resp.z.score)/sqrt(length(Resp.z.score)))*2)

colnames(repetition_data) <- c("ID", "Condition", "F0", "PromptedCue", "CIlow", "CIhigh")

repetition_data.lm <- lm(F0 ~ Condition + PromptedCue + Condition*PromptedCue, data=repetition_data)
anova(repetition_data.lm)
summary(repetition_data.lm)

ggplot(repetition_data, aes(x=PromptedCue, y=F0, group=Condition))+
    geom_point(stat="summary", size = 4,aes(color=Condition))+
    geom_errorbar(aes(ymax=CIlow, ymin=CIhigh, color=as.factor(Condition)), size=1, width=.9)+
    ylim(-0.1,0.2)

