#Correlation by subject by block
percept_repeat_data <- percept_repeat_data %>%
  group_by(ID )%>%
  mutate(experiment_corr = cor(initialOctave, F0, method = "pearson")) %>%
  group_by(ID, display) %>%
  mutate(condition_corr = cor(initialOctave, F0, method = "pearson")) %>%
  group_by(ID, block_num, display) %>%
  mutate(block_corr = cor(initialOctave, F0, method = "pearson"))

ggplot(percept_repeat_data, aes(block_num, block_corr, group=pID))+geom_point(aes(color=pID))+geom_line(aes(color=pID))

#look at changes over lagged data
lagged <- percept_repeat_data %>% 
              select(ID, ANSWER_TestStim, Resp.z.score, Response, trialnum2, display) %>%
              arrange(ID, trialnum2) %>%
              group_by(ID) %>%
              mutate(prev = lag(ANSWER_TestStim)) %>%
              mutate(is_same = ifelse(ANSWER_TestStim==prev, TRUE, FALSE)) %>% 
              filter(ANSWER_TestStim=="HighF0" | ANSWER_TestStim=="LowF0") %>%
              filter(!is.na(is_same)) %>%
              filter(Resp.z.score<=2.5 & Resp.z.score>=-2.5)



three_plots_by_trial<- function(d){
  print(ggplot(d, aes(trialnum2, Resp.z.score, color=display))+geom_point())
  print(ggplot(d, aes(trialnum2, Resp.z.score, color=Response))+geom_point()+ scale_color_manual(values=c("#E69F00", "#56B4E9")))
  print(ggplot(d, aes(trialnum2, Resp.z.score, color=ANSWER_TestStim))+geom_point())
}

perception_by_trial <- function (d){
  print(ggplot(d, aes(Response, trialnum2, color=ANSWER_TestStim))+geom_point(pch = 21, position = position_jitterdodge()))
}

#ggplot(ambi_only, aes(ANSWER_TestStim, Resp.z.score, color=is_same)) +geom_point()
