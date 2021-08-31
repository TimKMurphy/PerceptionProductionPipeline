#Plotting functions
#plot group pereptual data
#data.frame - perception_analysis_initialF0


plot_mean_z_by_block <- function(data2plot){
  ggplot(subset(data2plot, ANSWER_TestStim=="HighF0" | ANSWER_TestStim=="LowF0"),
         aes(block_num, Resp.z.score.block.Target.mean, 
             group=ANSWER_TestStim))+
    geom_line(stat="summary",size=2, aes(color=ANSWER_TestStim))+
    geom_point(stat="summary", size = 4, aes(color=ANSWER_TestStim))+
    geom_errorbar(stat="summary", size=1, width=.9,
                  aes(ymax=Resp.z.score.block.Target.mean+Resp.z.score.block.Target.sd,
                      ymin=Resp.z.score.block.Target.mean-Resp.z.score.block.Target.sd))+
    labs(title="Participant Productions by target word over blocks",x="Block", y = "Production - Initial F0 (z-scored)", color = "Target")
}

plot_mean_z_by_block_perception <- function(data2plot){
  ggplot(subset(data2plot, ANSWER_TestStim=="HighF0" | ANSWER_TestStim=="LowF0"),
         aes(block_num, Resp.z.score.block.Target.mean, 
             group=Response))+
    geom_line(stat="summary",size=2, aes(color=Response))+
    geom_point(stat="summary", size = 4, aes(color=Response))+
    geom_errorbar(stat="summary", size=1, width=.9,
                  aes(ymax=Resp.z.score.block.Target.mean+Resp.z.score.block.Target.sd,
                      ymin=Resp.z.score.block.Target.mean-Resp.z.score.block.Target.sd))+
    labs(title="Participant Productions by perceptual response over blocks",
         x="Block", 
         y = "Production - Initial F0 (z-scored)", 
         color = "Perception Response") +
    scale_color_manual(values=c("#E69F00", "#56B4E9"))
}


#individual participant data plotting - z scored octave
#Individual participant data plotting - meanF0
# 
plot_dist_z <- function(pID, block){
  data2plot<- percept_repeat_data %>% filter(ID==pID) %>% filter(display==block)
  ggplot(data2plot, aes(ANSWER_TestStim, Resp.z.score, color=Response))+geom_point(size = 4, alpha=1/5)
}

plot_mean_z <- function(pID, condition){
  data2plot<- percept_repeat_data %>% filter(ID==pID) %>% filter(display==condition)
  ggplot(data2plot, aes(ANSWER_TestStim, Resp.z.score, color=Response))+geom_point(stat="summary", size = 4)
}

#
#
#Perception Data
#
#

#graph - BEER
ggplot(percept_only_data, aes(x=block_num, y=isBeer, color=ANSWER_TestStim))+
  geom_point(stat="summary", size=2, aes(color=ANSWER_TestStim))+
  geom_line(stat="summary", aes(color=ANSWER_TestStim))+
  labs(title="BEER Responses",x="Block", y = "Percentage", color = "Condition")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %  
  geom_errorbar(stat="summary", aes(ymax=group_ci_low_beer, ymin=group_ci_high_beer, color=as.factor(ANSWER_TestStim)), size=1, width=.9)

#graph - Pier
ggplot(percept_only_data, aes(x=block_num, y=isPier, color=ANSWER_TestStim))+
  geom_point(stat="summary", size=2, aes(color=ANSWER_TestStim))+
  geom_line(stat="summary", aes(color=ANSWER_TestStim))+
  labs(title="PIER Responses",x="Block", y = "Percentage", color = "Condition")+
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) + # Multiply by 100 & add %  
  geom_errorbar(stat="summary", aes(ymax=group_ci_low_pier, ymin=group_ci_high_pier, color=as.factor(ANSWER_TestStim)), size=1, width=.9)