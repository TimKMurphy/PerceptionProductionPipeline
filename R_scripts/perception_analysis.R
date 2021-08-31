library(tidyverse)
library(vroom)
library(ggplot2)
setwd("C:/Users/anarc/Documents/PerceptionProductionStudy/data_pipeline/behavioral")

#import raw_data, create response_data, delete raw_data variable
files <- list.files()
raw_data <- read.csv(files[1])
response_data <- raw_data %>% select(Participant.External.Session.ID, 
                    Trial.Number,
                    Response,
                    display,
                    RecordName,
                    ANSWER_TestStim,
                    Screen.Name, 
                    Reaction.Time, 
                    TestStim,
                    Sound1,Sound2,Sound3,Sound4,Sound5,Sound6,Sound7,Sound8,
                    VOT,
                    F0,
                    SoundSetNumber,
                    ImageSetNumber) %>% 
                    filter(Response %in% c("Beer", "Pier")) %>%
                    #trial numbers are fucked up, need to manually add them
                    #for_merge is used for easy merging of mean pitch w response data
                    group_by(Participant.External.Session.ID) %>%
                    group_by(pID=substring(Participant.External.Session.ID, 20, 24)) %>%
                    mutate(trialnum2=matrix(seq(1:248)))
rm(raw_data)

#import mean F0 data, add to response data
setwd("../output/mean_pitch")
files <- list.files()
files = files[grepl(".csv", files)]
mean_pitch <- vroom(files)
mean_pitch <- mean_pitch %>% mutate(Trial.Number.Audio = sapply(strsplit(filename, "-"), "[[", 8)) %>%
                            mutate(RecordName.Audio = sapply(strsplit(filename, "-"), "[[", 7)) %>% 
                            mutate(ID = sapply(strsplit(filename, "\\\\"), "[[", 9)) %>%
                            mutate(ID = sapply(strsplit(ID, "_"), "[[", 1))
mean_pitch <- mean_pitch %>% unite("for_merge", ID, Trial.Number.Audio, remove=FALSE)
response_data <- response_data %>% unite("for_merge", pID, trialnum2, remove=FALSE)

percept_repeat_data <- merge(response_data, mean_pitch, by="for_merge")

#turn meanF0 string to numeric
percept_repeat_data <- percept_repeat_data %>% mutate(meanF0=as.numeric(sub(" Hz", "", meanF0)))
percept_repeat_data <- percept_repeat_data %>% mutate(meanOctave=log2(meanF0))
rm(mean_pitch, response_data)

#Z-score. individual Z-score for each individual, by response
#performed only on octave (log transformed) data
percept_repeat_data <- percept_repeat_data %>% 
                      group_by(ID, Response) %>% 
                      mutate(Resp.z.sdOctave=sd(meanOctave)) %>% 
                      mutate(Resp.z.meanOctave=mean(meanOctave))
percept_repeat_data <- percept_repeat_data %>%
                      mutate(Resp.z.score=(meanOctave-Resp.z.meanOctave)/Resp.z.sdOctave)


#plot perecptual judgement responses by block
canon_resp <- percept_repeat_data %>% filter(display=="Canonical") 
rev_resp <- percept_repeat_data %>% filter(display=="Reverse") 

canon_plot <- table(canon_resp$Response, canon_resp$ANSWER_TestStim)
barplot(canon_plot, beside=TRUE)

rev_plot <- table(rev_resp$Response, rev_resp$ANSWER_TestStim)
barplot(rev_plot, beside=TRUE)

#plot F0s by ID

ggplot(canon_resp, aes(ID, meanF0, color=Response))+geom_point(stat="summary", size = 4)
ggplot(rev_resp, aes(ID, meanF0, color=Response))+geom_point(stat="summary", size=4)


#plot z-scored octave across ID
ggplot(canon_resp, aes(ANSWER_TestStim, Resp.z.score, color=Response))+geom_point(stat="summary", size = 4)
ggplot(rev_resp, aes(ANSWER_TestStim, Resp.z.score, color=Response))+geom_point(stat="summary", size=4)


#Individual participant data plotting - meanF0
# 
plot_dist <- function(pID, block){
    data2plot<- percept_repeat_data %>% filter(ID==pID) %>% filter(display==block)
    ggplot(data2plot, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)
}

plot_mean <- function(pID, block){
  data2plot<- percept_repeat_data %>% filter(ID==pID) %>% filter(display==block)
  ggplot(data2plot, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)
}

#individual participant data plotting - z scored octave
#Individual participant data plotting - meanF0
# 
plot_dist_z <- function(pID, block){
  data2plot<- percept_repeat_data %>% filter(ID==pID) %>% filter(display==block)
  ggplot(data2plot, aes(ANSWER_TestStim, Resp.z.score, color=Response))+geom_point(size = 4, alpha=1/5)
}

plot_mean_z <- function(pID, block){
  data2plot<- percept_repeat_data %>% filter(ID==pID) %>% filter(display==block)
  ggplot(data2plot, aes(ANSWER_TestStim, Resp.z.scoure, color=Response))+geom_point(stat="summary", size = 4)
}