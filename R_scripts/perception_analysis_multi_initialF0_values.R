library(tidyverse)
library(vroom)
library(ggplot2)
setwd("C:/Users/anarc/Documents/PerceptionProductionStudy/data_pipeline/behavioral")

#import raw_data, create response_data, delete raw_data variable
files <- list.files()
data_v3 <- read.csv(files[1])
data_v4 <- read.csv(files[2])
raw_data=rbind(data_v3, data_v4)
rm(data_v3, data_v4)

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
percept_repeat_data <- percept_repeat_data %>% mutate(f01=as.numeric(sub(" Hz", "", f01)))
percept_repeat_data <- percept_repeat_data %>% mutate(f02=as.numeric(sub(" Hz", "", f02)))
percept_repeat_data <- percept_repeat_data %>% mutate(f03=as.numeric(sub(" Hz", "", f03)))
percept_repeat_data <- percept_repeat_data %>% mutate(f04=as.numeric(sub(" Hz", "", f04)))
percept_repeat_data <- percept_repeat_data %>% mutate(f05=as.numeric(sub(" Hz", "", f05)))

correlation_data <- percept_repeat_data %>% 
  group_by(pID) %>%
  mutate(corr_f01_f02 = cor(f01,f02)) %>%
  mutate(corr_f02_f03 = cor(f02,f03)) %>%
  mutate(corr_f03_f04 = cor(f03,f04)) %>%
  mutate(corr_f04_f05 = cor(f04,f05)) %>%
  select(pID,corr_f01_f02, corr_f02_f03, corr_f03_f04, corr_f04_f05)
                    



percept_repeat_data <- percept_repeat_data %>% mutate(initialOctave=log2(initialF0)/12)
rm(mean_pitch, response_data)

#Z-score. individual Z-score for each individual, by response
#performed only on octave (log transformed) data
percept_repeat_data <- percept_repeat_data %>% 
                      group_by(ID) %>% 
                      mutate(Resp.z.sdInitialOctave=sd(initialOctave)) %>% 
                      mutate(Resp.z.initialOctave=mean(initialOctave))
percept_repeat_data <- percept_repeat_data %>%
                      mutate(Resp.z.score=(initialOctave-Resp.z.initialOctave)/Resp.z.sdInitialOctave)

#get the F0 and VoT values of the sounds presented
#get averages of sounds
return_F0_VOT <- function(sounds, value)
{
  if(value=="F0")
  {
    new_names <- sapply(strsplit(sounds, "_|\\.| "), "[", 4)
  }
  else if(value=="VoT")
  {
    new_names <- sapply(strsplit(sounds, "_|\\.| "), "[", 5)
    new_names <- ifelse(new_names=="n10", -10, new_names)
  }
  new_names=as.numeric(new_names)
  return(new_names)
}

#apply function above - Gets F0, then Vot values
percept_repeat_data <- percept_repeat_data %>% 
  mutate_at(c("Sound1", "Sound2", "Sound3", "Sound4","Sound5", "Sound6","Sound7", "Sound8"),
            list(f0=return_F0_VOT), "F0") %>%
  mutate_at(c("Sound1", "Sound2", "Sound3", "Sound4","Sound5", "Sound6","Sound7", "Sound8"),
            list(vot=return_F0_VOT), "VoT") %>%
  mutate(mean.present.F0 = (Sound1_f0+Sound2_f0+Sound3_f0+Sound4_f0+Sound5_f0+Sound6_f0+Sound7_f0+Sound8_f0)/8) %>%
  mutate(mean.present.VoT = (Sound1_vot+Sound2_vot+Sound3_vot+Sound4_vot+Sound5_vot+Sound6_vot+Sound7_vot+Sound8_vot)/8)

#blocks
#
# Processing from here down works with summary data for participants by block
#

#block 1: 1-42
#block 2: 43-84
#block 2: 85-125
#block 2: 126-167
#block 2: 168-208
#block 2: 209-248
percept_repeat_data <- percept_repeat_data %>% 
  mutate(block_num=ifelse(between(trialnum2, 1, 42),1,
                          ifelse(between(trialnum2, 43, 84),2,
                                 ifelse(between(trialnum2, 85, 125),3,
                                        ifelse(between(trialnum2, 126, 167),4,
                                               ifelse(between(trialnum2, 168, 208),5,
                                                      ifelse(between(trialnum2, 209, 248),6,NA)))))))

# Mean and SE of z-scored initial F0 by participant by block
percept_repeat_data <- percept_repeat_data %>%
  group_by(ID, block_num, ANSWER_TestStim) %>%
    mutate(Resp.z.score.block.Target.mean = mean(Resp.z.score)) %>%
      group_by(ID, block_num, ANSWER_TestStim) %>%
      mutate(Resp.z.score.block.Target.sd= sd(Resp.z.score))

#
#
#Perception Only
#
#

#requires percept_repeat_data dataframe from main script
#adds up responses for summary dataframe
percept_only_data <- percept_repeat_data %>% 
  select(Response, Participant.External.Session.ID, display, ANSWER_TestStim, block_num) %>%
  mutate(ID = Participant.External.Session.ID) %>%
  mutate(isBeer=ifelse(Response=="Beer", 1, 0)) %>%
  mutate(isPier=ifelse(Response=="Pier", 1, 0)) %>%
  group_by(ID, block_num) %>%
  mutate(totalBeer=sum(isBeer)) %>%
  mutate(totalPier=sum(isPier)) %>%
  group_by(ID, block_num) %>%
  mutate(totalResp=totalBeer+totalPier) %>% 
  group_by(ID, block_num) %>%
  mutate(group_ci_low_beer=mean(isBeer)+(sd(isBeer)/sqrt(length(isBeer)))*2) %>% 
  mutate(group_ci_high_beer=mean(isBeer)-(sd(isBeer)/sqrt(length(isBeer)))*2) %>%
  mutate(group_ci_low_pier=mean(isPier)+(sd(isPier)/sqrt(length(isPier)))*2) %>% 
  mutate(group_ci_high_pier=mean(isPier)-(sd(isPier)/sqrt(length(isPier)))*2) %>%
  group_by(ID) %>% 
  filter(ANSWER_TestStim=="HighF0" | ANSWER_TestStim=="LowF0")
