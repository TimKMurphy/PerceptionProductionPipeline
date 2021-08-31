library(tidyverse)
library(vroom)
library(ggplot2)
library(filesstrings)
setwd("C:/Users/anarc/Documents/PerceptionProductionStudy/data_pipeline/behavioral")

#import raw_data, create response_data, delete raw_data variable
files <- list.files()
data_v3 <- read.csv(files[1])
data_v4 <- read.csv(files[2])
raw_data=rbind(data_v3, data_v4)
rm(data_v3, data_v4)

#Following section adds in information to ensure match between behavioral data and recorded audio
#Public ID is reduced to 5 digit identifier for ease of use
#Block number is added first. Because Trial.Number counts Canonical and Reverse Seperately,
#need to manually assign which trials belong to which block
#variable "display" is essentially condition
gorilla_spreadsheet <- raw_data %>% 
  filter(Response %in% c("Beer", "Pier")) %>%
  group_by(Participant.External.Session.ID) %>%
  mutate(pID=substring(Participant.External.Session.ID, 20, 24)) %>%
  mutate(RecordName = str_remove_all(RecordName, "_")) %>%
  mutate(block_num=ifelse(between(Trial.Number, 1, 42) & display=="Canonical", 1,
                          ifelse(between(Trial.Number, 1, 42) & display=="Reverse", 2,
                                 ifelse(between(Trial.Number, 43, 83) & display=="Canonical",3,
                                        ifelse(between(Trial.Number, 43, 84) & display=="Reverse",4,
                                               ifelse(between(Trial.Number, 84, 124) & display=="Canonical",5,
                                                      ifelse(between(Trial.Number, 84, 124) & display=="Reverse",6,NA)))))))
#Code below uses block information just added 
#to convert Trial.Number (trial number within condition, Canonical or Reverse)
#to Overall.Experiment.Trial.Number, which as name suggests is Trial Number across whole experiment
gorilla_spreadsheet <- gorilla_spreadsheet %>% 
  mutate(Overall.Experiment.Trial.Number=ifelse(block_num==1, as.numeric(Trial.Number),
                                                ifelse(block_num==2, as.numeric(Trial.Number)+42,
                                                       ifelse(block_num==3, as.numeric(Trial.Number)+42,
                                                              ifelse(block_num==4, as.numeric(Trial.Number)+83,
                                                                     ifelse(block_num==5, as.numeric(Trial.Number)+84,
                                                                            ifelse(block_num==6, as.numeric(Trial.Number)+124,NA)))))))
#Gorilla named audio recordings based on when they were encounter
#However, Gorilla seemed to skip over certain numbers between blocks
#for instance, the last recording in block one is numbered 42 and the first of block 2 is 44
#There does not exists a file with "43". This is likely due to the break participants took at these times
#The audio recording is recorded at a ".weba" file, hence the name weba.Trial.Number
gorilla_spreadsheet <- gorilla_spreadsheet %>% 
  mutate(weba.Trial.Number=ifelse(block_num==1, as.numeric(Overall.Experiment.Trial.Number),
                                  ifelse(block_num==2, as.numeric(Overall.Experiment.Trial.Number)+1,
                                         ifelse(block_num==3, as.numeric(Overall.Experiment.Trial.Number)+2,
                                                ifelse(block_num==4, as.numeric(Overall.Experiment.Trial.Number)+3,
                                                       ifelse(block_num==5, as.numeric(Overall.Experiment.Trial.Number)+4,
                                                              ifelse(block_num==6, as.numeric(Overall.Experiment.Trial.Number)+5,NA)))))))

#rm(raw_data)

#The code below imports F0 values from csv spreadsheet output by praat script
setwd("../output/mean_pitch")
files <- list.files()
csv_file_names <- files
files <- files[grepl(".csv", files)]
files <- vroom(files)

#The code below looks at the filename as specified in the csv and creates
#ID, Trial.Number.Audio (which should match weba.Trial.Number in data.frame gorilla_spreadsheet)
#and RecordName.Audio, which should match RecordName in data.frame gorilla_spreadsheet
files<- files %>% mutate(Trial.Number.Audio = sapply(strsplit(filename, "-"), "[[", 8)) %>%
  mutate(RecordName.Audio = sapply(strsplit(filename, "-"), "[[", 7)) %>% 
  mutate(ID = sapply(strsplit(filename, "\\\\"), "[[", 9)) %>%
  mutate(ID = sapply(strsplit(ID, "_"), "[[", 1))
files <- files %>% mutate(Trial.Number.Audio = as.numeric(Trial.Number.Audio)) %>%
  group_by(ID,Trial.Number.Audio)

#Creats "for_merge", a variable used to merge gorilla_spreadsheet and files (praat data)
files <- files %>% unite("for_merge", ID, RecordName.Audio, Trial.Number.Audio, remove=FALSE)
gorilla_spreadsheet <- gorilla_spreadsheet %>% unite("for_merge", pID, RecordName, weba.Trial.Number, remove=FALSE)
gorilla_spreadsheet <- merge(gorilla_spreadsheet, files, by="for_merge")


#
#
#
#

#turn meanF0 string to numeric
percept_repeat_data <- percept_repeat_data %>% mutate(initialF0=as.numeric(sub(" Hz", "", initialF0)))
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
