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

gorilla_spreadsheet <- raw_data %>% 
                    filter(Response %in% c("Beer", "Pier")) %>%
                    #trial numbers are fucked up, need to manually add them
                    #for_merge is used for easy merging of mean pitch w response data
                    group_by(Participant.External.Session.ID) %>%
                    mutate(pID=substring(Participant.External.Session.ID, 20, 24)) %>%
                    mutate(RecordName = str_remove_all(RecordName, "_")) %>%
                    mutate(block_num=ifelse(between(Trial.Number, 1, 42) & display=="Canonical", 1,
                                            ifelse(between(Trial.Number, 1, 42) & display=="Reverse", 2,
                                                   ifelse(between(Trial.Number, 43, 83) & display=="Canonical",3,
                                                          ifelse(between(Trial.Number, 43, 84) & display=="Reverse",4,
                                                                 ifelse(between(Trial.Number, 84, 124) & display=="Canonical",5,
                                                                        ifelse(between(Trial.Number, 84, 124) & display=="Reverse",6,NA)))))))
gorilla_spreadsheet <- gorilla_spreadsheet %>% 
                    mutate(Overall.Experiment.Trial.Number=ifelse(block_num==1, as.numeric(Trial.Number),
                                             ifelse(block_num==2, as.numeric(Trial.Number)+42,
                                               ifelse(block_num==3, as.numeric(Trial.Number)+42,
                                                ifelse(block_num==4, as.numeric(Trial.Number)+83,
                                                 ifelse(block_num==5, as.numeric(Trial.Number)+84,
                                                  ifelse(block_num==6, as.numeric(Trial.Number)+124,NA)))))))

gorilla_spreadsheet <- gorilla_spreadsheet %>% 
                    mutate(weba.Trial.Number=ifelse(block_num==1, as.numeric(Overall.Experiment.Trial.Number),
                                                                               ifelse(block_num==2, as.numeric(Overall.Experiment.Trial.Number)+1,
                                                                                      ifelse(block_num==3, as.numeric(Overall.Experiment.Trial.Number)+2,
                                                                                             ifelse(block_num==4, as.numeric(Overall.Experiment.Trial.Number)+3,
                                                                                                    ifelse(block_num==5, as.numeric(Overall.Experiment.Trial.Number)+4,
                                                                                                           ifelse(block_num==6, as.numeric(Overall.Experiment.Trial.Number)+5,NA)))))))
  
p <- gorilla_spreadsheet %>% filter(pID=="807e2") %>% select(display, block_num, Trial.Number, Overall.Experiment.Trial.Number, weba.Trial.Number)
rm(raw_data)

#Need to grab trial number from files themselves. Can't just asigned sequence due to jumps, missing data

setwd("../output/mean_pitch")
files <- list.files()
csv_file_names <- files
files <- files[grepl(".csv", files)]
files <- vroom(files)
files<- files %>% mutate(Trial.Number.Audio = sapply(strsplit(filename, "-"), "[[", 8)) %>%
  mutate(RecordName.Audio = sapply(strsplit(filename, "-"), "[[", 7)) %>% 
  mutate(ID = sapply(strsplit(filename, "\\\\"), "[[", 9)) %>%
  mutate(ID = sapply(strsplit(ID, "_"), "[[", 1))
files <- files %>% mutate(Trial.Number.Audio = as.numeric(Trial.Number.Audio)) %>%
  group_by(ID,Trial.Number.Audio)
files <- files %>% unite("for_merge", ID, RecordName.Audio, Trial.Number.Audio, remove=FALSE)
gorilla_spreadsheet <- gorilla_spreadsheet %>% unite("for_merge", pID, RecordName, weba.Trial.Number, remove=FALSE)

gorilla_spreadsheet <- merge(gorilla_spreadsheet, files, by="for_merge")
#files <- files %>% unite("for_merge", ID, Trial.Number.Audio, remove=FALSE)
#gorilla_spreadsheet <- gorilla_spreadsheet %>% unite("for_merge", pID, remove=FALSE)
#####

gorilla_spreadsheet <- gorilla_spreadsheet %>% mutate(weba_name = paste(Experiment.ID,"-",
                            Experiment.Version,"-",
                              Participant.Private.ID,"-",
                              Tree.Node.Key,"-",
                              Schedule.ID,"-",
                              display,"SoundSet", SoundSetNumber,
                            "-" , weba.Trial.Number, "-19.weba", sep="")) %>%
                            #select(pID, display, ANSWER_TestStim, block_num,trialnum2, weba_name) %>%
                            filter(block_num==1 | block_num==2) %>%
                            filter(between(Overall.Experiment.Trial.Number, 22,42) | between(Overall.Experiment.Trial.Number,65,85))

#write file
#write.csv(gorilla_spreadsheet, file="C:/Users/anarc/Documents/PerceptionProductionStudy/weba_for_categorization.csv")

#copy files to categorization folder
to_path = "C:/Users/anarc/Documents/PerceptionProductionStudy/productions_for_categorization/"
raw_path = "C:/Users/anarc/Documents/PerceptionProductionStudy/data_pipeline/raw/"

pIDs<- unique(gorilla_spreadsheet$pID)

for(i in 1:length(pIDs)){
  files_to_move <- gorilla_spreadsheet %>% filter(pID==pIDs[i]) %>% select(weba_name)
  files_to_move <- paste0(raw_path, pIDs[i], "/",files_to_move$weba_name)
  file.copy(files_to_move, to_path, overwrite = FALSE)
}

write.csv(gorilla_spreadsheet, "C:\\Users\\anarc\\Documents\\PerceptionProductionStudy\\weba_for_categorization.csv")
