library(tidyverse)
library(vroom)
library(ggplot2)
setwd("C:/Users/anarc/Documents/PerceptionProductionStudy/data_pipeline/behavioral/pilot")

#import raw_data, create response_data, delete raw_data variable
files <- list.files()
raw_data <- read.csv(files[1])
response_data <- raw_data %>% select(Participant.Public.ID, 
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
                    group_by(Participant.Public.ID) %>%
                    mutate(trialnum2=matrix(seq(1:248)))
rm(raw_data)

#import mean F0 data, add to response data
setwd("../output/mean_pitch/pilot")
files <- list.files()
mean_pitch <- vroom(files)
mean_pitch <- mean_pitch %>% mutate(Trial.Number.Audio = sapply(strsplit(filename, "-"), "[[", 8)) %>%
                            mutate(RecordName.Audio = sapply(strsplit(filename, "-"), "[[", 7)) %>% 
                            mutate(ID = sapply(strsplit(filename, "\\\\"), "[[", 9)) %>%
                            mutate(ID = sapply(strsplit(ID, "_"), "[[", 1)) %>%
                             mutate(ID = ifelse(ID=="Erin", "Erin_1", ID))
mean_pitch <- mean_pitch %>% unite("for_merge", ID, Trial.Number.Audio, remove=FALSE)
response_data <- response_data %>% unite("for_merge", Participant.Public.ID, trialnum2, remove=FALSE)

percept_repeat_data <- merge(response_data, mean_pitch, by="for_merge")
#turn meanF0 string to numeric
percept_repeat_data <- percept_repeat_data %>% mutate(meanF0=as.numeric(sub(" Hz", "", meanF0)))
percept_repeat_data <- percept_repeat_data %>% mutate(meanOctave=log2(meanF0))
rm(mean_pitch, response_data)

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

BTW <- percept_repeat_data %>% filter(ID=="BTW")
Erin_1 <- percept_repeat_data %>% filter(ID=="Erin_1")
airvine <- percept_repeat_data %>% filter(ID=="airvine")
ER5 <- percept_repeat_data %>% filter(ID=="ER5")

ggplot(BTW, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)
ggplot(Erin_1, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)
ggplot(airvine, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)
ggplot(ER5, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)

#BTW
BTW_canon <- canon_resp %>% filter(ID=="BTW")
ggplot(BTW_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(100,200)
ggplot(BTW_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)+ylim(100,200)
BTW_rev <- rev_resp %>% filter(ID=="BTW")
ggplot(BTW_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(100,200)
ggplot(BTW_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat = "summary", size = 4)+ylim(100,200)

#Erin_1
Erin_1_canon <- canon_resp %>% filter(ID=="Erin_1")
ggplot(Erin_1_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(175,250)
ggplot(Erin_1_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)+ylim(175,250)
Erin_1_rev <- rev_resp %>% filter(ID=="Erin_1")
ggplot(Erin_1_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(175,250)
ggplot(Erin_1_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat = "summary", size = 4)+ylim(175,250)


#airvine

airvine_canon <- canon_resp %>% filter(ID=="airvine")
ggplot(airvine_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(175,250)
ggplot(airvine_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)+ylim(175,250)
airvine_rev <- rev_resp %>% filter(ID=="airvine")
ggplot(airvine_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(175,250)
ggplot(airvine_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat = "summary", size = 4)+ylim(175,250)

#ER5
ER5_canon <- canon_resp %>% filter(ID=="ER5")
ggplot(ER5_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(100,250)
ggplot(ER5_canon, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat="summary", size = 4)+ylim(100,250)
ER5_rev <- rev_resp %>% filter(ID=="ER5")
ggplot(ER5_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(size = 4, alpha=1/5)+ylim(100,250)
ggplot(ER5_rev, aes(ANSWER_TestStim, meanF0, color=Response))+geom_point(stat = "summary", size = 4)+ylim(100,250)
