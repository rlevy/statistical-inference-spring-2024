library(ggplot2)
library(tidyverse)
library(lme4)

## We will be analyzing data from Boyce & Levy, 2023. 
#  Step 1: clone this repository:
#    https://github.com/vboyce/natural-stories-maze
#  Step 2:
#    Start R and set the working directory to the root directory of the repository
#  Step 3:
#    Run the following getting-started code, installing libraries as needed:

knitr::opts_chunk$set(echo = FALSE, warning=F, message=F)
options(knitr.table.format = "html")
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))
library(plyr)
library(tidyverse)
library(readr)
library(brms)
library(lme4)
library(rstan)
library(tidybayes)
library(knitr)
library(mgcv)
library(mgcViz)
library(tidymv)
library(rsample)
library(cowplot)
library(scales)
library(ggstance)
library(here)
theme_set(theme_bw())
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Step 4: run the following code to get the data ready for our practicum:

data <- read_rds(here("Data/cleaned.rds"))

data_filt <- data %>% filter(native %in% c("ENG", "English", "ENGLISH", "english")) #I peeked at what people put that semantically maps to english

data_error_summ <- data_filt %>% 
  mutate(correct.num=ifelse(correct=="yes", 1,0)) %>% 
  group_by(subject) %>%
  filter(type!="practice") %>% 
  filter(rt<5000) %>% 
  summarize(pct_correct=mean(correct.num)) %>% 
  ungroup() %>% 
  mutate(is.attentive=ifelse(pct_correct>.8, T,F)) %>% 
  select(subject, is.attentive)

data_low_error <- data_filt %>% 
  left_join(data_error_summ, by="subject") %>% 
  filter(is.attentive) %>% 
  filter(type!="practice")

  data_error_free <- data_low_error %>% 
    mutate(word_num_mistake=ifelse(correct=="no", word_num,NA)) %>% 
    group_by(sentence, subject) %>% fill(word_num_mistake) %>% ungroup() %>% 
    mutate(after_mistake=word_num-word_num_mistake,
           after_mistake=ifelse(is.na(after_mistake),0,after_mistake)) %>% 
    filter(correct=="yes") %>% 
    filter(!after_mistake %in% c(1,2))
  
data_no_first <- data_error_free %>% filter(word_num!=0)

data_ready <- data_no_first %>% filter(rt>100 & rt<5000) %>% 
  select(subject, word_num, word, rt, sentence, type)

data_pre_error <- data_no_first %>% filter(rt>100 & rt<5000) %>% 
  filter(after_mistake==0) %>% 
  select(subject, word_num, word, rt, sentence, type) %>% write_rds(here("Data/maze_pre_error.rds"))

data_stories <- data_ready %>% select(type, subject) %>% 
  unique() %>% 
  group_by(type) %>% 
  tally()

labs <- read_rds(here("Prep_code/natural_stories_surprisals.rds")) %>% 
  left_join(read_delim(here("Materials/natural_stories_sentences.tsv"), delim="\t")) %>% 
  select(word_num=Word_In_Sentence_Num, word=Word, sentence=Sentence, everything())

select_labs <- labs %>% 
  mutate(across(ends_with("surp"),~ifelse(ngram_token_count==1 & txl_token_count==1 & grnn_token_count==1 & gpt_token_count==1 & word_num>0, .x, NA))) %>% 
  select(-ends_with("token_count")) %>% 
    mutate(txl_center=txl_surp-mean(txl_surp, na.rm=T),
         ngram_center=ngram_surp-mean(ngram_surp, na.rm=T),
         grnn_center=grnn_surp-mean(grnn_surp, na.rm=T),
         freq_center=freq-mean(freq, na.rm=T),
         length_center=length-mean(length, na.rm=T),
         gpt_center=gpt_surp-mean(gpt_surp, na.rm=T)) %>% 
  group_by(sentence,Story_Num,Sentence_Num) %>% 
  mutate(across(txl_surp:gpt_center,.names="past_{.col}", lag),
         across(txl_surp:gpt_center,.names="past2_{.col}", ~lag(.x,n=2)),
         across(txl_surp:gpt_center,.names="past3_{.col}", ~lag(.x,n=3)))

labelled_pre_error <- data_pre_error %>% inner_join(select_labs, by=c("word_num", "word", "sentence")) %>%
  filter(word_num>1) %>% 
  mutate(Word_ID=as_factor(str_c(Story_Num, Word_In_Story_Num, sep="_")))%>% write_rds(here("Data/maze_pre_error.rds"))
  
dat_gpt2 <- subset(all_data,model=="GPT-2")


# Step 4: follow along with the practicum as below:

word_token_counts <- dat_gpt2 %>%
  group_by(Word_ID) %>%
  summarize(count=n())
ggplot(word_token_counts,aes(x=count)) + geom_histogram()



system.time(m <- lmer(rt ~ surprisal * len + freq * len +
              prev_surp * prev_len + prev_freq * prev_len+
              (surprisal * len + freq * len +
              prev_surp * prev_len + prev_freq * prev_len|subject)+(1|Word_ID),dat_gpt2))
              
system.time(m <- lmer(rt ~ surprisal + freq +  len +
              (surprisal + freq + len|subject)+(1|Word_ID),dat_gpt2,REML=F,control=lmerControl(optimizer="bobyqa")))              


dat_gpt2_by_subject <- dat_gpt2 %>%
  group_by(surprisal,len,freq,Word_ID) %>%
  summarize(mean_RT=mean(rt))

summary(lm(mean_RT ~ surprisal + freq +  len,dat_gpt2_by_subject))
summary(ms$bobyqa)


## now try a subset of participants
set.seed(1)
N <- 8
present_participants <- xtabs(~subject,dat_gpt2)>0
present_participant_ids <- as.numeric(names(present_participants)[present_participants])
participant_subset <- sample(present_participant_ids,N)

dat_subset <- filter(dat_gpt2,subject %in% participant_subset)
word_token_counts <- dat_subset %>%
  group_by(Word_ID) %>%
  summarize(count=n())
ggplot(word_token_counts,aes(x=count)) + geom_histogram()


dat_subset_by_subject <- dat_subset %>%
  group_by(surprisal,len,freq,Word_ID) %>%
  summarize(mean_RT=mean(rt))

system.time(m.subset <- lmer(rt ~ surprisal + freq +  len +
              (surprisal + freq + len|subject)+(1|Word_ID),dat_subset,REML=F,control=lmerControl(optimizer="bobyqa")))              
summary(m.subset)
summary(lm(mean_RT ~ surprisal + freq +  len,dat_subset_by_subject))

