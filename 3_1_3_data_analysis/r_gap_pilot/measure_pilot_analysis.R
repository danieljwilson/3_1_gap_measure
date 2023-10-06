# Load libraries ----
library(tidyverse)
library(data.table)
library(broom)
library(dtplyr)
library(lme4)
library(lmerTest)
library(ggbeeswarm)
library(cowplot)
library(leaflet) # for maps

#source("https://raw.githubusercontent.com/hauselin/Rcode/master/cleanQuestionnaire.R")
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")
source('cleanQuestionnaire.R')

# Import Data ----
## Main ----
dataFile <- "../../3_1_1_raw_data/051121_pilot/Value-Intention-Behavior Gap Measure_October 6, 2021_00.13.csv"

qualtrics.df <- read_csv(dataFile)
qualtrics = as.data.table(qualtrics.df) # convert to data.table
qualtrics = qualtrics[Status=='IP Address'] # select non-preview, non-metadata rows


names(qualtrics)[duplicated(names(qualtrics))] # check duplicated column names
qualtrics[, n_distinct(ResponseId)] # how many subjects?

### Clean subjects ----
#### Remove subjects who don't agree to debrief ----
qualtrics = qualtrics[debrief_form_1 == 'Yes' & debrief_form_2 == 'Yes']

#### Attention check ----
# attention check requires answer of 'Strongly disagree' 
qualtrics = qualtrics[qualtrics$attention_check=='Strongly disagree']

# create subject ID
qualtrics$subjectID = 1:qualtrics[, n_distinct(ResponseId)]

## Validation ----
dataFile <- "../../3_1_1_raw_data/051121_pilot/numeric_Value-Intention-Behavior Gap Measure_October 6, 2021_00.14.csv"

v.qualtrics.df <- read_csv(dataFile)
v.qualtrics = as.data.table(v.qualtrics.df) # convert to data.table
v.qualtrics = v.qualtrics[Status=='0'] # select non-preview, non-metadata rows

### Clean subjects ----
#### Remove subjects who don't agree to debrief ----
v.qualtrics = v.qualtrics[debrief_form_1 == '1' & debrief_form_2 == '1']

#### Attention check ----
# attention check requires answer of 'Strongly disagree' 
v.qualtrics = v.qualtrics[v.qualtrics$attention_check=='1']

# create subject ID
v.qualtrics$subjectID = 1:v.qualtrics[, n_distinct(ResponseId)]

# DEMOGRAPHICS ----
glimpse(qualtrics)

demographics = qualtrics[, list(StartDate, LocationLatitude, LocationLongitude,
                         gender, age, education, father_education, mother_education,
                         marital_status, employment_status, income, parents_income,
                         net_worth, parents_net_worth, job, accomplishments)] # have added some more to this, and need to add race

# See where people are
m = leaflet() %>%
  addTiles() %>%
  addMarkers(lng = as.numeric(demographics$LocationLongitude), lat=as.numeric(demographics$LocationLatitude))
m

# gender
table(demographics$gender)

# age

table(demographics$age) # changing this to a single number entry was 18-40 essentially

# MEASURES ----
# did people think it was an accurate assessment
ggplot(v.qualtrics, aes(as.numeric(questions_accuracy_1))) + geom_histogram(bins = 4)

# suggestions
qualtrics$questions_suggestion

## Intention-Behavior ----

### Action Global ----
action_global_vars = as.data.frame(qualtrics) %>% 
  select(subjectID,
         action_day_actual_1, action_day_belief_1,
         action_week_actual_1, action_week_belief_1,
         action_month_actual_1, action_month_belief_1,
         action_year_actual_1, action_year_belief_1,
         action_overall_1
        )
  
# get average action gap
action_avg = action_global_vars %>%
  select(contains('actual')) %>%
  lapply(as.numeric) %>%
  data.frame() %>%
  rowMeans(na.rm = TRUE)

action_global_vars$action_avg = action_avg

# get average belief gap
belief_avg = action_global_vars %>%
  select(contains('belief')) %>%
  lapply(as.numeric) %>%
  data.frame() %>%
  rowMeans(na.rm = TRUE)

action_global_vars$belief_avg = belief_avg

# calculate difference between actual and belief
action_global_vars$belief_action_gap = action_global_vars$belief_avg - action_global_vars$action_avg

# plot gap


### Action Category ----
action_cat_vars = as.data.frame(qualtrics) %>% 
  select(subjectID,
         starts_with("action_cat"))

# intend average
intend_total = action_cat_vars %>% select(contains('intend')) %>%
  lapply(as.numeric) %>%
  data.frame() %>%
  rowSums(na.rm = TRUE)

action_cat_vars$intend_total = intend_total

action_cat_vars$action_cat_gap = as.numeric(action_cat_vars$intend_total) - 24

# 

# calculate domain gap for each person weighted/unweighted by importance


## Value-Intention ----
# which values did people choose
qualtrics[,values_att_6p_select_1:values_att_6p_select_55]

# VALIDATION MEASURES ----
ambition <- select(v.qualtrics, subjectID, starts_with("ambition")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
briefSelfControl <- select(v.qualtrics, subjectID, starts_with("bscs")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
futureTimePerspective <- select(v.qualtrics, subjectID, starts_with("future")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
SWLCantril <- select(v.qualtrics, subjectID, starts_with("swl")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
secWorkEthic <- select(v.qualtrics, subjectID, starts_with("swe")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
gritShort <- select(v.qualtrics, subjectID, starts_with("sgrit")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
selfEsteemSingle <- select(v.qualtrics, subjectID, starts_with("self_esteem")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()
personalityInventory10 <- select(v.qualtrics, subjectID, starts_with("personality10")) %>% tbl_df() %>% mutate_all(as.numeric) %>% data.table()

# test for people choosing only one score
test_range = socialQs
which(apply(test_range[,-1], 1, range)[2,] - apply(test_range[,-1], 1, range)[1,] ==0)

# perform reliability analysis
factor_data %>% select(paste0('Item', 13:17)) %>% alpha(keys = TRUE)

## Ambition ----
# 1: not at all like me
# 5: very much like me

ambition_s = ambition %>%
  as_tibble() %>%
  mutate(ambition5_4 = 6 - ambition5_4r) %>%
  select(-ambition5_4r) %>%
  mutate(ambition_score = rowMeans(.[,c(2:dim(ambition)[2])])) %>%
  select(subjectID, ambition_score)

ggplot(ambition_s, aes(ambition_score)) + geom_histogram(bins = 5)
ggplot(ambition_s, aes(x = 1, y=ambition_score, fill='red')) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  coord_flip()+
  geom_jitter(aes(color = 'red'), width=0.15, alpha = 0.6)+
  theme(legend.position="none")

## Brief Self Control ----
# 1: not at all like me
# 5: very much like me
# Reverse-scored items: 2, 3, 4, 5, 7, 9, 10, 12, 13

briefSelfControl_s = briefSelfControl %>%
  as_tibble() %>%
  mutate(bscs_2 = 6 - bscs_2_r,
         bscs_3 = 6 - bscs_3_r,
         bscs_4 = 6 - bscs_4_r,
         bscs_5 = 6 - bscs_5_r,
         bscs_7 = 6 - bscs_7_r,
         bscs_9 = 6 - bscs_9_r,
         bscs_10 = 6 - bscs_10_r,
         bscs_12 = 6 - bscs_12_r,
         bscs_13 = 6 - bscs_13_r
         ) %>%
  select(-c(bscs_2_r,a
            bscs_3_r,
            bscs_4_r,
            bscs_5_r,
            bscs_7_r,
            bscs_9_r,
            bscs_10_r,
            bscs_12_r,
            bscs_13_r)) %>%
  mutate(bscs_score = rowMeans(.[,c(2:dim(briefSelfControl)[2])], na.rm = T)) %>%
  select(subjectID, bscs_score)

ggplot(briefSelfControl_s, aes(bscs_score)) + geom_histogram(bins = 5)
ggplot(briefSelfControl_s, aes(x = 1, y=bscs_score, fill='red')) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  coord_flip()+
  geom_jitter(aes(color = 'red'), width=0.15, alpha = 0.6)+
  theme(legend.position="none")

## Future Time Perspective ----
# 1: very untrue
# 7: very true
# Reverse-scored items: 8, 9, 10
# Obtain the participant’s mean score of all 10 items. There are no defined cutoffs for interpreting FTP
# scores. Higher mean scores suggest a more expansive view of the future

futureTimePerspective_s = futureTimePerspective %>%
  as_tibble() %>%
  mutate(future_time_8 = 8 - future_time_8,
         future_time_9 = 8 - future_time_9,
         future_time_10 = 8 - future_time_10
  ) %>%
  mutate(future_score = rowMeans(.[,c(2:dim(futureTimePerspective)[2])], na.rm = T)) %>%
  select(subjectID, future_score)

ggplot(futureTimePerspective_s, aes(future_score)) + geom_histogram(bins = 5)
ggplot(futureTimePerspective_s, aes(x = 1, y=future_score, fill='red')) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  coord_flip()+
  geom_jitter(aes(color = 'red'), width=0.15, alpha = 0.6)+
  theme(legend.position="none")

## Secular Work Ethic ----
# https://docs.google.com/document/d/1_95yYfsq3ndeS64wiNp2tyO7cyRsLIbJGfE9wgqBcGo/edit#heading=h.l4lr03cu53fh
# 1: strongly disagree
# 5: strongly agree
# Reverse-scored items: none
# Note: there are subscales which we are ignoring for the moment

secWorkEthic_s = secWorkEthic %>%
  as_tibble() %>%
  mutate(workEthic_score = rowMeans(.[,c(2:dim(secWorkEthic)[2])], na.rm = T)) %>%
  select(subjectID, workEthic_score)

ggplot(secWorkEthic_s, aes(workEthic_score)) + geom_histogram(bins = 5)
ggplot(secWorkEthic_s, aes(x = 1, y=workEthic_score, fill='red')) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  coord_flip()+
  geom_jitter(aes(color = 'red'), width=0.15, alpha = 0.6)+
  theme(legend.position="none")

## Short Grit ----
# https://docs.google.com/document/d/1LtIqQioBbtFEPGxC2CRPpZxa06E-f4bh88BLa7PjPM8/edit#heading=h.nsmwi64qqspq
# 1: very much like me
# 5: not like me at all
# Reverse-scored items: 2, 4, 7, 8
# Note: there are subscales which we are ignoring for the moment

gritShort_s = gritShort %>%
  as_tibble() %>%
  mutate(sgrit_2 = 6 - sgrit_2_r,
         sgrit_4 = 6 - sgrit_4_r,
         sgrit_7 = 6 - sgrit_7_r,
         sgrit_8 = 6 - sgrit_8_r
  ) %>%
  select(-c(sgrit_2_r,
            sgrit_4_r,
            sgrit_7_r,
            sgrit_8_r)) %>%
  mutate(grit_score = rowMeans(.[,c(2:dim(gritShort)[2])], na.rm = T)) %>%
  select(subjectID, grit_score)

ggplot(gritShort_s, aes(grit_score)) + geom_histogram(bins = 5)
ggplot(gritShort_s, aes(x = 1, y=grit_score, fill='red')) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  coord_flip()+
  geom_jitter(aes(color = 'red'), width=0.15, alpha = 0.6)+
  theme(legend.position="none")

## Personality Inventory (10) ----
# http://gosling.psy.utexas.edu/scales-weve-developed/ten-item-personality-measure-tipi/
# 1: disagree strongly
# 7: agree strongly
# Reverse-scored items: 2, 4, 7, 8
# Note: there are subscales which we are using

personalityInventory10_s = personalityInventory10 %>%
  as_tibble() %>%
  mutate(personality10_2_agr = 8 - personality10_2_agr_r,
         personality10_4_emo = 8 - personality10_4_emo_r,
         personality10_6_ext = 8 - personality10_6_ext_r,
         personality10_8_con = 8 - personality10_8_con_r,
         personality10_10_ote = 8 - personality10_10_ote_r
  ) %>%
  select(-c(personality10_2_agr_r,
            personality10_4_emo_r,
            personality10_6_ext_r,
            personality10_8_con_r,
            personality10_10_ote_r,
            )) %>%
  mutate(tipi_extraversion = rowMeans(.[,c(2,7)], na.rm = T),
         tipi_agreeableness = rowMeans(.[,c(3,8)], na.rm = T),
         tipi_conscientiousness = rowMeans(.[,c(4,9)], na.rm = T),
         tipi_emoStability = rowMeans(.[,c(5,10)], na.rm = T),
         tipi_openExperience = rowMeans(.[,c(6,11)], na.rm = T),
         ) %>%
  select(subjectID, tipi_extraversion, tipi_agreeableness,
         tipi_conscientiousness, tipi_emoStability, tipi_openExperience)

ggplot(personalityInventory10_s, aes(tipi_extraversion)) + geom_histogram(bins = 5)
ggplot(personalityInventory10_s, aes(x = 1, y=tipi_extraversion, fill='red')) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0)) +
  coord_flip()+
  geom_jitter(aes(color = 'red'), width=0.15, alpha = 0.6)+
  theme(legend.position="none")

# join all scales
validation_measures = merge(selfEsteemSingle, SWLCantril, by = 'subjectID') %>%
  merge(personalityInventory10_s, by= 'subjectID') %>%
  merge(gritShort_s, by= 'subjectID') %>%
  merge(secWorkEthic_s, by= 'subjectID') %>%
  merge(futureTimePerspective_s, by= 'subjectID') %>%
  merge(briefSelfControl_s, by= 'subjectID') %>%
  merge(ambition_s, by= 'subjectID')

# ANALYSIS ----
## Validation Measures ----
# correlations between validation measures
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(validation_measures[,2:dim(validation_measures)[2]], histogram=TRUE, pch=19)

## IB Gap ----
### IB Gap Basic vs. Validation ----
# correlations between validation measures and IB gap
ib_gap = qualtrics[, c('subjectID', 'overall_success_pre_1', 'overall_success_post_1',
                       'action_overall_1', 'value_overall_1')]

# convert to numeric
ib_gap <- data.frame(data.matrix(ib_gap))

# merge with validation df
validation_ibGap = merge(validation_measures, ib_gap, by= 'subjectID')

# look at correlation
chart.Correlation(validation_ibGap[,2:dim(validation_ibGap)[2]], histogram=TRUE, pch=19)

### Eisenhower Matrix vs. Validation ----
# eisenhower quadrants
eisenhower = qualtrics[, c('subjectID', 'action_plan_eisenhow_1',
                       'action_plan_eisenhow_2', 'action_plan_eisenhow_3',
                       'action_plan_eisenhow_4')]

# convert to numeric
eisenhower <- data.frame(data.matrix(eisenhower))

# merge with validation df
validation_eisenhower = merge(validation_measures, eisenhower, by= 'subjectID')

# look at correlation
chart.Correlation(validation_eisenhower[,2:dim(validation_eisenhower)[2]], histogram=TRUE, pch=19)

# mean of goal success for each person



