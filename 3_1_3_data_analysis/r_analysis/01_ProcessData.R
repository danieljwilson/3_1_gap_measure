###### Import Libraries ######
  library(data.table)
  library(tidyverse)
  library(readxl)
  
  
  #/////////////////////////////////#
  ###### Import and Clean Data ######
  #/////////////////////////////////#
  
  ## Import participants from cohort 1 and 2
  cohort1 = read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_1/run1_subjects.csv')
  cohort2 = read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_2/run2_subjects.csv')
  
  ## Import qualtrics cohort 1 and 2
  
  #//////////#
  # Cohort 1 #
  #//////////#
  
  qualtrics_1 = as.data.table(read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_1/run1_qualtrics_onboardingOffboarding.csv'))
  q1 = qualtrics_1[Status=='0'] # select non-preview, non-metadata rows
  
  # indicate whether the entry is for onboarding or offboarding
  q1$onOff = ''
  q1[q1$EndDate > "2022-12-15",]$onOff = 'offBoarding'
  q1[q1$EndDate <= "2022-12-15",]$onOff = 'onBoarding'
  
  # select only those in cohort 1
  q1 <- q1[consent_sonaID %in% cohort1$Student.Number]
  
  # Find duplicates based on the "sonaID" column (which is actually student number)
  # onboarding
  q1on = q1[q1$onOff == 'onBoarding']
  
  group <- as.data.table(q1on)
  q1on = group[group[, .I[which.max(Progress)], by=consent_sonaID]$V1]
  
  # add Participant ID
  q1on$Student.Number = as.numeric(q1on$consent_sonaID)
  q1on = merge(q1on, cohort1[c('ParticipantIdentifier', 'Student.Number')], by = 'Student.Number', all.x = TRUE) # left join
  
  # offboarding
  q1off = q1[q1$onOff == 'offBoarding']
  group <- as.data.table(q1off)
  q1off = group[group[, .I[which.max(Progress)], by=consent_sonaID]$V1]
  
  # add Participant ID
  q1off$Student.Number = as.numeric(q1off$consent_sonaID)
  q1off = merge(q1off, cohort1[c('ParticipantIdentifier', 'Student.Number')], by = 'Student.Number', all.x = TRUE) # left join
  
  
  # combine on and offboarding
  q1 = bind_rows(q1on, q1off)
  
  #//////////#
  # Cohort 2 #
  #//////////#
  
  # onboarding
  qualtrics_2on1 = as.data.table(read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_2/qualtrics/run2_on_ibGap_part1.csv'))
  qualtrics_2on1 = qualtrics_2on1[Status=='0'] # select non-preview, non-metadata rows
  qualtrics_2on1 <- qualtrics_2on1[consent_sonaID %in% cohort2$Student.Number]
  # select only those after mid-December
  qualtrics_2on1 = qualtrics_2on1[qualtrics_2on1$EndDate > '2022-12-13']
  
  
  qualtrics_2on2 = as.data.table(read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_2/qualtrics/run2_on_ibGap_part2.csv'))
  qualtrics_2on2 = qualtrics_2on2[Status=='0'] # select non-preview, non-metadata rows
  qualtrics_2on2 <- qualtrics_2on2[consent_sonaID %in% cohort2$Student.Number]
  
  # for some reason the time and effort columns got messed up and have an 'x' in front of the number of the domain
  # for example:
  colnames(qualtrics_2on2)[grep("domain_time_", colnames(qualtrics_2on2))]
  # it looks like 'time' and 'effort' dimensions of domains have this issue
  colnames(qualtrics_2on2)[grep("x1", colnames(qualtrics_2on2))]
  
  # rename
  time_cols = colnames(qualtrics_2on2)[grep("domain_time_", colnames(qualtrics_2on2))]
  effort_cols = colnames(qualtrics_2on2)[grep("domain_effort_", colnames(qualtrics_2on2))]
  
  # Define the letter to remove
  letter_to_remove <- "x"
  
  # Rename columns by removing the specified letter
  for (col in time_cols) {
    new_col_name <- gsub(letter_to_remove, "", col)
    colnames(qualtrics_2on2)[colnames(qualtrics_2on2) == col] <- new_col_name
  }
  
  for (col in effort_cols) {
    new_col_name <- gsub(letter_to_remove, "", col)
    colnames(qualtrics_2on2)[colnames(qualtrics_2on2) == col] <- new_col_name
  }
  
  # see if we can also get data from people who completed Qualtrics on SONA in the fall and then took part in cohort 2
  qualtrics_2on3 = as.data.table(read.csv('/Users/djw/Downloads/Intention-Behavior+Gap+Measure+-+SONA+-+Oct+2022_August+15,+2023_08.47.csv'))
  qualtrics_2on3 = qualtrics_2on3[Status=='0'] # select non-preview, non-metadata rows
  qualtrics_2on3 <- qualtrics_2on3[consent_sonaID %in% cohort2$Student.Number]
  
  
  # Combine onboarding
  q2on = bind_rows(qualtrics_2on1, qualtrics_2on2)
  q2on = bind_rows(q2on, qualtrics_2on3)
  
  # Name
  q2on$onOff = 'onBoarding'
  # Find duplicates based on the "consent_sonaID" column - this is actually their student number
  group <- as.data.table(q2on)
  q2on = group[group[, .I[which.max(Progress)], by=consent_sonaID]$V1]
  
  # add Participant ID
  q2on$Student.Number = as.numeric(q2on$consent_sonaID)
  q2on = merge(q2on, cohort2[c('ParticipantIdentifier', 'Student.Number')], by = 'Student.Number', all.x = TRUE) # left join
  
  # offboarding
  qualtrics_2off = as.data.table(read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_2/qualtrics/run2_qualtrics_offboarding.csv'))
  qualtrics_2off = qualtrics_2off[Status=='0'] # select non-preview, non-metadata rows
  qualtrics_2off <- qualtrics_2off[consent_sonaID %in% cohort2$Student.Number]
  
  # Name
  qualtrics_2off$onOff = 'offBoarding'
  
  # Find duplicates based on the "consent_sonaID" column - this is actually their student number
  group <- as.data.table(qualtrics_2off)
  qualtrics_2off = group[group[, .I[which.max(Progress)], by=consent_sonaID]$V1]
  
  # add Participant ID
  qualtrics_2off$Student.Number = as.numeric(qualtrics_2off$consent_sonaID)
  qualtrics_2off = merge(qualtrics_2off, cohort2[c('ParticipantIdentifier', 'Student.Number')], by = 'Student.Number', all.x = TRUE) # left join
  
  # combine on and offboarding
  q2 = bind_rows(q2on, qualtrics_2off)
  
  #//////////////////////////#
  ###### Rename Domains ######
  #//////////////////////////#
  
  # number of domains
  domain_num = q1 %>% select(contains('ib_domain_goal')) %>% as_tibble() %>% ncol()
  
  q1 = rename_domains(q1, domain_num)
  q2 = rename_domains(q2, domain_num)
  
  # combine and add column with cohort info
  df = bind_rows("cohort 1" = q1, "cohort 2" = q2, .id = "cohort")
  

#/////////////////////////#
###### Calculate BMI ######
#/////////////////////////#

# Calculate height in CM
df$height = as.numeric(df$height.1_1) * 30.48 + as.numeric(df$height.2_1) * 2.54
# Calculate bmi
df$bmi = (as.numeric(df$weight)/2.2) / (df$height/100)^2


#//////////////////////////////////////////#
#### Average Gap Calculate (Unweighted) ####
#//////////////////////////////////////////#

cols <- colnames(select(df, contains('ib_domain_success')))

# Convert selected columns to numeric
df <- df %>% mutate_at(cols, as.numeric)

# get average
domain_success = df %>%
  select(contains('ib_domain_success')) %>%
  data.frame() %>%
  rowMeans(na.rm = TRUE)

# gap is opposite of success
domain_gap = 100 - domain_success 

# add to measure df
df$domain_gap = domain_gap

#//////////////#
#### GRADES ####
#//////////////#

## Goal grade
grades_q <- select(df[df$onOff=='onBoarding',], contains("grade_goal_3")) %>% as_tibble() %>% mutate_all(as.numeric) %>% data.table()
df
grade_goal = grades_q %>%
  as_tibble() %>%
  mutate(goal = rowMeans(.[,c(2:dim(grades_q)[2])], na.rm = T)) %>%
  select(goal)

df$gradeGoalAvg = NA
df[df$onOff=='onBoarding',]$gradeGoalAvg = pull(grade_goal) 

## Predicted grade
grades_q <- select(df[df$onOff=='onBoarding',], contains("grade_predict_3")) %>% as_tibble() %>% mutate_all(as.numeric) %>% data.table()

grade_predict = grades_q %>%
  as_tibble() %>%
  mutate(goal = rowMeans(.[,c(2:dim(grades_q)[2])], na.rm = T)) %>%
  select(goal)

df$gradePredictAvg = NA
df[df$onOff=='onBoarding',]$gradePredictAvg = pull(grade_predict)

## Actual grade
# pull from `01a_grades.R` script
grades = read_csv('data/grades_all.csv')

# merge
df = merge(df, grades[c('grades_avg', 'ParticipantIdentifier', 'cohort')], by = c('ParticipantIdentifier'), all = TRUE) # outer join
# clean columns
df$cohort.x = NULL # delete
names(df)[names(df) == "cohort.y"] <- "cohort"

#///////////////////////////#
#### VALIDATION MEASURES ####
#///////////////////////////#

### Qualtrics ----

## Conscientiousness 
# Hexaco 60 (ignoring subscales)
df$con_hex_score = calc_survey_score(df, startswith = TRUE, finder = 'con_hex', likert = 5, reverse = 'r')

## Flourishing
df$flourishing_score = calc_survey_score(df, startswith = TRUE, finder = 'flourishing', likert = 7, reverse = '')

## Food Screener (Rapid)
# food fruit
df$food_fruitveg_score = calc_survey_score(df, startswith = TRUE, finder = 'food_fruitveg', likert = 6, reverse = '')

# food fat
df$food_fat_score = calc_survey_score(df, startswith = TRUE, finder = 'food_fat', likert = 5, reverse = '')

## Harmony of Life
df$harmony_score = calc_survey_score(df, startswith = TRUE, finder = 'harmony', likert = 7, reverse = '')

## Quality of Life
df$qol_score = calc_survey_score(df, startswith = TRUE, finder = 'qol', likert = 7, reverse = '')

## Satisfaction with Life
df$sat_life_score = calc_survey_score(df, startswith = TRUE, finder = 'swl_scale', likert = 7, reverse = '')

## Social Desirability
# note that this is code True = 1, False = 2
# but the questions are written in such a way that higher scores still map to higher social desirability tendencies
df$social_des_score = calc_survey_score(df, startswith = TRUE, finder = 'social_des', likert = 2, reverse = 'r')

## Subjective Happiness
df$sub_happy_score = calc_survey_score(df, startswith = TRUE, finder = 'happiness', likert = 7, reverse = 'r')

##########################-
### Experiment Factory ----
##########################-

## Cohort 1 Onboarding
bat_c1_on = read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_1/battery_cohort1_offB.csv')
bat_c1_on$onOff = 'onBoarding'
bat_c1_on$cohort = 'cohort 1'
# fix errors
bat_c1_on$propensity_to_plan_tlr = as.numeric(bat_c1_on$propensity_to_plan_tlr, errors = "ignore")

## Cohort 1 Offboarding
bat_c1_off = read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_1/battery_cohort1_onB.csv')
bat_c1_off$onOff = 'offBoarding'
bat_c1_off$cohort = 'cohort 1'
# fix errors
bat_c1_off$berlin_numeracy_score = as.numeric(bat_c1_off$berlin_numeracy_score, errors = "ignore")
bat_c1_off$propensity_to_plan_tlr = as.numeric(bat_c1_off$propensity_to_plan_tlr, errors = "ignore")

## Cohort 2 Onboarding
bat_c2_on = read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_2/battery_cohort2_onB.csv')
bat_c2_on$onOff = 'onBoarding'
bat_c2_on$cohort = 'cohort 2'
# fix errors
bat_c2_on$berlin_numeracy_score = as.numeric(bat_c2_on$berlin_numeracy_score, errors = "ignore")

## Cohort 2 Offboarding
bat_c2_off = read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_2/battery_cohort2_offB.csv')
bat_c2_off$onOff = 'offBoarding'
bat_c2_off$cohort = 'cohort 2'
# fix errors
bat_c2_off$propensity_to_plan_tlr = as.numeric(bat_c2_off$propensity_to_plan_tlr, errors = "ignore")

# merge all
battery = bind_rows(list(bat_c1_on, bat_c1_off, bat_c2_on, bat_c2_off))

## Combine with main DF
# need to rename student number...also should probably be using Participant ID
colnames(df)[colnames(df) == "Student.Number"] <- "student_id"
battery = subset(battery, select = -c(student_id))
# outer join
df = merge(x = df, y = battery, by = c('cohort', 'onOff', 'ParticipantIdentifier'), all = TRUE)

# Combine split 'cohort' columns
df <- df %>%
  mutate(cohort = coalesce(cohort.x, cohort.y)) %>%
  select(-cohort.x, -cohort.y)

#////////////////////#
#### CORRELATIONS ####
#////////////////////#

cor_cols = c('con_hex_score',
             'flourishing_score',
             'food_fruitveg_score',
             'food_fat_score',
             'harmony_score',
             'qol_score',
             'sat_life_score',
             'social_des_score',
             'sub_happy_score',
             
             'ambition_score',
             'brief_self_control_score',
             'bsss_overall',
             'DASS_overall',
             'DASS_depression',
             'DASS_anxiety',
             'DASS_stress',
             # 'effort_avoidance_score',
             # 'emotion_regulation_score',
             'future_time_perspective_score',
             # 'general_self_efficacy_score',
             'grit_scale_score',
             # 'k6_survey_score',
             'need_for_cognition_score',
             'perceived_stress_score',
             'rosenberg_SES_score',
             'trait_hedonic_capacity_score',
             
             'domain_gap',
             'ParticipantIdentifier'
             )

# predicting gap
cor_cols_predict_gap = c('con_hex_score',
             'food_fruitveg_score',
             'food_fat_score',
             'social_des_score',
             'ambition_score',
             'brief_self_control_score',
             'bsss_overall',
             'future_time_perspective_score',
             'grit_scale_score',
             'need_for_cognition_score',
             'trait_hedonic_capacity_score',
             
             'domain_gap'
)

cor_cols_gap_predicts = c(
                         'flourishing_score',
                         'harmony_score',
                         'qol_score',
                         'sat_life_score',
                         'sub_happy_score',
                         'DASS_overall',
                         'DASS_depression',
                         'DASS_anxiety',
                         'DASS_stress',
                         'perceived_stress_score',
                         'rosenberg_SES_score',

                         'domain_gap'
)


library(corrr)

res.cor = correlate(df[cor_cols])

res.cor %>%
  focus(domain_gap) %>%
  mutate(rowname = reorder(term, domain_gap)) %>%
  ggplot(aes(x = rowname, y = domain_gap, fill = domain_gap)) +
  geom_col() + coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1), guide = "colorbar") +
  geom_text(aes(label = round(domain_gap, 2)), vjust = 0.5, hjust = -0.1, color = "black") +
  theme_minimal()

res.cor %>% network_plot(min_cor = .2)

library("PerformanceAnalytics")
chart.Correlation(df[cor_cols_predict_gap], histogram=TRUE, pch=19)
chart.Correlation(df[cor_cols_gap_predicts], histogram=TRUE, pch=19)

# take mean of on and offboarding
grouped_data <- df[cor_cols] %>%
  group_by(ParticipantIdentifier) %>%
  summarize(across(everything(), mean))

# calculate correlations
res.cor = correlate(grouped_data)
# remove extraneous part of string
res.cor$term = sub("_[^_]*$", "", res.cor$term)
# plot
res.cor %>%
  focus(domain_gap) %>%
  mutate(rowname = reorder(term, domain_gap)) %>%
  ggplot(aes(rowname, domain_gap)) +
  geom_col() + coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1), guide = "colorbar") +
  theme_minimal()


#### CONVERT TO NUMERIC #####
df$age = as.numeric(df$age)


#///////////////////#
#### Save Out DF ####
#///////////////////#

x = df
saveRDS(x, file = "data/df.RDS") 
# save to measure processed data
measure_path = '/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_1_gap_measure/3_1_2_processed_data/'
write.csv(x, file = paste0(measure_path, "all_subject_measure_data.csv"), row.names = FALSE)

#### VALIDATION WITH DAILY MEAN ####
run1_gap = as.data.table(read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_1/run1_selfReport.csv'))
run1_gap = run1_gap %>%
  group_by(ParticipantIdentifier) %>%
  summarise(mean_gap = mean(DAILY_past24_gap))

run2_gap = as.data.table(read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_2/run2_selfReport.csv'))
run2_gap = run2_gap %>%
  group_by(ParticipantIdentifier) %>%
  summarise(mean_gap = mean(DAILY_past24_gap))

run_gaps = bind_rows(run1_gap, run2_gap)

cohorts = bind_rows(cohort1, cohort2)
cohorts$Student.Number = as.character(cohorts$Student.Number)
cor_df = merge(cor_df, cohorts, by= 'Student.Number')

cor_df = merge(cor_df, run_gaps, by = 'ParticipantIdentifier')


#### PREDICTION OF HELD OUT DOMAIN ####


#/////////////////#
#### FUNCTIONS ####
#/////////////////#

# rename function
rename_domains = function(df, domain_num){
  # list of domain categories
  # removed effort
  ib_domain_cats = c('ambition', 'external',
                     'goal', 'import', 'internal',
                     'rank', 'satis', 'source', 'specific',
                     'success', 'time'
  )
  
  domain_names = c("Diet",
                   "Exercise",
                   "MentalPersonal_Health",
                   "Medical_Health",
                   "Sleep",
                   "Alcohol_drug",
                   "Online",
                   "Phone",
                   "Video games",
                   "Reading_leisure",
                   "SocialMedia",
                   "Sports_playing",
                   "TV_Streaming",
                   "Family",
                   "Friends",
                   "Partner",
                   "Social_life",
                   "Hobby",
                   "Housework",
                   "Cooking",
                   "Work_School",
                   "Environment",
                   "Culture",
                   "Learning",
                   "Self-Improvement",
                   "Volunteering",
                   "Community involvement",
                   "Admin",
                   "Future_Planning",
                   "Finances",
                   "Time_Management",
                   "Punctuality",
                   "Personal_Values",
                   "Other"
  )
  
  for (i in 1:length(ib_domain_cats)) {
    # make a list of the existing names for each cateogry
    old_names = df %>%
      select(starts_with('ib_domain')) %>%
      select(contains(ib_domain_cats[i], ignore.case = FALSE)) %>%
      as_tibble() %>%
      names()
    old_names = old_names[1:34] # added this becuase there was some doubling of columns
    
    # make a list of names with category and domain
    new_names = rep('ib_domain_', domain_num)
    new_names = paste0(new_names,ib_domain_cats[i], '_', domain_names)
    
    # replace existing names with new names
    df = df %>%
      rename_at(vars(all_of(old_names)), ~ new_names) %>%
      as_tibble()
  }
  
  return(df)
}

# survey score function
calc_survey_score <- function(data, startswith, finder, likert, reverse = '') {
  cols <- NULL
  if (startswith == TRUE) {
    cols <- grep(paste0("^", finder), names(data), value = TRUE)
  } else {
    cols <- grep(paste0(finder, "$"), names(data), value = TRUE)
  }
  
  temp <- data[, cols]
  temp <- temp %>% mutate_all(as.numeric)
  
  # Control for reverse scoring
  if (reverse != '') {
    cols_r <- grep(reverse, names(temp), value = TRUE)
    temp[cols_r] <- likert + 1 - temp[cols_r]
  }
  
  measure_score <- rowMeans(temp, na.rm = TRUE)
  
  return(measure_score)
}