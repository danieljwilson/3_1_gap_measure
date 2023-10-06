###### 0. Import Libraries ######
library(data.table)
library(tidyverse)
library(readxl)

#////////////////////#
#### 1. Load Data ####
#////////////////////#

## 1.1 Import participants from cohort 1 and 2 ----
cohort1 = read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_1/run1_subjects.csv')
cohort2 = read.csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_2/run2_subjects.csv')

## 1.2 Import 

#//////////////#
#### GRADES ####
#//////////////#

## Actual Grades
# Note that I am taking the average of all grades. We could also look at grades in specific classes.

# 1st cohort
grades1 = read_csv('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_2_processed_data/run_1/run1_grades.csv')
grades1 = grades1[c('ParticipantIdentifier', 'grades_avg')]
# add student number
grades1 = merge(grades1, cohort1, by='ParticipantIdentifier')
grades1 = grades1[c('Student.Number', 'grades_avg', 'ParticipantIdentifier')]
# Drop rows with NA in the 'Student.Number' column
grades1 <- grades1[complete.cases(grades1$Student.Number), ]
# Label cohort
grades1$cohort = 'cohort 1'
# convert grade avg
grades1$grades_avg = grades1$grades_avg * 100

# 2nd cohort
grades2 = read_excel('/Users/djw/Documents/pCloud_synced/Academics/Projects/2020_thesis/thesis_experiments/3_experiments/3_3_experience_sampling/3_3_1_raw_data/run_2/grades/ACADEMIC_ACTIVITIES_230509.xlsx')
# convert to wide
grades2 = dcast(as.data.table(grades2), PERSON_ID ~ ACAD_ACT_CD, value.var="ENTERED_MARK")
# Calculate row average, ignoring NAs and omitting first column
# convert 'CR' to NA
grades2 = replace(grades2, grades2 == 'CR', NA)
# convert df to numeric
grades2 <- as.data.frame(lapply(grades2, as.numeric))
grades2$grades_avg <- rowMeans(grades2[, -1], na.rm = TRUE)
grades2$Student.Number = grades2$PERSON_ID
grades2 = grades2[c('Student.Number', 'grades_avg')]
# Add PID
grades2 = merge(grades2, cohort2[c('Student.Number', 'ParticipantIdentifier')], by='Student.Number')
# Drop rows with NA in the 'Student.Number' column
grades2 <- grades2[complete.cases(grades2$Student.Number), ]
# Label cohort
grades2$cohort = 'cohort 2'

## Add PSYA01/A02 Grades
# this would consist of additional columns indicating the date, grade, and course percentage of exams/assignments

# Join both cohorts
grades = bind_rows(grades1, grades2)
# remove any duplicate rows (shouldn't be any)
grades <- grades[!duplicated(grades), ]

## Save file
write.csv(grades, file = 'data/grades_all.csv', row.names = FALSE)