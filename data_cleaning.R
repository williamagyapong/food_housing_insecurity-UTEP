
# Load required packages
library(dplyr)

# Importing the original survey data
dir_path <- dirname(rstudioapi::getSourceEditorContext()$path)
FIHI <- read.csv(paste0(dir_path, .Platform$file.sep, "datcsv.csv"))


#=========================================================
# User-defined Helper functions
#========================================================

na_to_zero <- function(x) 
{
  return(ifelse(is.na(x), 0, x ))
}

na_to_99 <- function(x) 
{
  return(ifelse(is.na(x), 99, x ))
}

# This function helps restore back any prior missingness. Care must be taken in 
# order not to apply it to a variable where 0 has a different meaning.
zero_to_na <- function(x) 
{
  return(ifelse(x == 0, NA, x ))
}


#------------------ Data Cleaning/Preprocessing -------------------

FIHI_new <- FIHI %>% 
  # change column names to lowercase
  rename_with(tolower) %>%
  # Removing unwanted variables
  dplyr::select(-externalid, -satellite, -email, -datetime, -carddata,
                -starts_with(c("q5", "q8", "q18", "q21", "q29", "q24", 
                               "q36", "q37", "q38", "q39")
                             )
         ) %>%
  # Strip off trailing strings to get clean variable names
  rename_with(~gsub('\\..*', '', .x))  


##------- Consolidating common variables split across levels -----
 # We create dummy columns for the number of non-na values within each range for the targeted variable
FIHI_new_ <- FIHI_new %>% 
  mutate(q6Count = rowSums(!is.na(select(., starts_with("q6")))),
         q7Count = rowSums(!is.na(select(., starts_with("q7")))),
         q11Count = rowSums(!is.na(select(., starts_with("q11")))),
         q25Count = rowSums(!is.na(select(., starts_with("q25"))))) %>%
  # change the 0 level for q25_7 to avoid issues with approach for working on these multiple-response variables
  mutate(q25_7 = ifelse(q25_7 == 0, 7, q25_7))

## View these variables to make sure the count was done right
# FIHI_new_%>%select(respondentid,starts_with("q6")) %>%kable()
# FIHI_new_%>%select(respondentid,starts_with("q7")) %>%kable()
# FIHI_new_%>%select(respondentid,starts_with("q11")) %>%kable()
# FIHI_new_%>%select(respondentid,starts_with("q25")) %>%kable()


##-------------- Separating multiple responses ---------------------------------
# Ethnicity: 
#    - level 8 is for mixed race
#    - set Hispanic and White/Caucassian to Hispanic [q6_1!=0 & q6_6!=0]
#    - set Hispanic and Black to Black [q6_1!=0 & q6_4!=0]
#
# Gender:
#    - level 7: Mixed gender
#
# Federal Aid (q25): 
#    - Not applicable -> 7
#    - Multiple sources -> 8
#    - work study is almost lost when multiple responses are sorted out, 
#    so we decided to do: work study + other sources => work study
#    - 
#
FIHI_sub <- FIHI_new_ %>% 
  mutate(across(starts_with(c("q6", "q7", "q11", "q25")), na_to_zero)) %>%
  mutate(q6 = ifelse(q6Count>1, ifelse(q6_1!=0 & q6_6!=0, 1,
                                       ifelse(q6_1!=0 & q6_4!=0, 4, 8)
                                       ),
                     rowSums(across(starts_with("q6_")))), 
  
         q7 = ifelse(q7Count>1, 7, rowSums(across(starts_with("q7_")))),
         
         q11 = ifelse(q11Count>1,10, rowSums(across(starts_with("q11_")))),
         
         q25 = ifelse(q25Count>1, ifelse(q25_2!=0, 2, 8), 
                      rowSums(across(starts_with("q25_")))),
         
         .after = starts_with("q4"),
         .keep = "unused") %>%
  relocate(q11, .after = starts_with("q10")) %>% # just to keep the order of the variables
  relocate(q25, .after = starts_with("q23")) %>%
  mutate(across(starts_with(c("q6", "q7", "q11", "q25")), zero_to_na))

##---------- Removing respondents with 0 or extremely few responses ---------

# Found 7087 zero responses, 160 two responses, and 114 four responses
# We decided to get rid of these
FIHI_sub <- FIHI_sub %>%
  mutate(count = rowSums(!is.na(select(., -respondentid)))) %>%
  filter(count > 4) %>%
  select(-count)

##---------- Collapsing levels within some of the variables ---------
#
# Ethnicity(q6): maintain Hispanic (1), Black/African American(4), White(6), Asian(3)
# and collapse American Indian(2), Native Hawaiian(5), mixed race(8) and other(7) into other (8).
# We decided not to keep a mixed race level because the total came to 88 after we 
# assigned (Hispanic, white) to Hispanic, and (Hispanic, Black) to Black.
#
# Gender(q7) now will have only 3 levels (female -> 1, male -> 2, non-binary/non-conforming -> 3).
#
# Income(q9): keep first 5 levels and 100000 or more (11)->8, collapse (6,7)->6, (8,9,10)->7
# 
# Academic level (q10): keep first 5, and collapse last 2 -> 6 
#
# College (q11): maintain first 6, put rest into other (level 7)
#
# Commute (q12): keep car(1), Bus/public (4) and other(8), collapse car & carpool (2,3) -> 9,
# collapse bike, trolley, walk (5,6,7) -> 10. Keep Not applicable (0)
#
# Combine Q15 and Q16 into a single variable, number of dependents.
# No for Q15 becomes 0 with a level of 4, while levels of Q16 are maintained 
# 
# Federal Aid(q25): Emergency loan reduced from 316 to 50, so we added those to loans(3)
#
# Q26: Never -> 1, sometimes true -> 2, often true -> 3
##

FIHI_sub2 <- FIHI_sub %>%
  mutate(q6 = ifelse(q6%in%c(2,5,7,8), 8, q6),
         q7 = ifelse(q7 >=4, 3, q7),
         q9 = ifelse(q9%in%c(6,7), 12, ifelse(q9%in%c(8,9,10), 13, q9)),
         q10 = ifelse(q10>5, 6, q10),
         # q11 = ifelse(q11>6, 7, q11),
         q12 = ifelse(q12%in%2:3, 9, ifelse(q12%in%5:7, 10, ifelse(q12==0, 11, q12))),
         q15 = ifelse(q15==2, 4, q16),
         q25 = ifelse(q25==4, 3, q25)
         ) %>% 
  select(-q16) # remove this variable since it has already been used up



##------------------ Assign labels to the various levels for EDA -----------------------

FIHI_sub3 <- FIHI_sub2 %>%
  mutate(
    q1 = factor(q1, levels = 1:2, 
                      labels = c("Full Time", "Part Time")),
    q2 = factor(q2, levels = 1:3, 
                      labels = c("Full Time", "Part Time", "No")),
    q3 = factor(q3, levels = 1:2, 
                      labels = c("On-Campus", "Off-Campus")),
    q4 = factor(q4, levels = 1:2, 
                      labels = c("19 hours or less", "More than 19 hours")),
    q6 = factor(q6, levels = c(1,3,4,6,8), 
                      labels = c("Hispanic", "Asian", "Black/African American", "White/Caucasian",
                                 "Other")),
    q7 = factor(q7, levels = 1:3, 
                      labels = c("Female", "Male", "Non-binary/Non-conforming")),
    q9 = factor(q9, levels = c(1:5,12,13,11), 
                      labels = c("< 10,000", "10,000-19,999", "20,000-29,999", "30,000-39,000",
                                 "40,000-49,999", "50,000-69,999", "70,000-99,999", ">=100,000")),
    q10 = factor(q10, levels = 1:6, 
                      labels = c("Freshman", "Sophomore", "Junior", "Senior",
                                 "Masters", "Doctoral/Professional")),
    q11 = factor(q11, levels = 1:10, 
                      labels = c("Business", "Education", "Engineering", "Health Sciences",
                                 "Liberal Arts", "Science", "Nursing", "Pharmacy",
                                 "Other", "More than one")),
    q12 = factor(q12, levels = c(1,9,4,10,8,11), 
                      labels = c("Car (alone)", "Car (someone drives) & Carpool",
                                 "Bus/Public transport", "Bike, Trolley, or Walk",
                                 "Other", "Not applicable")),
    q13 = factor(q13, levels = 1:4, 
                      labels = c("Not reliable at all", "Somewhat reliable",
                                 "Fairly reliable", "Very reliable")),
    q14 = factor(q14, levels = 1:2, 
                      labels = c("Yes", "No")),
    q15 = factor(q15, levels = c(4, 1:3), 
                      labels = c("0", "1", "2 - 3", "4 or more")),
    q17 = factor(q17, levels = 1:2, 
                      labels = c("Yes", "No")),
    q19 = factor(q19, levels = 1:4, 
                      labels = c("On-Campus", "Off-Campus with family", 
                                 "Off-Campus not with family", "Other")),
    q20 = factor(q20, levels = 1:2, 
                      labels = c("Yes", "No")),
    q22 = factor(q22, levels = 1:3, 
                      labels = c("Rarely", "Sometimes", "Often")),
    q23 = factor(q23, levels = 1:2, 
                      labels = c("Yes", "No")),
    q25 = factor(q25, levels = c(1:3,5,8,7,6 ), 
                      labels = c("Grants", "Work Study", "Loans", "Scholarship", 
                                 "Multiple Aids", "Other", "None")),
    q26 = factor(q26, levels = 1:3, 
                      labels = c("Never true", "Sometimes true", "Often true")),
    q27 = factor(q27, levels = 1:3, 
                      labels = c("Never true", "Sometimes true", "Often true")),
    q28 = factor(q28, levels = 1:2, 
                      labels = c("Yes", "No")),
    q30 = factor(q30, levels = 1:2, 
                      labels = c("Yes", "No")),
    q31 = factor(q31, levels = 1:2, 
                      labels = c("Yes", "No")),
    q32 = factor(q32, levels = 1:3, 
                      labels = c("Decreased", "About the same", "Increased")),
    q33 = factor(q33, levels = 1:3, 
                      labels = c("Decreased", "About the same", "Increased")),
    q34 = factor(q34, levels = 1:3, 
                      labels = c("Decreased", "About the same", "Increased")),
    q35 = factor(q35, levels = 1:3, 
                      labels = c("Decreased", "About the same", "Increased")),
    )

##------------- Cleaning data further for modeling  ---------------------
# Make distorted levels after collapsing consecutive.
FIHI_sub2 <- FIHI_sub2 %>%
  mutate(q6 = factor(q6, levels = c(1,3,4,6,8),  labels = 1:5),
         q9 = factor(q9, levels = c(1:5,12,13,11), labels = 1:8),
         q12 = factor(q12, levels = c(1,9,4,10,8,11),  labels = 1:6),
         q15 = factor(q15, levels = c(4, 1:3), labels = 1:4),
         q25 = factor(q25, levels = c(1:3,5,8,7,6 ), labels = 1:7)
  )

# For modeling purposes, we recoded response variables with 3 class levels into 2 levels 
FIHI_sub2 <- FIHI_sub2 %>%
  mutate(q22 = ifelse(q22 !=1, 1, 2),
         q26 = ifelse(q26 !=1, 1, 2),
         q27 = ifelse(q27 !=1, 1, 2)
         ) %>%
  mutate(q20 = factor(q20, levels = 1:2, labels = c("Yes", "No")),
         q22 = factor(q22, levels = 1:2, labels = c("Yes", "No")),
         q26= factor(q26, levels = 1:2, labels = c("Yes", "No")),
         q27= factor(q27, levels = 1:2, labels = c("Yes", "No")),
         q28= factor(q28, levels = 1:2, labels = c("Yes", "No")),
         q30= factor(q30, levels = 1:2, labels = c("Yes", "No")),
         q31= factor(q31, levels = 1:2, labels = c("Yes", "No"))
          )

##------------- Rename the remaining variables ---------------------
var_names <- c("respondent_id", "enrollment", "employment", "employment_type",
               "weekly_work_hrs", "ethnicity", "gender", "total_income",
               "academic_level", "college/school", "mode_transport", 
               "transport_reliability", "living_alone", "dependents", 
               "household_head", "residence", "permanent_address", 
               "spent_night_elsewhere", "know_homeless_student", 
               "federal_student_aid", "FI_q26", "FI_q27", "FI_q28", "FI_q30", 
               "FI_q31", "expenditures_changed", "income_changed", 
               "fed_aid_changed", "debt_changed")

names(FIHI_sub2) <- var_names
names(FIHI_sub3) <- var_names


# Save preprocessed data 
# write.csv(FIHI_sub3, paste0(file_path, '/datcsv_clean.csv'))

save(FIHI_sub2, FIHI_sub3, 
     file=paste0(file_path, .Platform$file.sep, 'FIHI_clean.RData')
     )


# clean up the environment and console
rm(list = ls())
cat("\014") # ctrl+L

# Clear loaded package


#------------- END OF FILE ----------------------------------------




