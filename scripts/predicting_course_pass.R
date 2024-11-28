library(readr)
library(dplyr)
library(tidyr)
library(tibble)
getwd()
setwd("/Users/kevinmcgowan/projects/Predicting_HS_Performance/data")
data<- read_csv("DATA (1).csv")
#see top 5 rows
view(data)
head(data)
colnames(data)
#above shows all columns have been coded as numeric values, as have the column names. 

################################################################################
# too make the data more interpretable, the column names will be inserted, and a table of definition
# will be printed for reference:

# step 1: Create table of variables and definitions
  # Variable table pasted from data source (https://archive.ics.uci.edu/dataset/856/higher+education+students+performance+evaluation) 
  # and converted to a strings by adding quotes and commas to separate index from variable name and definition strings

variable_table <- tibble::tribble(
  ~Index, ~Variable, ~Definition,
  "STUDENT ID", "StudentID", "Unique identifier for each student",
  "1", "Student Age", "(1: 18-21, 2: 22-25, 3: above 26)",
  "2", "Sex", "(1: female, 2: male)",
  "3", "Graduated high-school type", "(1: private, 2: state, 3: other)",
  "4", "Scholarship type", "(1: None, 2: 25%, 3: 50%, 4: 75%, 5: Full)",
  "5", "Additional work", "(1: Yes, 2: No)",
  "6", "Regular artistic or sports activity", "(1: Yes, 2: No)",
  "7", "Do you have a partner", "(1: Yes, 2: No)",
  "8", "Total salary if available", "(1: USD 135-200, 2: USD 201-270, 3: USD 271-340, 4: USD 341-410, 5: above 410)",
  "9", "Transportation to the university", "(1: Bus, 2: Private car/taxi, 3: bicycle, 4: Other)",
  "10", "Accommodation type in Cyprus", "(1: rental, 2: dormitory, 3: with family, 4: Other)",
  "11", "Mothers’ education", "(1: primary school, 2: secondary school, 3: high school, 4: university, 5: MSc., 6: Ph.D.)",
  "12", "Fathers’ education", "(1: primary school, 2: secondary school, 3: high school, 4: university, 5: MSc., 6: Ph.D.)",
  "13", "Number of sisters/brothers", "(1: 1, 2: 2, 3: 3, 4: 4, 5: 5 or above)",
  "14", "Parental status", "(1: married, 2: divorced, 3: died - one of them or both)",
  "15", "Mothers’ occupation", "(1: retired, 2: housewife, 3: government officer, 4: private sector employee, 5: self-employment, 6: other)",
  "16", "Fathers’ occupation", "(1: retired, 2: government officer, 3: private sector employee, 4: self-employment, 5: other)",
  "17", "Weekly study hours", "(1: None, 2: <5 hours, 3: 6-10 hours, 4: 11-20 hours, 5: more than 20 hours)",
  "18", "Reading frequency (non-scientific books/journals)", "(1: None, 2: Sometimes, 3: Often)",
  "19", "Reading frequency (scientific books/journals)", "(1: None, 2: Sometimes, 3: Often)",
  "20", "Attendance to the seminars/conferences related to the department", "(1: Yes, 2: No)",
  "21", "Impact of your projects/activities on your success", "(1: positive, 2: negative, 3: neutral)",
  "22", "Attendance to classes", "(1: always, 2: sometimes, 3: never)",
  "23", "Preparation to midterm exams 1", "(1: alone, 2: with friends, 3: not applicable)",
  "24", "Preparation to midterm exams 2", "(1: closest date to the exam, 2: regularly during the semester, 3: never)",
  "25", "Taking notes in classes", "(1: never, 2: sometimes, 3: always)",
  "26", "Listening in classes", "(1: never, 2: sometimes, 3: always)",
  "27", "Discussion improves my interest and success in the course", "(1: never, 2: sometimes, 3: always)",
  "28", "Flip-classroom", "(1: not useful, 2: useful, 3: not applicable)",
  "29", "Cumulative grade point average in the last semester (/4.00)", "(1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: above 3.49)",
  "30", "Expected Cumulative grade point average in the graduation (/4.00)", "(1: <2.00, 2: 2.00-2.49, 3: 2.50-2.99, 4: 3.00-3.49, 5: above 3.49)",
  "COURSE ID", "CourseID", "Unique identifier for the course",
  "GRADE", "FinalGrade", "0: Fail, 1: DD, 2: DC, 3: CC, 4: CB, 5: BB, 6: BA, 7: AA"
)

# Step 2: Create a Named Vector for Renaming 
rename_vector <- setNames(variable_table$Variable, variable_table$Index)

# Step 3: Rename Columns in "data"
data_clean <- data  
if (all(colnames(data) %in% names(rename_vector))) {
  colnames(data_clean) <- rename_vector[as.character(colnames(data))]
} else {
  stop("Mismatch between dataset column names and variable table indices.")
}

# Step 4: Validate the Renaming
# Compare `data` and `data_clean` for equality of values
comparison_result <- all.equal(data, data_clean)
if (isTRUE(comparison_result)) {
  cat("Data and DataClean are identical in values.\n")
} else {
  cat("Differences found in values between data and data_clean:\n")
  print(comparison_result)
}
#there are 33 mismatches, which is expected since the code above just renamed 33 variables.

# Verify column names for potential mismatch
cat("\nColumn names in `data` but not in `data_clean`:\n")
print(setdiff(colnames(data), colnames(data_clean)))
# Code above confirms all 33 mismatch are column names.

cat("\nColumn names in `data_clean` but not in `data`:\n")
print(setdiff(colnames(data_clean), colnames(data)))
#above shows both tables have the same number of columns and indices are aligned

# Step 5: Value Matching
# Compare row-by-row values between the datasets
value_comparison <- data.frame(
  Index = 1:ncol(data),
  Original_Column = colnames(data),
  Cleaned_Column = colnames(data_clean),
  Match = sapply(1:ncol(data), function(i) all(data[[i]] == data_clean[[i]]))
)

print(value_comparison)

#Value comparison shows a TRUE match for every row, proving a successful change in column names 
#while preserving row values
################################################################################
# Inspect current data set
print(n=33,data_clean)
print(variable_table$Definition)

# Remove "STUDENT" from every value in the StudentID column
data_clean <- data_clean %>%
  mutate(StudentID = gsub("STUDENT", "", StudentID))

# View the updated data_clean
head(data_clean)

# The column names are still very long and contain spaces that will challenge later analysis.
colnames(data_clean)
# rename columns

# Rename columns with your custom names
data_clean <- data_clean %>%
  rename(
    ID = 'StudentID',
    Age = `Student Age`,
    Sex = Sex,
    HS_Type = `Graduated high-school type`,
    Scholarship = `Scholarship type`,
    Work = `Additional work`,
    Artistic = `Regular artistic or sports activity`,
    Partner_Status = `Do you have a partner`,
    Salary = `Total salary if available`,
    Transportation = `Transportation to the university`,
    Accommodation = `Accommodation type in Cyprus`,
    Mothers_Ed = `Mothers’ education`,
    Fathers_Ed = `Fathers’ education`,
    Siblings = `Number of sisters/brothers`,
    Parental_Status = `Parental status`,
    Mothers_Occ = `Mothers’ occupation`,
    Fathers_Occ = `Fathers’ occupation`,
    Study_Hours = `Weekly study hours`,
    NonScientific_Reading = `Reading frequency (non-scientific books/journals)`,
    Scientific_Reading = `Reading frequency (scientific books/journals)`,
    Seminars = `Attendance to the seminars/conferences related to the department`,
    Project_Impact = `Impact of your projects/activities on your success`,
    Attendance = `Attendance to classes`,
    Midterm1_Prep = `Preparation to midterm exams 1`,
    Midterm2_Prep = `Preparation to midterm exams 2`,
    Note_Taking = `Taking notes in classes`,
    Class_Listening = `Listening in classes`,
    Discussion_Impact = `Discussion improves my interest and success in the course`,
    FlipClassroom = `Flip-classroom`,
    Last_GPA = `Cumulative grade point average in the last semester (/4.00)`,
    Expected_Grad_CGPA = `Expected Cumulative grade point average in the graduation (/4.00)`,
    CourseID = CourseID,
    FinalGrade = FinalGrade
  )
colnames(data_clean)
################################################################################
## Exploratory data analysis

nrow(data_clean)
#we see we have only 145 students for training and testing. This small dataset posses some challenges 
#like over fitting. Additoinally, our student sample is only engineering students limiting the generalizability of any findings we get.

# Print the different grades students can get
unique(variable_table$Definition[variable_table$Variable == "FinalGrade"])

unique(data_clean$FinalGrade)
#with 8 unique values for final grade and only 145 students, classification will be difficult.
#by coding values as either pass or fail, prediction could be stronger. 


















# Citations
Yilmaz, N. & Şekeroğlu, B. (2019). Higher Education Students Performance Evaluation [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C51G82.