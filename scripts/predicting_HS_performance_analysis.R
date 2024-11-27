# Predicting High School Core Course Performance Analysis

# This R Script uses Paulo Cortez's Student Performance Dataset (2008) to create a machine learning algorithm.
  # Specifically, this report builds and compares two different classification models that predict pass or fail for
  # in Portuguese (equivalent of English class in US) or mathematics. RMSE is used as the primary 
  # measure of accuracy. 


# Requirements:
# - Ensure necessary libraries are installed (see library loading section).
# - The script will download the Student Performance dataset (2008) if not already present. 

#Load libraries:
# Install missing libraries
packages <- c("here", "stringr", "dplyr")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the libraries
lapply(packages, library, character.only = TRUE)


# Load Data
# Define file paths for locally hosted datasets
math_file <- here("data", "student-mat.csv")
portuguese_file <- here("data", "student-por.csv")

# Read the datasets
math_data <- read.csv(math_file, sep = ";")
portuguese_data <- read.csv(portuguese_file, sep = ";")

# See datasets to better understand task at hand.
head(math_data)
head(portuguese_data)

#As seen above, both datasets have the same variables, but are corresponded to two different subjects:
  #1. Portuguese
  #2. Mathematics

# Additionally, all variables have been encoded into numeric values. The key to the meaning of these variables 
# is stored in the student text file below:

# Define the file path
student_txt <- here("data", "student.txt")

# Read and process the file
variable_table <- readLines(student_txt) %>%
  # Keep lines that contain attributes (filter out comments and blank lines)
  str_subset("^\\d+\\s") %>%
  # Extract variable names and definitions
  str_match("^(\\d+)\\s+([^\\-]+)\\s+-\\s+(.*)$") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  # Create a clean data frame
  select(-1) %>%
  rename(Index = V2, Variable = V3, Definition = V4)

# Preview the table
print(variable_table)

#this roough table output can be fed to Chat GPT for quick error free tabularization. The output is as follows: 

variables <- c(
  "school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
  "reason", "guardian", "traveltime", "studytime", "failures", "schoolsup", "famsup", "paid",
  "activities", "nursery", "higher", "internet", "romantic", "famrel", "freetime", "goout",
  "Dalc", "Walc", "health", "absences", "G1", "G2", "G3"
)

descriptions <- c(
  "Student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)",
  "Student's sex (binary: 'F' - female or 'M' - male)",
  "Student's age (numeric: from 15 to 22)",
  "Student's home address type (binary: 'U' - urban or 'R' - rural)",
  "Family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)",
  "Parent's cohabitation status (binary: 'T' - living together or 'A' - apart)",
  "Mother's education (numeric: 0 - none, 1 - primary, 2 - 5th to 9th, 3 - secondary, 4 - higher)",
  "Father's education (numeric: 0 - none, 1 - primary, 2 - 5th to 9th, 3 - secondary, 4 - higher)",
  "Mother's job (nominal: 'teacher', 'health', 'services', 'at_home', 'other')",
  "Father's job (nominal: 'teacher', 'health', 'services', 'at_home', 'other')",
  "Reason to choose this school (nominal: 'home', 'reputation', 'course', 'other')",
  "Student's guardian (nominal: 'mother', 'father', 'other')",
  "Home to school travel time (numeric: 1 - <15 min, 2 - 15 to 30 min, 3 - 30 min to 1 hr, 4 - >1 hr)",
  "Weekly study time (numeric: 1 - <2 hrs, 2 - 2 to 5 hrs, 3 - 5 to 10 hrs, 4 - >10 hrs)",
  "Number of past class failures (numeric: n if 1 <= n < 3, else 4)",
  "Extra educational support (binary: yes or no)",
  "Family educational support (binary: yes or no)",
  "Extra paid classes in the course subject (binary: yes or no)",
  "Extra-curricular activities (binary: yes or no)",
  "Attended nursery school (binary: yes or no)",
  "Wants to take higher education (binary: yes or no)",
  "Internet access at home (binary: yes or no)",
  "With a romantic relationship (binary: yes or no)",
  "Quality of family relationships (numeric: 1 - very bad to 5 - excellent)",
  "Free time after school (numeric: 1 - very low to 5 - very high)",
  "Going out with friends (numeric: 1 - very low to 5 - very high)",
  "Workday alcohol consumption (numeric: 1 - very low to 5 - very high)",
  "Weekend alcohol consumption (numeric: 1 - very low to 5 - very high)",
  "Current health status (numeric: 1 - very bad to 5 - very good)",
  "Number of school absences (numeric: from 0 to 93)",
  "First period grade (numeric: from 0 to 20)",
  "Second period grade (numeric: from 0 to 20)",
  "Final grade (numeric: from 0 to 20, output target)"
)

# Combine into a data frame
variable_table <- data.frame(
  Index = seq_along(variables),
  Variable = variables,
  Definition = descriptions
)

# Print the table
print(variable_table)





#balls 

































#will likely need boot strapping for small dataset...






# References 
# Cortez, P. (2008). Student Performance [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C5TG7T.
