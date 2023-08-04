# Install / Load the Libraries
library(readxl)
library(tidyverse)

# Read in the data sets
user <- read_excel("user.xlsx")
loan <- read_excel("loan.xlsx")

# View the data sets
View(user)
View(loan)

# Merge the two data sets
  # merged_data <- user %>% inner_join(y = loan, by = "User_ID")
  # View(merged_data)
merged_data <- user %>% full_join(y = loan, by = 'User_ID')
View(merged_data)

# Clean merged data
#   Remove duplicate rows
merged_data <- merged_data %>% distinct(User_ID, .keep_all = TRUE)

  # Remove duplicate columns
merged_data2 <- select(merged_data, -(Started_Application_Date:Repayment_Date))

  # Arrange the columns 
merged_data2 <- select(merged_data2, User_ID, Loan_ID, everything())
View(merged_data2)

merged_data <- merged_data2
View(merged_data)

# Convert columns to Dates
merged_data$Signup <- as.Date(merged_data$Signup)

merged_data$Started_Application_Time <- as.Date(
  merged_data$Started_Application_Time)

merged_data$Completed_Application_time <- as.Date(
  merged_data$Completed_Application_time)

merged_data$Awaiting_Payment_Time <- as.Date(
  merged_data$Awaiting_Payment_Time)

merged_data$Repayment_Time <- as.Date(merged_data$Repayment_Time)

merged_data$Canceled_Time <- as.Date(merged_data$Canceled_Time)

View(merged_data)

# Metric 1: Application Submission Rate
Signups_merged <- nrow(merged_data[!is.na(merged_data$Signup),])
Signups_merged

Completed_merged <- nrow(merged_data[!is.na(
  merged_data$Completed_Application_time),])
Completed_merged

Completed_merged / Signups_merged # ~23% of signups completed their applications

# This metric clearly provides the initial interest of users that create an 
#   account and complete an application

# Solution: With a low submission rate, marketing strategies need to be changed
# to highlight the pros for potential customers


# Graph of Application Submission Rate
Not_completed <- Signups_merged - Completed_merged
Not_completed

ggplot(data = data.frame(Status = c("Completed", "Not Completed"),
                         Count = c(Completed_merged, Not_completed)),
       aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Application Submmission Rate",
       x = "Status",
       y = "Count") +
  theme_minimal()


# Metric 2: Application Completion Rate
Starting_merged <- nrow(merged_data[!is.na(
  merged_data$Started_Application_Time),])
Starting_merged

Completed_merged / Starting_merged # ~69% of started applications were completed

# This metric can help PerPay identify any problems or bottlenecks during the
#   application process

# Solution: Even with a high completion rate, ~31% of users fail to complete the 
#   application resulting in revenue lost. Around 10,000 applicants fail to
#     finish so PerPay needs to revise their process to identify the problem


# Graph of Application Completion Rate
ggplot(data = data.frame(Status = c("Completed", "Started"),
                         Count = c(Completed_merged, Starting_merged)),
       aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Application Completion Rate",
       x = "Status",
       y = "Count") +
  theme_minimal()


# Metric 3: Loan Approval Rate
Approval_merged <- nrow(merged_data[!is.na(merged_data$Awaiting_Payment_Time),])
Approval_merged

Approval_merged / Completed_merged # ~70% of all completed applications were
# approved of their loans

# This metric just shows how many people are able to meet the criteria of the 
#   loan process

# Solution: PerPay may evaluate the reason for 30% of applicants unable to get 
#   a loan and maybe revise the underwriting process.


# Graph of Loan Approval Rate
ggplot(data = data.frame(Status = c("Approved", "Completed Applications"),
                         Count = c(Approval_merged, Completed_merged)),
       aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Application Completion Rate",
       x = "Status",
       y = "Count") +
  theme_minimal()


# Metric 4: Cancellation Rate
Canceled_merged <- nrow(merged_data[!is.na(merged_data$Canceled_Time),])
Canceled_merged

Canceled_merged / Starting_merged # ~94% of started applications are canceled

# This metric describes how many applicants follow through from their initial 
#   interest

# Solution: Identify why applicants are canceling and find ways to alleviate 
#   this problem


# Loan Repayment Date
  # Date Format
merged_data$approval_date <- as.Date(merged_data$Awaiting_Payment_Time)
merged_data$Repayment_date <- as.Date(merged_data$Repayment_Time)

  # Time difference in days
merged_data$time_difference <- as.numeric(merged_data$Repayment_date - 
                                            merged_data$approval_date)
  # Loans within 15 days or less
within_15_days <- merged_data %>% filter(time_difference <= 15)

  # Count the number of loans
number_of_loans_within_15_days <- nrow(within_15_days)
number_of_loans_within_15_days
# 725 loans enter repayment within 15 days or less

# Number of loans that entered repayment
nrow(merged_data[!is.na(merged_data$Repayment_date),])
# 1080 loans entered repayment 

# Total Loan Amount ~ 16.2 million
sum(merged_data$Amount, na.rm = TRUE)
