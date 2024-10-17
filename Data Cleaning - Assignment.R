library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Data import

df = read.csv("/Users/vimalraaj/Downloads/Programming For Data Analysis/PFDA Assignment/5. credit score classification data.csv")
df
View(df)

#Data cleaning

#Lowercase all columns name

names(df) = tolower(names(df))
colnames(df)

#Check how many NAs in dataset by column

colSums(is.na(df))

#id
#customer_id
#month
#name

pattern <- "[,_\"]"
df$name <- gsub(pattern, "", as.character(df$name))
df$name = replace(df$name, df$name == "", NA)

df <- df %>%
  group_by(customer_id) %>%
  fill(name, .direction = "downup") %>%
  ungroup()

#age

df$age <- gsub("[-_]", "", as.character(df$age))
df$age <- as.numeric(df$age)

mode_age_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df <- df %>%
  group_by(customer_id) %>%
  mutate(age = mode_age_function(age[!is.na(age)]))  %>%
  ungroup()

#ssn

df$ssn = replace(df$ssn, df$ssn == "#F%$D@*&8", NA)

df <- df %>%
  group_by(customer_id) %>%
  fill(ssn, .direction = "downup") %>%
  ungroup()

#occupation

df$occupation <- replace(df$occupation, df$occupation == "_______", NA)
df$occupation <- gsub("[_]", " ", as.character(df$occupation))

df <- df %>%
  group_by(customer_id) %>%
  fill(occupation, .direction = "downup") %>%
  ungroup()

#annual_income

df$annual_income <- gsub("_", "", as.character(df$annual_income))
df$annual_income <- as.numeric(df$annual_income)
df$annual_income = replace(df$annual_income, df$annual_income == "", NA)
#maximum monthly income * 12 = 182455
df$annual_income = replace(df$annual_income, df$annual_income > 182455, NA)
df$annual_income[is.na(df$annual_income)] <- df$monthly_inhand_salary[is.na(df$annual_income)] * 12
df$annual_income = replace(df$annual_income, is.na(df$annual_income), median(df$annual_income, na.rm = TRUE))

#monthly_inhand_salary

df$monthly_inhand_salary = replace(df$monthly_inhand_salary, df$monthly_inhand_salary == "", NA)
df$monthly_inhand_salary <- as.numeric(df$monthly_inhand_salary)
df$monthly_inhand_salary[is.na(df$monthly_inhand_salary)] <- df$annual_income[is.na(df$monthly_inhand_salary)] / 12

#num_bank_accounts

df$num_bank_accounts <- as.integer(df$num_bank_accounts)
df$num_bank_accounts = replace(df$num_bank_accounts, df$num_bank_accounts == "", NA)
df$num_bank_accounts = replace(df$num_bank_accounts, df$num_bank_accounts > 10, NA)
df$num_bank_accounts = replace(df$num_bank_accounts, df$num_bank_accounts < 1, 1)
df$num_bank_accounts = replace(df$num_bank_accounts, is.na(df$num_bank_accounts), median(df$num_bank_accounts, na.rm = TRUE))

#num_credit_card

df$num_credit_card = replace(df$num_credit_card, df$num_credit_card == "", NA)
df$num_credit_card <- as.integer(df$num_credit_card)
df$num_credit_card = replace(df$num_credit_card, df$num_credit_card > 10, NA)
df$num_credit_card = replace(df$num_credit_card, df$num_credit_card < 1, NA)
df$num_credit_card = replace(df$num_credit_card, is.na(df$num_credit_card), median(df$num_credit_card, na.rm = TRUE))

#interest_rate

df$interest_rate <- as.numeric(df$interest_rate)
df$interest_rate = replace(df$interest_rate, df$interest_rate > 34, median(df$interest_rate, na.rm = TRUE))
df$interest_rate = replace(df$interest_rate, df$interest_rate < 1, median(df$interest_rate, na.rm = TRUE))

#num_of_loan

df$num_of_loan = replace(df$num_of_loan, df$num_of_loan == "", NA)
df$num_of_loan <- as.integer(df$num_of_loan)
df$num_of_loan <- sapply(df$type_of_loan, function(x) {
  if (is.na(x)) {
    return(0)
  } else {
    return(length(unlist(strsplit(x, ","))))
  }
})

df$num_of_loan

#type_of_loan

df$type_of_loan <- replace(df$type_of_loan, df$type_of_loan == "", NA)

#delay_from_due_date

df$delay_from_due_date = replace(df$delay_from_due_date, df$delay_from_due_date == "", NA)
df$delay_from_due_date = replace(df$delay_from_due_date, (df$delay_from_due_date < 0), -df$delay_from_due_date[df$delay_from_due_date < 0])
df$delay_from_due_date = replace(df$delay_from_due_date, is.na(df$delay_from_due_date), median(df$delay_from_due_date, na.rm = TRUE))
df$delay_from_due_date

#num_of_delayed_payment

df$num_of_delayed_payment = replace(df$num_of_delayed_payment, df$num_of_delayed_payment == "", NA)
df$num_of_delayed_payment = as.integer(gsub("[_-]", "", df$num_of_delayed_payment))
df$num_of_delayed_payment = replace(df$num_of_delayed_payment, df$num_of_delayed_payment > 28, NA)
df$num_of_delayed_payment = replace(df$num_of_delayed_payment, is.na(df$num_of_delayed_payment), median(df$num_of_delayed_payment, na.rm = TRUE))
df$num_of_delayed_payment

#changed_credit_limit

df$changed_credit_limit = replace(df$changed_credit_limit, df$changed_credit_limit == "", NA)
df$changed_credit_limit = as.numeric(gsub("[_-]", "", df$changed_credit_limit))
df$changed_credit_limit = replace(df$changed_credit_limit, is.na(df$changed_credit_limit), median(df$changed_credit_limit, na.rm = TRUE))
df$changed_credit_limit

#num_credit_inquiries

df$num_credit_inquiries = replace(df$num_credit_inquiries, df$num_credit_inquiries == "", NA)
df$num_credit_inquiries = replace(df$num_credit_inquiries, df$num_credit_inquiries > 17, NA)
df$num_credit_inquiries = replace(df$num_credit_inquiries, is.na(df$num_credit_inquiries), median(df$num_credit_inquiries, na.rm = TRUE))
df$num_credit_inquiries

#credit_mix

df$credit_mix = replace(df$credit_mix, df$credit_mix == "", NA)
unique(df$credit_mix)
df$credit_mix = replace(df$credit_mix, df$credit_mix == "_", NA)
get_credit_mix_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
mode_value = get_credit_mix_mode(df$credit_mix)
df$credit_mix = replace(df$credit_mix, is.na(df$credit_mix), mode_value)
as.factor(df$credit_mix)
df$credit_mix

#outstanding_debt

df$outstanding_debt = replace(df$outstanding_debt, df$outstanding_debt == "", NA)
df$outstanding_debt = as.numeric(gsub("[_]", "", df$outstanding_debt))
df$outstanding_debt = replace(df$outstanding_debt, is.na(df$outstanding_debt), median(df$outstanding_debt, na.rm = TRUE))
df$outstanding_debt

#credit_utilization_ratio

df$credit_utilization_ratio = replace(df$credit_utilization_ratio, df$credit_utilization_ratio == "", NA)
df$credit_utilization_ratio = replace(df$credit_utilization_ratio, is.na(df$credit_utilization_ratio), median(df$credit_utilization_ratio, na.rm = TRUE))
df$credit_utilization_ratio

#credit_history_age

# function to parse Credit_History_Age
parse_age <- function(age) {
  if (is.na(age)) return(NA)
  matches <- regmatches(age, gregexpr("\\d+", age))
  if(length(matches[[1]]) < 2) return(NA)
  years <- as.numeric(matches[[1]][1])
  months <- as.numeric(matches[[1]][2])
  return(years * 12 + months)
}

# function to format age back to "X Years and Y Months"
format_age <- function(total_months) {
  if (is.na(total_months)) return(NA)
  years <- total_months %/% 12
  months <- total_months %% 12
  return(paste(years, "Years and", months, "Months"))
}

# function to handle NA filling 
fill_na <- function(age_vector) {
  for (i in seq_along(age_vector)) {
    if (is.na(age_vector[i])) {
      if (i == 1) {
        next
      } else {
        age_vector[i] <- age_vector[i-1] + 1
      }
    }
  }
  return(age_vector)
}

# function to handle the leftover NA filling 
fill_leftover_na <- function(age_vector) {
  last_non_na <- NULL
  for (i in seq_along(age_vector)) {
    if (!is.na(age_vector[i])) {
      last_non_na <- age_vector[i]
      break  # Exit loop after finding the first non-NA value
    }
  }
  if (!is.null(last_non_na)) {
    for (i in seq_along(age_vector)) {
      if (is.na(age_vector[i])) {
        age_vector[i] <- last_non_na - 1
        if (age_vector[i] %% 12 == 0) {
          age_vector[i] <- age_vector[i] - 1
          age_vector[i] <- 12
        }
      } else {
        last_non_na <- age_vector[i]
      }
    }
  }
  return(age_vector)
}

# Convert Credit_History_Age to total months
df <- df %>%
  mutate(credit_history_age_months = sapply(credit_history_age, parse_age))

# Apply the filling logic for each Customer_ID
df <- df %>%
  group_by(customer_id) %>%
  mutate(credit_history_age_months = fill_na(credit_history_age_months)) %>%
  ungroup()

df <- df %>%
  group_by(customer_id) %>%
  mutate(credit_history_age_months = fill_leftover_na(credit_history_age_months)) %>%
  ungroup()


# Convert back to original format
df <- df %>%
  mutate(credit_history_age = sapply(credit_history_age_months, format_age)) %>%
  select(-credit_history_age_months)


#payment_of_min_amount

df$payment_of_min_amount = replace(df$payment_of_min_amount,df$payment_of_min_amount == "NM", "No")
df$payment_of_min_amount = as.factor(df$payment_of_min_amount)

#total_emi_per_month

tabulate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df <- df %>%
  group_by(customer_id) %>%
  mutate(total_emi_per_month = tabulate_mode(total_emi_per_month[!is.na(total_emi_per_month)])) %>%
  ungroup()

df$total_emi_per_month <- as.numeric(df$total_emi_per_month)
df$total_emi_per_month <- round(df$total_emi_per_month, 2)

#amount_invested_monthly

df$amount_invested_monthly = replace(df$amount_invested_monthly,df$amount_invested_monthly == "", NA)
df$amount_invested_monthly = replace(df$amount_invested_monthly,df$amount_invested_monthly == "__10000__", NA)
df$amount_invested_monthly = as.numeric(df$amount_invested_monthly)
df$amount_invested_monthly = replace(df$amount_invested_monthly,is.na(df$amount_invested_monthly), median(df$amount_invested_monthly, na.rm = TRUE))
df$amount_invested_monthly <- round(df$amount_invested_monthly, 2)

#payment_behaviour

df$payment_behaviour = replace(df$payment_behaviour,df$payment_behaviour == "!@9#%8", NA)
find_mode <- function(x) {
  if (any(is.na(x))) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) > 0) {
      mode_val <- ux[which.max(tabulate(match(x[!is.na(x)], ux)))]
      x[is.na(x)] <- mode_val
    }
  }
  x
}

df <- df %>%
  group_by(customer_id) %>%
  mutate(payment_behaviour = find_mode(payment_behaviour)) %>%
  ungroup()

as.factor(df$payment_behaviour)

#monthly_balance

df$monthly_balance =replace(df$monthly_balance,df$monthly_balance == "", NA)
df$monthly_balance =replace(df$monthly_balance,df$monthly_balance == "__-333333333333333333333333333__", NA)
df$monthly_balance =replace(df$monthly_balance,is.na(df$monthly_balance),median(df$monthly_balance, na.rm = TRUE))
df$monthly_balance = as.numeric(df$monthly_balance)
df$monthly_balance <- round(df$monthly_balance, 2)

#credit score

as.factor(df$credit_score)
