library(dplyr)
library(stringr)
library(ggplot2)
library(car)
library(MASS)
library(caret)
library(tidyr)
library(lubridate)
library(corrplot)
library(GGally)
library(patchwork)

#data import
filepath = "C:\\High school DxD\\Degree\\Year 2\\DA\\5. credit score classification data.csv"
df = read.csv(filepath)
View(df)
str(df)
names(df) = tolower(names(df))
#clean data
#delayed payment
df$num_of_delayed_payment = replace(df$num_of_delayed_payment, df$num_of_delayed_payment == "", NA)
df$num_of_delayed_payment = as.integer(gsub("[_-]", "", df$num_of_delayed_payment))
df$num_of_delayed_payment = replace(df$num_of_delayed_payment, df$num_of_delayed_payment > 28, NA)
df$num_of_delayed_payment = replace(df$num_of_delayed_payment, is.na(df$num_of_delayed_payment), median(df$num_of_delayed_payment, na.rm = TRUE))

#credit utilization ratio
df$credit_utilization_ratio = replace(df$credit_utilization_ratio, df$credit_utilization_ratio == "", NA)
df$credit_utilization_ratio = replace(df$credit_utilization_ratio, is.na(df$credit_utilization_ratio), median(df$credit_utilization_ratio, na.rm = TRUE))

#amount invested monthly
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

#total emi per month
df$amount_invested_monthly = replace(df$amount_invested_monthly,df$amount_invested_monthly == "", NA)
df$amount_invested_monthly = replace(df$amount_invested_monthly,df$amount_invested_monthly == "__10000__", NA)
df$amount_invested_monthly = as.numeric(df$amount_invested_monthly)
df$amount_invested_monthly = replace(df$amount_invested_monthly,is.na(df$amount_invested_monthly), median(df$amount_invested_monthly, na.rm = TRUE))
df$amount_invested_monthly <- round(df$amount_invested_monthly, 2)

# How does the frequency of delayed payments correlate with changes in credit score?
# Convert credit_score to a factor with ordered levels
df$credit_score <- factor(df$credit_score, levels = c("Poor", "Standard", "Good"), ordered = TRUE)

# Fit the ordinal logistic regression model
model <- polr(credit_score ~ num_of_delayed_payment, data = df, Hess = TRUE)

# Summarize the model
summary(model)

# Get the p-values for the coefficients
coefficients <- coef(summary(model))
p_values <- pnorm(abs(coefficients[, "t value"]), lower.tail = FALSE) * 2

# Combine coefficients and p-values into a data frame
results <- cbind(coefficients, p_value = p_values)
print(results)
# Calculate odds ratios for the coefficients
exp_coeff <- exp(coef(model))
exp_coeff

# Perform Levene's Test
levene_test <- leveneTest(num_of_delayed_payment ~ credit_score, data = df)
levene_test

# Perform ANOVA
anova_result <- aov(num_of_delayed_payment ~ credit_score, data = df)
summary(anova_result)


#Visualize
ggplot(df, aes(x = num_of_delayed_payment, fill = credit_score)) +
  geom_bar(position = "fill", color = "black") +
  labs(x = "Number of Delay Payments", y = "Proportion", fill = "Credit Score") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("Poor" = "red", "Standard" = "yellow", "Good" = "green"))
# Create boxplot
ggplot(df, aes(x = credit_score, y = num_of_delayed_payment, fill = credit_score)) +
  geom_boxplot() +
  labs(x = "Credit Score", y = "Number of Delay Payments", fill = "Credit Score") +
  theme_minimal() +
  theme(legend.position = "none")

dfbarplot = df %>%
  group_by(credit_score) %>%
  summarize(mean_delayed_payment = mean(num_of_delayed_payment, na.rm = TRUE)) %>%
  ggplot(aes(x = credit_score, y = mean_delayed_payment, fill = credit_score)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Delayed Payments by Credit Score",
       x = "Credit Score Category",
       y = "Mean Number of Delayed Payments") +
  theme_minimal()
dfbarplot

# Create a violin plot
ggplot(df, aes(x = credit_score, y = num_of_delayed_payment, fill = credit_score)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Delayed Payments by Credit Score",
       x = "Credit Score Category",
       y = "Number of Delayed Payments",
       fill = "Credit Score") +
  theme_minimal()

# number of delayed payments impact the credit score differently across various credit utilization ratios
# Perform Levene's Test for Credit Utilization Ratio
levene_utilization <- leveneTest(credit_utilization_ratio ~ credit_score, data = df)
levene_utilization

# Perform ANOVA for Credit Utilization Ratio
anova_utilization <- aov(credit_utilization_ratio ~ credit_score, data = df)
summary(anova_utilization)

# Fit the ordinal logistic regression model with interaction terms
model <- polr(credit_score ~ num_of_delayed_payment * credit_utilization_ratio, data = df, Hess = TRUE)

# Summarize the model
summary(model)

# Get the p-values for the coefficients
coefficients <- coef(summary(model))
p_values <- pnorm(abs(coefficients[, "t value"]), lower.tail = FALSE) * 2

# Combine coefficients and p-values into a data frame
results <- cbind(coefficients, p_value = p_values)
print(results)

#Visualize data
# Fit the interaction model
interaction_model <- lm(num_of_delayed_payment ~ credit_utilization_ratio * credit_score, data = df)
summary(interaction_model)

# Create interaction plot
ggplot(df, aes(x = credit_utilization_ratio, y = num_of_delayed_payment, color = credit_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Plot: Delayed Payments and Credit Utilization Ratio",
       x = "Credit Utilization Ratio",
       y = "Number of Delayed Payments",
       color = "Credit Score") +
  theme_minimal()
# Create an interaction plot
interaction.plot(df$credit_utilization_ratio, df$credit_score, df$num_of_delayed_payment,
                 main = "Interaction Plot: Delayed Payments and Credit Utilization Ratio",
                 xlab = "Credit Utilization Ratio",
                 ylab = "Number of Delayed Payments",
                 col = rainbow(length(unique(df$credit_score))),
                 trace.label = "Credit Score")


# Scatter Plot Matrix
ggpairs(df, columns = c("num_of_delayed_payment", "credit_utilization_ratio"), ggplot2::aes(color = credit_score)) +
  labs(title = "Scatter Plot Matrix of Delayed Payments and Credit Utilization Ratio by Credit Score") +
  theme_minimal()

# Generate predicted probabilities for visualization
newdata <- expand.grid(
  num_of_delayed_payment = seq(min(df$num_of_delayed_payment), max(df$num_of_delayed_payment), length.out = 100),
  credit_utilization_ratio = seq(min(df$credit_utilization_ratio), max(df$credit_utilization_ratio), length.out = 100)
)

# Predict probabilities for each level of credit score
pred_probs <- predict(model, newdata, type = "probs")

# Add predicted probabilities to newdata
newdata$prob_poor <- pred_probs[, "Poor"]
newdata$prob_standard <- pred_probs[, "Standard"]
newdata$prob_good <- pred_probs[, "Good"]

# Visualize the predicted probabilities for "good" credit score
ggplot(newdata, aes(x = num_of_delayed_payment, y = credit_utilization_ratio, fill = prob_good)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5, name = "Probability of Good Credit Score") +
  labs(title = "Interaction Effect of Delay Payments and Credit Utilization on Credit Score",
       x = "Number of Delayed Payments",
       y = "Credit Utilization Ratio") +
  theme_minimal()

# Heat map for "poor" credit score probability
ggplot(newdata, aes(x = num_of_delayed_payment, y = credit_utilization_ratio, fill = prob_poor)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5, name = "Probability of Poor Credit Score") +
  labs(title = "Interaction Effect of Delay Payments and Credit Utilization on Poor Credit Score",
       x = "Number of Delayed Payments",
       y = "Credit Utilization Ratio") +
  theme_minimal()



# Scatter plot with regression lines for each credit score category
ggplot(df, aes(x = num_of_delayed_payment, y = credit_utilization_ratio, color = credit_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Delayed Payments and Credit Utilization Ratio by Credit Score",
       x = "Number of Delayed Payments",
       y = "Credit Utilization Ratio",
       color = "Credit Score") +
  theme_minimal()

# Facet grid plot for number of delayed payments vs credit utilization ratio by credit score category
ggplot(df, aes(x = num_of_delayed_payment, y = credit_utilization_ratio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_grid(. ~ credit_score) +
  labs(title = "Delayed Payments vs Credit Utilization Ratio by Credit Score Category",
       x = "Number of Delayed Payments",
       y = "Credit Utilization Ratio") +
  theme_minimal()

# Density plot for number of delayed payments by credit score category
ggplot(df, aes(x = num_of_delayed_payment, fill = credit_score)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Delayed Payments by Credit Score Category",
       x = "Number of Delayed Payments",
       fill = "Credit Score") +
  theme_minimal()

# Density plot for credit utilization ratio by credit score category
ggplot(df, aes(x = credit_utilization_ratio, fill = credit_score)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Credit Utilization Ratio by Credit Score Category",
       x = "Credit Utilization Ratio",
       fill = "Credit Score") +
  theme_minimal()

# Frequency Polygon for Credit Utilization Ratio by Credit Score
ggplot(df, aes(x = credit_utilization_ratio, color = credit_score)) +
  geom_freqpoly(binwidth = 0.05, linewidth = 1.2) +  # Use linewidth instead of size
  labs(title = "Frequency Polygon of Credit Utilization Ratio by Credit Score",
       x = "Credit Utilization Ratio",
       y = "Frequency",
       color = "Credit Score") +
  theme_minimal()

# Violin Plot for Credit Utilization Ratio by Credit Score
ggplot(df, aes(x = credit_score, y = credit_utilization_ratio, fill = credit_score)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Credit Utilization Ratio by Credit Score",
       x = "Credit Score Category",
       y = "Credit Utilization Ratio",
       fill = "Credit Score") +
  theme_minimal()


#The number of delayed payments and the amount invested monthly can both significantly impact your credit score.

levene_invested <- leveneTest(amount_invested_monthly ~ credit_score, data = df)
print(levene_invested)

anova_invested <- aov(amount_invested_monthly ~ credit_score, data = df)
summary(anova_invested)

# Fit the ordinal logistic regression model with both predictors
model <- polr(credit_score ~ num_of_delayed_payment + amount_invested_monthly, data = df, Hess = TRUE)

# Summarize the model
summary(model)

# Get the p-values for the coefficients
coefficients <- coef(summary(model))
p_values <- pnorm(abs(coefficients[, "t value"]), lower.tail = FALSE) * 2

# Combine coefficients and p-values into a data frame
results <- cbind(coefficients, p_value = p_values)
results

# Generate predicted probabilities for visualization
newdata <- expand.grid(
  num_of_delayed_payment = seq(min(df$num_of_delayed_payment), max(df$num_of_delayed_payment), length.out = 100),
  amount_invested_monthly = seq(min(df$amount_invested_monthly), max(df$amount_invested_monthly), length.out = 100)
)

# Predict probabilities for each level of credit score
pred_probs <- predict(model, newdata, type = "probs")

# Add predicted probabilities to newdata
newdata$prob_poor <- pred_probs[, "Poor"]
newdata$prob_standard <- pred_probs[, "Standard"]
newdata$prob_good <- pred_probs[, "Good"]

# Interaction plot for "good" credit score probability
ggplot(newdata, aes(x = num_of_delayed_payment, y = amount_invested_monthly, fill = prob_good)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 0.5, name = "Probability of Good Credit Score") +
  labs(title = "Interaction Effect of Delayed Payments and Monthly Investment on Good Credit Score",
       x = "Number of Delayed Payments",
       y = "Amount Invested Monthly") +
  theme_minimal()

# Boxplot for number of delayed payments by credit score
ggplot(df, aes(x = credit_score, y = num_of_delayed_payment)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Delayed Payments by Credit Score Category",
       x = "Credit Score Category",
       y = "Number of Delayed Payments") +
  theme_minimal()

# Boxplot for amount invested monthly by credit score
ggplot(df, aes(x = credit_score, y = amount_invested_monthly)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribution of Monthly Investment by Credit Score Category",
       x = "Credit Score Category",
       y = "Amount Invested Monthly") +
  theme_minimal()

# Scatter plot with regression lines for each credit score category
ggplot(df, aes(x = num_of_delayed_payment, y = amount_invested_monthly, color = credit_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Delayed Payments, Monthly Investment, and Credit Score",
       x = "Number of Delayed Payments",
       y = "Amount Invested Monthly",
       color = "Credit Score") +
  theme_minimal()
# Violin Plot for Amount Invested Monthly by Credit Score
ggplot(df, aes(x = credit_score, y = amount_invested_monthly, fill = credit_score)) +
  geom_violin(trim = FALSE) +
  labs(title = "Violin Plot of Amount Invested Monthly by Credit Score",
       x = "Credit Score Category",
       y = "Amount Invested Monthly",
       fill = "Credit Score") +
  theme_minimal()
# Frequency Polygon for Amount Invested Monthly by Credit Score
ggplot(df, aes(x = amount_invested_monthly, color = credit_score)) +
  geom_freqpoly(binwidth = 100, linewidth = 1.2) +
  labs(title = "Frequency Polygon of Amount Invested Monthly by Credit Score",
       x = "Amount Invested Monthly",
       y = "Frequency",
       color = "Credit Score") +
  theme_minimal()

interaction_model <- lm(num_of_delayed_payment ~ amount_invested_monthly * credit_score, data = df)
summary(interaction_model)
# Create interaction plot
ggplot(df, aes(x = amount_invested_monthly, y = num_of_delayed_payment, color = credit_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Plot: Delayed Payments and Amount Invested Monthly",
       x = "Amount Invested Monthly",
       y = "Number of Delayed Payments",
       color = "Credit Score") +
  theme_minimal()

interaction.plot(df$amount_invested_monthly, df$credit_score, df$num_of_delayed_payment,
                 main = "Interaction Plot: Delayed Payments and Amount Invested Monthly",
                 xlab = "Amount Invested Monthly",
                 ylab = "Number of Delayed Payments",
                 col = rainbow(length(unique(df$credit_score))),
                 trace.label = "Credit Score")
