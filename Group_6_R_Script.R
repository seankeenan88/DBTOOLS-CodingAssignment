# Load libraries
library(ggplot2)
library(readxl)
Group6 <- read_excel("C:/Users/Sean/Desktop/Group 6/Group6.xlsx")
View(Group6)
# View the structure of the dataset
str(Group6)
# Summary statistics
summary(Group6)
# Histogram of Stars
ggplot(Group6, aes(x = Stars)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Stars", x = "Stars", y = "Frequency")
# Scatter plot of Stars vs. Total Helpful Votes Received
ggplot(Group6, aes(x = Stars, y = `Total Helpful Votes Received`)) +
  geom_point(color = "blue") +
  labs(title = "Stars vs. Total Helpful Votes Received", x = "Stars", y = "Total Helpful Votes Received")
# Create scatter plot of Stars vs. Helpfulness
ggplot(Group6, aes(x = Stars, y = Helpfulness)) +
  geom_point(color = "orange") +
  labs(title = "Stars vs. Helpfulness", x = "Stars", y = "Helpfulness")                                                                               # Check for missing values in the 'Total Helpful Votes Received' column
if (any(is.na(Group6$`Total Helpful Votes Received`)) || any(!is.numeric(Group6$`Total Helpful Votes Received`))) {
  stop("Invalid values detected in 'Total Helpful Votes Received' column.")
}
# Histogram of Total Helpful Votes Received
ggplot(Group6, aes(x = `Total Helpful Votes Received`)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Total Helpful Votes Received", x = "Total Helpful Votes Received", y = "Frequency")                                                                                                                                                                               # Check for missing values in the 'Whether Purchase is verified or not' column
if (any(is.na(Group6$`Whether Purchase is verified or not`))) {
  stop("Invalid values detected in 'Whether Purchase is verified or not' column.")
}
# Convert 'Whether Purchase is verified or not' to factor if it's not already
if (!is.factor(Group6$`Whether Purchase is verified or not`)) {
  Group6$`Whether Purchase is verified or not` <- factor(Group6$`Whether Purchase is verified or not`)
}
# Create data for pie chart
verified_counts <- table(Group6$`Whether Purchase is verified or not`)
# Create pie chart
pie(verified_counts,
    labels = c("Not Verified", "Verified"),
    col = c("red", "lightgreen"),
    main = "Verified Purchases vs Not Verified")
# Linear regression model
model <- lm(Stars ~ Helpfulness, data = Group6)
summary(model)

