
x <- c(3, 12, 6, -5, 0, 8, 15, 1, -10, 7)
print(x)
y <- seq(min(x), max(x), length = 10)
print(y)

# Assuming you have defined vectors x and y before these calculations

# Assuming you have defined vectors x and y before these calculations

# Calculations for vector x:
sum_x <- sum(x)
mean_x <- mean(x)
sd_x <- sd(x)
var_x <- var(x)
mad_x <- mean(abs(x - mean(x)))
quartiles_x <- quantile(x, probs = c(0.25, 0.5, 0.75))
quintiles_x <- quantile(x, probs = seq(0.2, 0.8, by = 0.2))

# Calculations for vector y:
sum_y <- sum(y)
mean_y <- mean(y)
sd_y <- sd(y)
var_y <- var(y)
mad_y <- mean(abs(y - mean(y)))  # Corrected this line
quartiles_y <- quantile(y, probs = c(0.25, 0.5, 0.75))
quintiles_y <- quantile(y, probs = seq(0.2, 0.8, by = 0.2))

# Print the results, handling NAs:
print(c(sum_x, mean_x, sd_x, var_x, mad_x, quartiles_x, quintiles_x), na.print = "NA")
print(c(sum_y, mean_y, sd_y, var_y, mad_y, quartiles_y, quintiles_y), na.print = "NA")




z <- sample(x, size = 7, replace = TRUE)

result <- t.test(x, y)
p_value <- result$p.value
alpha <- 0.05
if (p_value < alpha) {
  cat("Differences in means are statistically significant.\n")
} else {
  cat("Differences in means are not statistically significant.\n")
}

# Sort vector x
sorted_x <- x[order(x)]

# Perform paired t-test
paired_t_result <- t.test(sorted_x, y, paired = TRUE)

# Print t-test result
print(paired_t_result)

negative_indices <- x < 0
negative_indices

x <- x[!negative_indices]

#problem 2
# Create the dataframe X
col1 <- c(1,2,3,NA,5)
col2 <- c(4,5,6,89,101)
col3 <- c(45,NA,66,121,201)
col4 <- c(14,NA,13,NA,27)
X <- data.frame(col1, col2, col3, col4)

# Identify rows with missing values
rows_with_missing <- X[complete.cases(X), ]

# Display rows with missing values
print(rows_with_missing)

y <- c(3,12,99,99,7,99,21)
y[y == 99] <- NA
num_missing <- sum(is.na(y))
print(num_missing)

#problem 3

# Replace with your actual directory and file name
directory <- "C:/Users/ASUS/Downloads"
file_name <- "college.csv"
file_path <- file.path(directory, file_name)

# Read the CSV file into a data frame
college <- read.csv(file_path)

rownames (college) <- college [,1]
View (college )
college <- college [,-1]
View(college)

# Remove rows with missing values
complete_data <- na.omit(college[, c("Private", "Outstate")])

# Plot side-by-side boxplots
plot(complete_data$Private, complete_data$Outstate,
     xlab = "Private",
     ylab = "Outstate",
     main = "Side-by-Side Boxplots of Outstate vs Private")

# Remove rows with missing and infinite values
complete_data <- college[complete.cases(college[, c("Private", "Outstate")]) & is.finite(college$Outstate), ]

# Plot side-by-side boxplots
boxplot(complete_data$Private, complete_data$Outstate,
     xlab = "Private",
     ylab = "Outstate",
     main = "Side-by-Side Boxplots of Outstate vs Private")


# Make sure to install and load the ggplot2 package
# install.packages("ggplot2")
# Install ggplot2 package (if not already installed)
install.packages("ggplot2")



library(ggplot2)

# Check data and data types
str(college)

# Remove missing and infinite values
complete_data <- college[complete.cases(college[, c("Private", "Outstate")]) & is.finite(college$Outstate), ]

# Create side-by-side boxplots using ggplot2
ggplot(complete_data, aes(x = Private, y = Outstate)) +
  geom_boxplot() +
  xlab("Private") +
  ylab("Outstate") +
  ggtitle("Side-by-Side Boxplots of Outstate vs Private")


Elite <- rep ("No", nrow(college ))
Elite [college$Top10perc >50] <- "Yes"
Elite <- as.factor (Elite)
college <- data.frame(college ,Elite)

# Part v: Summary of elite universities
summary(college$Elite)

# Part vi: Side-by-side boxplots of Outstate versus Elite
plot(college$Elite, college$Outstate, xlab = "Elite", ylab = "Outstate", main = "Outstate vs Elite")

# Divide the print window into 2x2 regions for multiple plots
par(mfrow = c(2, 2))

# Create histograms with different numbers of bins
hist(college$Top10perc, main = "Histogram of Top10perc", xlab = "Top10perc")
hist(college$Apps, main = "Histogram of Apps", xlab = "Apps")
hist(college$Accept, main = "Histogram of Accept", xlab = "Accept")
hist(college$Enroll, main = "Histogram of Enroll", xlab = "Enroll")

# Reset the plotting layout to default
par(mfrow = c(1, 1))

# problem 4
install.packages("plyr")
library(plyr)
?baseball
bball <- baseball
# Set sacrifice flies (sf) to 0 for players before 1954
bball$sf[bball$yearID < 1954] <- 0
# Set missing hit by pitch (hbp) values to 0
bball$hbp[is.na(bball$hbp)] <- 0
# Exclude player records with fewer than 50 at bats (ab)
bball <- bball[bball$ab >= 50, ]
# Calculate on-base percentage (OBP) for each player
bball$OBP <- (bball$h + bball$bb + bball$hbp) /
  (bball$ab + bball$bb + bball$hbp + bball$sh)

# Print the cleaned and calculated data
print(bball)
# Compute on-base percentage (OBP) for each player
baseball$obp <- (baseball$h + baseball$bb + baseball$hbp) /
  (baseball$ab + baseball$bb + baseball$hbp + baseball$sf)

# Print the data with OBP included
print(baseball)

# Assuming you've loaded the "plyr" package and loaded the "baseball" dataset
baseball$obp <- (baseball$h + baseball$bb + baseball$hbp) /
  (baseball$ab + baseball$bb + baseball$hbp + baseball$sf)


library(plyr)

# Sort the data based on OBP in descending order
sorted_baseball <- arrange(baseball, desc(obp))

# Print the top five records
top_five <- head(sorted_baseball, 5)
print(top_five[, c("year", "id", "obp")])

print(top_five)


#problem 5 

data(quakes, package = "datasets")


plot(quakes$mag, quakes$depth, xlab = "Magnitude", ylab = "Depth")

quakeAvgDepth <- aggregate(depth ~ mag, data = quakes, FUN = mean)
print(quakeAvgDepth )

colnames(quakeAvgDepth) <- c("Magnitude", "Average_Depth")

plot(quakeAvgDepth$Magnitude, quakeAvgDepth$Average_Depth, xlab = "Magnitude", ylab = "Average Depth")

