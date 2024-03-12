# Remove all objects just to be safe.
     rm(list=ls(all=TRUE)) # Remove all objects from R history
     options(scipen=999) # Turn off Scientific Notation

# Load the dataset from the specified URL into the ec2024 data frame
ec2024 <- read.csv("https://raw.githubusercontent.com/jcervas/Data/master/Elections/Presidential/Pres%20by%20State/president_state.csv")

# Display the first few rows of the dataset to inspect its structure
head(ec2024)

# Display the dimensions of the dataset (number of rows and columns)
dim(ec2024)

# Initialize two new columns in the dataset with NA (missing values) to store lagging democratic share data
ec2024$lagging_one <- NA
ec2024$lagging_two <- NA

# Loop through elections from 1872 to 2020 in 4-year increments
for (year in seq(1872, 2020, 4)) {
     # Calculate the previous election year
     lag <- year - 4
     # Assign the Democratic share from the lag year to the current year's lagging_one column
     ec2024$lagging_one[ec2024$year %in% year] <- ec2024$dem[ec2024$year %in% lag]
}

# Loop through elections from 1876 to 2020 in 4-year increments
for (year in seq(1876, 2020, 4)) {
     # Calculate the election year eight years ago
     lag2 <- year - 8
     # Assign the Democratic share from the lag2 year to the current year's lagging_two column
     ec2024$lagging_two[ec2024$year %in% year] <- ec2024$dem[ec2024$year %in% lag2]
}

# Define the limits for the x and y axes of the plots
xlim <- ylim <- c(0, 1)
# Define labels for the x and y axes
xlab <- "Democratic Share of\n Previous Election"
ylab <- "Democratic Share of Recent Election"

# Subset the data for elections from 1984 to 2020
ec7284 <- subset(ec2024, year %in% seq(1872,1984, 4))

# Set up the plotting area to display two plots side by side
par(mfrow=c(1, 2))

# Plot the relationship between the Democratic share of the previous and the recent election for 1872-1984
plot(
     x=ec7284$lagging_one, 
     y=ec7284$dem, 
     xlim=xlim, 
     ylim=ylim,
     xlab=xlab,
     ylab=ylab, 
     main="1872-1984")
# Fit a linear model and add the regression line in red
reg1 <- lm(ec7284$dem ~ ec7284$lagging_one)
abline(reg1, col="red")
# Add text to the plot displaying the slope and adjusted R-squared of the regression, in red
text(
     x=0.8, y=0.1, 
     paste0(
          "Slope: ", round(summary(reg1)$coefficients[2], d=3),
          "\nAdj-R-Sqr: ", round(summary(reg1)$adj.r.squared, d=3)),
     col="red",
     cex=0.85)

# Subset the data for elections from 1984 to 2020
ec8420 <- subset(ec2024, year %in% seq(1984, 2020, 4))

# Plot the relationship between the Democratic share of the previous and the recent election for 1984-2020
plot(
     x=ec8420$lagging_one, 
     y=ec8420$dem, 
     xlim=xlim, 
     ylim=ylim,
     xlab=xlab,
     ylab=ylab, 
     main="1984-2020")
# Fit a linear model for the subsetted data and add the regression line in red
reg2 <- lm(ec8420$dem ~ ec8420$lagging_one)
abline(reg2, col="red")
# Add text to the plot displaying the slope and adjusted R-squared of the regression, in red
text(
     x=0.8, y=0.1, 
     paste0(
          "Slope: ", round(summary(reg2)$coefficients[2], d=3),
          "\nAdj-R-Sqr: ", round(summary(reg2)$adj.r.squared, d=3)), 
     col="red",
     cex=0.85)
