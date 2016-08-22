library(dplyr)

# Step 0: Load the Data in RStudio
# Save the data set as a CSV file called titanic_original.csv and load it in
# RStudio into a data frame.

df <- read.csv("titanic_original.csv")
df <- tbl_df(df)

# Step 1: Port of Embarkation
# The embarked column has some missing values, which are known to 
# correspond to passengers who actually embarked at Southampton. Find
# the missing values and replace them with S. (Caution: Sometimes a
# missing value might be read into R as a blank or empty string.)

is.missing <- function(x) {
  grepl("^\\s*$",x)|is.na(x)
} 

df$embarked[is.missing(df$embarked)] <- "S"

# Step 2: Age
# You'll notice that a lot of the values in the Age column are missing.
# While there are many ways to fill these missing values, using the mean
# or median of the rest of the values is quite common in such cases.
# Calculate the mean of the Age column and use that value to populate
# the missing values
# Think about other ways you could have populated the missing values
# in the age column. Why would you pick any of those over the mean
# (or not)?

df$age[is.missing(df$age)] <- mean(df$age, na.rm = TRUE)

# Step 3: Lifeboat
# You're interested in looking at the distribution of passengers in
# different lifeboats, but as we know, many passengers did not make it
# to a boat :-( This means that there are a lot of missing values in
# the boat column. Fill these empty slots with a dummy value e.g. the
# string 'None' or 'NA'

levels(df$boat) <- c(levels(df$boat),"None")
df$boat[is.missing(df$boat)] <- "None"

# Step 4: Cabin
# You notice that many passengers don't have a cabin number associated
# with them. Does it make sense to fill missing cabin numbers with a
# value? What does a missing value here mean?
# You have a hunch that the fact that the cabin number is missing might
# be a useful indicator of survival. Create a new column has_cabin_number
# which has 1 if there is a cabin number, and 0 otherwise.

df <- mutate(df,has_cabin_number = as.integer(!is.missing(cabin)))

write.csv(df, "titanic_clean.csv")
