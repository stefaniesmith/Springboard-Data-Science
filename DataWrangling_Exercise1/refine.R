library(tidyr)
library(dplyr)
library(stringr)

# Step 0: Load the Data in RStudio
# Save the data set as a CSV file called refine_original.csv and load it in
# RStudio into a data frame.

df <- read.csv("refine_original.csv")
df <- tbl_df(df)

# Step 1: Cleanup Brand Names
# Clean up the 'company' column, so all of the misspellings of the brand
# names are standardized. For example, you can transform the values in
# the column to be: philips, akzo, van houten and unilever (all lowercase).

df$company <- df$company %>% 
  tolower %>% 
  str_replace("^ak.*","akzo") %>% 
  str_replace("^[ph|f].*ps$","philips") %>% 
  str_replace("^uni.*ver$","unilever")

# Step 2: Separate Product Code and Number
# Separate the product code and product number into separate columns
# i.e. add two new columns called product_code and product_number,
# containing the product code and number respectively.

df <- separate(df, col = Product.code...number, into = c("Product.code","Number"))

# Step 3: Add Product Categories
# You learn that the product codes actually represent the following product
# categories:
# p = Smartphone
# v = TV
# x = Laptop
# q = Tablet
# In order to make the data more readable, add a column with the product
# category for each record.

lut <- c("p" = "Smartphone", "v" = "TV", "x" = "Laptop", "q" = "Tablet")
df$Product.category <- lut[df$Product.code]

# Step 4: Add Full Address for Geocoding
# You'd like to view the customer information on a map. In order to do that,
# the addresses need to be in a form that can be easily geocoded. Create a
# new column full_address that concatenates the three address fields
# (address, city, country), separated by commas.

df <- unite(df,full_address,address,city,country,sep=",")

# Step 5: Create Dummy Variables for Company and Product Category
# Both the company name and product category are categorical variables
# i.e. they take only a fixed set of values. In order to use them in further
# analysis you need to create dummy variables. Create dummy binary
# variables for each of them with the prefix company_ and product_ i.e.
# 1. Add four binary (1 or 0) columns for company: company_philips,
# company_akzo, company_van_houten and company_unilever
# 2. Add four binary (1 or 0) columns for product category:
# product_smartphone, product_tv, product_laptop and product_tablet

df <- df %>% 
  mutate(company_akzo = as.integer(company == "akzo")) %>% 
  mutate(company_philips = as.integer(company == "philips")) %>% 
  mutate(company_unilever = as.integer(company == "unilever")) %>% 
  mutate(company_van_houten = as.integer(company == "van houten")) %>% 
  mutate(product_smartphone = as.integer(Product.category == "Smartphone")) %>% 
  mutate(product_tv = as.integer(Product.category == "TV")) %>% 
  mutate(product_laptop = as.integer(Product.category == "Laptop")) %>% 
  mutate(product_tablet = as.integer(Product.category == "Tablet")) 
