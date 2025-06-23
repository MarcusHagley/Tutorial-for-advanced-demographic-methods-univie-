# Script 1: R Basics & Best Practices (First Class)

# -------------------------------------------------------------
# This is the first class where we will go through basic R concepts.
# Where you are reading this now is called the "Source Editor" or "Source Panel".
# This is where we write and save code. You can also write code in the console,
# but it's best practice to store it in scripts for reproducibility.
# -------------------------------------------------------------

# You can open new scripts by either CTRL + SHIFT + N or File > New file > R script


# 1. Introduction: Variables & Assignments
# Let's start with the basics: the "<-" which is called the assign operator. 
# Here we are assigning a numeric variable(Represents numbers (integers or decimals))
# to the box called "x"

x <- 5  # Numeric variable

# You can also do this with  character variables (Represents text or string values)
y <- "Hello, R!"  # Character variable

# Or logical variables(Represents TRUE or FALSE values)
z <- TRUE  # Logical variable

# These are also called vectors. If you want to see what is in a vector you can
# use the command "print"
print(x) # shows us what is in the vector x
print(y) # shows us what is in vector y and so forth

# A vector can have multiple values. We do this by adding a "c(value1,value2,value3)"
# c() stands for “combine” (or “concatenate”). 
# You can think of it like a little tool that gathers a set of values and puts them together into the vector.

x1 <- c(1,2,3,4) # Adding a box (vector) with the number 1,2,3,4
print(x1)


# Task: Create three new variables (numeric, character, logical) and print them.
num_var <- 
  num_var2 <-  
  char_var <- 
  char_var2 <- 
  log_var <- 
  print(num_var)
print(char_var)
print(log_var)

# Did you use the correct type of data? You can use the class function to examine them:

class(num_var)
class(char_var)
class(log_var)

# Numerical variables work like a calculator. You can use mathematical operators 
# such as +, -, /, *, =. * is a bit special in matrix algebra.
# Here are some examples:

char_var -  char_var2 #What does it say?
num_var - num_var2 # What does it say?
num_var * num_var2 
num_var / num_var2 
num_var^2  # Exponentiation
sqrt(num_var2) #square root
exp(num_var) # exponential



# 2. Best Practices in Writing R Scripts
# - Use clear variable names
# - Comment your code using '#' before text
# - Avoid doing everything in the console: write scripts instead

# Task: Create a script section comment and a meaningful variable name
# --- Section: User Information ---
user_name <- "Marcus"
user_age <- 100
print(user_name)


# 3. Data Structures
# Vectors, lists, and matrices are fundamental building blocks in R.
# Lists are flexible containers most widely used by very advanced coders
# Matrix is a two-dimensional array, usually used in matrix calculations
# Dataframe is a table-like structure, most commonly used.  

my_vector <- c(1, 2, 3, 4, 5)
my_list <- list(name = "Marcus", age = 30)
my_matrix <- matrix(1:9, nrow = 3)

# Task: Print the structure of each object
str(my_vector) #If you want to know what str() does you can write "?str()" in the console
str(my_list)
str(my_matrix)

# Task: Create a new vector with five elements and print the third element
new_vector <- c("A", "B", "C", "D", "E")
print(new_vector[3])  # The square brackets [] are used for accessing or subsetting parts of an object. Here we are saying take the third element. 


# 4. Working with Data Frames
# A data.frame is like a table with rows and columns. Each column is a vector.
df <- data.frame(name = c("Alice", "Bob","Micheal"), age = c(25, 30,27))
print(df)

# Task: Add a new column "city" to the data frame
df$city <- c("New York", "London")
print(df)


# 5. Installing and Using Packages
# install.packages("ggplot2")  # Uncomment to install a package
# install.packages(c("ggplot2", "dplyr"))  to install multiple packages at once

library(ggplot2)

# Task: Install and load a package
# Uncomment the line below to install if needed
# install.packages("dplyr")
library(dplyr)


# 6. Basic Data Manipulation
# 'dplyr' uses the pipe operator '%>%' to chain functions.
# mutate() can add or modify columns.

df <- df %>% 
  mutate(age_plus_5 = age + 5) # We are creating a new variable using age and adding 5 to it

print(df)

# Task: Filter the data frame for individuals 
# 1. older than 27
# 2. older or equal to 27
# 3. Younger

# 1.
older_df <- df %>% 
  filter(age > 27)

print(older_df)

# 2.
older_equal_df <- df %>% 
  filter(age >= 27)

print(older_equal_df)

# 3.
younger_df <- df %>% 
  filter(age < 27)

print(younger_df)


# 7. Basic Visualization
# ggplot2 allows for flexible plotting.

ggplot(df, aes(x = name, y = age)) + geom_bar(stat = "identity")

# Task: Create a scatter plot of 'age_plus_5' against 'age'
ggplot(df, aes(x = age, y = age_plus_5)) + geom_point()


# -------------------------------------------------------------
# Additional Practice Tasks (Fill in the missing pieces)
# 1. Create a numeric vector called 'my_numbers' with elements 10, 20, 30, 40.
#    Print the second element.
my_numbers 
print(my_numbers[___])

# 2. Convert 'my_numbers' to a data.frame with one column named 'values'.
df_numbers 
print(df_numbers)

# 3. Use dplyr to filter 'df' for rows where 'age_plus_5' is less than 30.
#    Store in an object named 'younger_equal_df'.
younger_df <- df %>% 
  filter(___ _ 30)

print(younger_df)

# 4. Create a bar chart with 'df', using 'name' on the x-axis and 'city' on the y-axis.
#    (Hint: consider geom_bar(stat = "identity"), though city is not numeric.)

ggplot(df, aes(x = ___, y = ___)) + geom_bar(stat = "identity")

# -------------------------------------------------------------
# This concludes the first class!
