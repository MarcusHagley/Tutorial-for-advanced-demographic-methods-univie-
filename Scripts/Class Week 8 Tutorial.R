
cat("In today's class we will focus on the logistic regression and how we can use public data to model fertility using the logistic regression. We will learn how to add covariates to give us a more nuanced picture of fertility patterns.This script builts on Miguel's scripts from the 2025S 234001-1 Applied Methods of Demographic Analysis (hands-on intensive course)
course.")

# Preamble ----------------------------------------------------------------

library(dplyr)        
library(tidyr)        
library(ggplot2)      
library(ggpmisc)


# Eurostat and logistic model ---------------------------------------------------
cat("In this part we will use Eurostat data. If you do not have it please install it")
library(eurostat)


library(eurostat)
library(dplyr)

# Download the dataset
fertility_data <- get_eurostat("demo_fagec", time_format = "num")

# Correct labels
fertility_data <- label_eurostat(fertility_data)

austria_births <- fertility_data %>%
  filter(
    geo == "Austria",
    TIME_PERIOD == "2011"
  ) %>% 
  rename(year = TIME_PERIOD,
         births = values,
         marriage = indic_de)

# Download "exposure"
pop_data <- get_eurostat("demo_pjan", time_format = "num")


austria_females <- pop_data %>%
  filter(
    geo == "AT",
    sex == "F",
    TIME_PERIOD %in% c("2011","2012")
  ) %>% 
  rename(year = TIME_PERIOD,
         exposure = values)



# Data processesing  -------------------------------------------------------

# Fix females: Remove the unecessary variables, let's contruct ASFRs
austria_females <- austria_females %>%
  filter(
    age != "TOTAL", # We don't need total 
    age != "UNK", # We don't need unknown
    age != "Y_LT1", 
    age != "Y_OPEN", # These are better for morality so we don't need the open
  ) %>% 
  mutate(age = as.numeric(sub("Y", "", age)))   %>%  # Makes the column numeric
  select(geo,age,year,exposure) %>% 
  filter(
    age > 14 & age < 51 # Constrain the fertility between 15-49 (same as Coale and Trussell)
  ) %>% 
  arrange(age, year) %>% 
  group_by(age) %>% 
  mutate(exposure = (exposure + lead(exposure)) / 2) %>%
  ungroup() %>% 
  filter(!is.na(exposure)) %>%               # As final year has no lead to calculate it will return as NA so we can easily remove
  select(geo, age, year, exposure)   

# Fix births
head(austria_births) # Here we see that the age variables has a lot of wierd things going on

#Let's tabulate
table(austria_births$age) # Here we see that they use a complete different format. 

cat("By examining and understanding the data we now know that we need to remove the 5 year categories, the Total and Unkown, and remove years from the singular ages")


# Here we do these things
austria_births_pop <- austria_births %>%
  filter(
    !age %in% c("Total", "Unknown") & 
      !grepl("^From", age) # 
  ) %>%
  mutate(age = as.numeric(sub("^([0-9]{1,2}).*", "\\1", age))) %>% # Makes the column numeric
  select(age,births,marriage) %>% 
  filter(
    age > 14 & age < 51,
    marriage == "Live births - total") 


cat("now we need to complete the dataset that we will use. By joining the two datasets by age")

# Merging the datasets
austria_pop <- austria_births_pop %>%
  inner_join(austria_females, by = "age") %>%
  mutate(
    ASFR = (births / exposure) * 1000    # make into a rate per 1 000
  ) %>%
  rename(category = marriage) %>% 
  arrange(age)

# Check TFR
austria_pop_tfr <- austria_pop %>%
  summarise(TFR = sum(ASFR) / 1000) %>% 
  print()


# Create the logit model
austria_pop_logit <- austria_pop %>% 
  arrange(age) %>%
  mutate(
    CFR = cumsum(ASFR),
    max_CFR = max(CFR),
    y = CFR / max_CFR,
    logit = log((1 - y) / y)
  ) %>% 
  filter(y > 0, y < 1) %>% 
  ungroup()


# Modelling logistic ---------------------------------------------------------------



# Visualizing the model:
p1 <- ggplot(austria_pop_logit, 
             aes(x = age, 
                 y = logit)) +  
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,  # Here we fit a regression model y(logit) ~ x(age)
    parse   = TRUE,
    label.x = "left",  # optional positioning tweaks
    label.y = "top"
  ) +
  labs(
    title = "Logit(CFR/TFR) by Age and Marital Status",
    x     = "Age",
    y     = "logit(CFR/TFR)",
    color = "Marital status"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p1)


# Fit a model

fit_model_pop <- lm(logit ~ age , data = austria_pop_logit)
summary(fit_model_pop)

fit_glm <- glm(y ~ age ,
               family = binomial(link = "logit"),
               data = austria_pop_logit)
summary(fit_glm)


# Logistic model part 2 ---------------------------------------------------



# 1. Create a new dataframe with fitted values
austria_pop_logit$predicted <- predict(fit_model_pop)


# 3. Plot original data and regression line
p2 <- ggplot(fit_model_pop, aes(x = age, y = y)) +
  geom_point(color = "blue") +                  # Actual data points
  geom_line(aes(y = 1/(1+exp(predicted))), color = "red") +# Regression line
  labs(title = "Fit logitistic model",
       x = "Age",
       y = "Cumulative distribution") +
  theme_minimal()


print(p2)




# Heterogeneiety ----------------------------------------------------------
cat("So now we will add marital status to our model to examine how the married vs. unmarried groups differ in both the pace (timing) and shape (spread) of their fertility schedules. By including an interaction between age and marital status, we can estimate separate level (m) and shape (k) parameters for each group and formally test whether the timing or intensity of childbearing truly differs between married and unmarried women.")

# Here we do these things
austria_births_hetero <- austria_births %>%
  filter(
    !age %in% c("Total", "Unknown") & 
      !grepl("^From", age) # 
  ) %>%
  mutate(age = as.numeric(sub("^([0-9]{1,2}).*", "\\1", age))) %>% # Makes the column numeric
  select(age,births,marriage) %>% 
  filter(
    age > 14 & age < 51,
    marriage != c("Live births - total", "Unknown") # Check that there are 0 unkowns
  )


# Main file for heterogeneiety
austria_main <- austria_births_hetero %>%
  inner_join(austria_females,by="age") %>% 
  mutate(
    ASFR = (births / exposure)*1000, #  make into a rate per 1000. 
    in_marriage = ifelse(marriage == "In marriage", 1, 0)) %>% 
  arrange(age) 

# let's examine TFR by marital status in Austria
austria_main_tfr <- austria_main %>%
  group_by(marriage) %>%
  summarise(TFR = sum(ASFR) / 1000)

print(austria_main_tfr) # Married people have more children

# By marriage status
austria_main_logit <- austria_main %>% 
  arrange(marriage, age) %>%
  group_by(in_marriage) %>%
  mutate(
    CFR = cumsum(ASFR),
    max_CFR = max(CFR),
    y = CFR / max_CFR,
    logit = log((1 - y) / y)
  ) %>% 
  filter(y > 0, y < 1) %>% 
  ungroup()


# Modelling logistic ---------------------------------------------------------------



# Plotting stratified model:
p1 <- ggplot(austria_main_logit, 
             aes(x = age, 
                 y = logit, 
                 color = marriage,     
                 group = marriage)) +  
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
    formula = y ~ x,  # Here we fit a regression model y(logit) ~ x(age)
    parse   = TRUE,
    label.x = "left",  # optional positioning tweaks
    label.y = "top"
  ) +
  labs(
    title = "Logit(CFR/TFR) by Age and Marital Status",
    x     = "Age",
    y     = "logit(CFR/TFR)",
    color = "Marital status"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p1)

cat("Stratified models (i.e. fitting separate models for each group) are useful when you believe the underlying process truly differs between categories. For example, modeling retirement behavior or health outcomes.

However, if your goal is to formally test whether those differences are statistically significant (i.e they differ from one another), you should fit a single model that includes both groups and add an interaction term. That way you can assess, in one framework, whether the effect of your predictor truly differs by group.")


# Interaction model:


# Fit a model

fit_model <- lm(logit ~ age * marriage, data = austria_main_logit)
summary(fit_model)


# Logistic model part 2 ---------------------------------------------------



# 1. Create a new dataframe with fitted values
austria_main_logit$predicted <- predict(fit_model)


austria_main_without_variable$predicted <- predict(fit_model)

# 3. Plot original data and regression line
p2 <- ggplot(austria_main_logit, aes(x = age, y = y, group = in_marriage)) +
  geom_point(color = "blue") +                  # Actual data points
  geom_line(aes(y = 1/(1+exp(predicted))), color = "red") +# Regression line
  labs(title = "Fit logitistic model",
       x = "Age",
       y = "Cumulative distribution") +
  theme_minimal()


print(p2)



