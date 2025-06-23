# Class 11: Leslie Matrix Population Projection
# 2025S 234001-1 Applied Methods of Demographic Analysis (Tutorial)
# In this script, we will:
# - Retrieve period lifetables and fertility rates from the HMD/HFD
# - Construct Leslie matrices for females and males
# - Compute intrinsic growth rates and stable age distributions
# - Project populations under baseline and skewed initial structures
# - Compare scenarios (Baseline, T-shaped, Y-shaped)


rm(list=ls())

# Preamble ----------------------------------------------------------------


library(HMDHFDplus)
library(demography)   
library(MASS)
library(ggplot2)

# Username and password
username = "marcus.immonen.hagley@gmail.com"
password = "Google2004!"


# Parameters

pop <- "USA"
year <- 2019
periods <- 50 # Set the number of years to forecast
open_age <- 85
radix <- 100000



# Functions ---------------------------------------------------------------

project_pop <- function(KF0, KM0, Lf, Lm, periods){
  # 1) get required vector‐length from the Leslie
  nages <- nrow(Lf)
  
  
  # 3) storage
  popF <- matrix(0, nages, periods)
  popM <- matrix(0, nages, periods)
  popF[,1] <- KF0
  popM[,1] <- KM0
  
  # 4) projection loop
  for (t in seq_len(periods-1)) { # Here I loop which functions cannot do. 
    popF[,t+1] <- Lf %*% popF[,t]
    popM[,t+1] <- Lm %*% popM[,t]
  }
  
  list(female = popF, male = popM) # returns the list
} # Here we require initial pop of females, males, Leslie matrix for males and females and lastly how many times we will do it. 



# Data processing ---------------------------------------------------------

# Getting mortality structure

lifetable_female <- readHMDweb(pop,
                               "fltper_1x1", 
                               username=username,
                               password=password)

lifetable_male <- readHMDweb(pop,
                             "mltper_1x1",
                             username=username, 
                             password=password)

lifetable_female <- lifetable_female %>% 
  dplyr::filter(Year == year)


lifetable_male   <- lifetable_male %>% 
  dplyr::filter(Year == year)

lx_Female <- lifetable_female$lx / lifetable_female$lx[1]
lx_Male <- lifetable_male$lx / lifetable_male$lx[1]

ages   <- 0:max(lifetable_female$Age)
num_ages    <- length(ages)
n      <- 1
ax_male    <- lifetable_female$ax
ax_female    <- lifetable_female$ax


# Fertility structure

fert <- readHFDweb("USA",  
                   "asfrRR",
                   username=username, 
                   password=password)
fert <- subset(fert, Year == year)

Fx <- rep(0, num_ages)
idx <- which(ages >= min(fert$Age) & ages <= max(fert$Age))
Fx[idx] <- fert$ASFR

ffab <- 0.4886 # Fraction of females at birth

# Computing person-years lived

Lx_F <- c((lx_Female[-num_ages] + lx_Female[-1]) / 2, lx_Female[num_ages]/2)
Lx_M <- c((lx_Male[-num_ages] + lx_Male[-1]) / 2, lx_Male[num_ages]/2)



# Building the leslie matrix ----------------------------------------------
cat("As we know, the Leslie matrix places age‐specific births in the first row\n",
    "and then fills the sub‐diagonal with survivorship probabilities\n",
    "that advance each age cohort to the next age class.\n\n")

Leslie_F <- matrix(0, num_ages, num_ages)
Leslie_M <- matrix(0, num_ages, num_ages)

surv_F <- Lx_F[-1] / Lx_F[-num_ages]
surv_M <- Lx_M[-1] / Lx_M[-num_ages]
for(i in 1:(num_ages-1)){
  Leslie_F[i+1,i] <- surv_F[i]
  Leslie_M[i+1,i] <- surv_M[i]
}

# Fraction of females at birth - Fraction of males is 1-ffab
births_F <- (Lx_F[1] / lx_Female[1]) * Fx * ffab
births_M <- (Lx_M[1] / lx_Male[1])   * Fx * (1 - ffab)

Leslie_F[1, ] <- births_F
Leslie_M[1, ] <- births_M


# Decomposition and stable rate -------------------------------------------

Decomposition <- eigen(Leslie_F)
r   <- log(Re(Decomposition$values[1])) / n
cat("Lotka's intrinsic growth rate r =", round(r,4), "\n")

stable_F <- Re(Decomposition$vectors[,1])

stable_F <- stable_F / sum(stable_F)


# Let's examine the stable pop:

dfF <- data.frame(
  age  = ages,
  dens = stable_F
)

ggplot(dfF, aes(x = age, y = dens)) +
  geom_line(size = 1, color = "steelblue") +
  geom_area(fill = "steelblue", alpha = 0.3) +
  labs(
    x     = "Age (years)",
    y     = "Density",
    title = paste0(pop, " (", year, ") stable age distribution density")
  ) +
  theme_minimal()



#  Projection of sexes ----------------------------------------------------

K0_F <- 100000 * lx_Female
K0_M <- 100000 * lx_Male



popF <- matrix(0, num_ages, periods)
popM <- popF
popF[,1] <- K0_F; popM[,1] <- K0_M

for(t in 1:(periods-1)){
  popF[,t+1] <- Leslie_F %*% popF[,t]
  popM[,t+1] <- Leslie_M %*% popM[,t]
}

df_fin <- data.frame(
  age    = ages,
  female = popF[,periods],
  male   = popM[,periods]
)

ggplot(df_fin, aes(x=age)) +
  geom_col(aes(y=female), fill="red",   alpha=0.6) +
  geom_col(aes(y=-male),   fill="blue",  alpha=0.6) +
  scale_y_continuous(labels=abs) +
  labs(
    x     = "Age",
    y     = "Population",
    title = paste0("Projected pyramid after ",periods," years")
  ) +
  coord_flip() +
  theme_minimal()

# What happens if we start of with a skewd distribution? And a sudden change of
# mortality and fertility rates?

# Let's change the Us structure to be heavy on the end (T-shaped) or Young


# Scenario ----------------------------------------------------------------

# Let's use the USA but alter their initial structure

skew_factor_T <- (ages + 1) / mean(ages + 1)       # “T-shaped”: more old
skew_factor_Y <- rev(skew_factor_T)                # “Y-shaped”: more young

K0_F_T <- K0_F * skew_factor_T / mean(skew_factor_T)
K0_M_T <- K0_M * skew_factor_T / mean(skew_factor_T)
K0_F_Y <- K0_F * skew_factor_Y / mean(skew_factor_Y)
K0_M_Y <- K0_M * skew_factor_Y / mean(skew_factor_Y)


num_ages <- length(lx_Female) 


Leslie_F <- matrix(0, num_ages, num_ages)
Leslie_M <- matrix(0, num_ages, num_ages)

# 2) fill the subdiagonal with survivorship
surv_F <- Lx_F[-1] / Lx_F[-num_ages]
surv_M <- Lx_M[-1] / Lx_M[-num_ages]
for(i in 1:(num_ages-1)){
  Leslie_F[i+1, i] <- surv_F[i]
  Leslie_M[i+1, i] <- surv_M[i]
}

# 3) compute births-to-females & births-to-males
#    (use lx_Female etc. if that’s what you named your lx vectors)
b_F <- (Lx_F[1] / lx_Female[1]) * Fx * ffab
b_M <- (Lx_M[1] / lx_Male[1])   * Fx * (1 - ffab)

# 4) fill the first row
Leslie_F[1, ] <- b_F
Leslie_M[1, ] <- b_M



# Set period
periods <- 50

# Run code
out_base <- project_pop(K0_F,   K0_M,   Leslie_F, Leslie_M, periods)
out_T    <- project_pop(K0_F_T, K0_M_T, Leslie_F, Leslie_M, periods)
out_Y    <- project_pop(K0_F_Y, K0_M_Y, Leslie_F, Leslie_M, periods)

# 4) assemble for plotting total pop at final time
df_scen <- rbind(
  data.frame(age=ages,
             pop=out_base$female[,periods] + out_base$male[,periods],
             scenario="Baseline"),
  data.frame(age=ages,
             pop=out_T$female[,periods]    + out_T$male[,periods],
             scenario="T-shaped"),
  data.frame(age=ages,
             pop=out_Y$female[,periods]    + out_Y$male[,periods],
             scenario="Y-shaped")
)

ggplot(df_scen, aes(x=age, y=pop, color=scenario)) +
  geom_line(size=1) +
  labs(
    x = "Age",
    y = "Total population",
    title = paste0("Population by age after ", periods, " years")
  ) +
  theme_minimal()



# Exercise ----------------------------------------------------------------

cat(
  "Exercises:
1. Let's say there is a government that wants you to decide whether they should aim for policies that can increases higher fertility in lower ages or higher ages. Which one should they chose? The policies can affect a 5-year age group by 10%
2. Extend projections to 50,100,150,250,500 years. How does the stable age structure evolve?
3. Compare male vs female stable distributions. What factors drive the patterns?
4. Discuss how skewed initial age structures affect long-term population composition.")

