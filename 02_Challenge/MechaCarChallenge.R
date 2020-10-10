# 1. MPG Regression
# Import datasets
MechaCar <- read.csv(file='MechaCar_mpg.csv',header=TRUE,sep=',')

# mpg multiple linear regression
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=MechaCar)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=MechaCar))

# 2. Suspension Coil Summary
# Import datasets
Suspension <- read.csv(file='Suspension_Coil.csv', header=TRUE, sep=',')

# Create a summary ststistics table for the suspension coil
Suspension %>% 
  summarize(PSI_mean = mean(PSI), PSI_median = median(PSI), PSI_variance = var(PSI), PSI_sd = sd(PSI))

# Create a summary statistics table for the suspension coil’s pounds-per-inch continuous variable
Suspension %>% 
  group_by(Manufacturing_Lot) %>%
  summarize(PSI_mean = mean(PSI), PSI_median = median(PSI), PSI_variance = var(PSI), PSI_sd = sd(PSI))

#3. Suspension Coil T-Test
# Generate random 60 samples
sample_table1 <- Suspension %>% sample_n(60)
sample_table2 <- Suspension %>% sample_n(60)

# t-test for two samples
t.test(sample_table1$PSI, sample_table2$PSI)

# Determine if the suspension coil’s PSI results are statistically different from the mean population PSI results of 1,500.
t.test(Suspension$PSI, mu = 1500)

t.test(subset(Suspension, Manufacturing_Lot == "Lot1")$PSI, mu = 1500)

t.test(subset(Suspension, Manufacturing_Lot == "Lot2")$PSI, mu = 1500)

t.test(subset(Suspension, Manufacturing_Lot == "Lot3")$PSI, mu = 1500)

# 4. Design Your Own Study
# Use module mtcars dataset to compare cylinder (cyl) vs. horsepower (hp)
# Create linear regression model
lm(cyl ~ hp, data = mtcars)
summary(lm(cyl ~ hp, data = mtcars))

