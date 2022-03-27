# Loading packages
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggfortify)
library(readr)
library(rcompanion)


# The wheat yield -------------------------------------------------------------------------------------------------------------------------------------------------

# Loading data
wheat <- read_csv("data/fertiliser_crop_data.csv")

# Question 1:
# Since I am interested in two variables which may impact the wheat yield either singly or in tandem, I am going to make use of a two-way ANOVA

# H0 (1): Fertiliser type does not influence wheat yield
# H0 (2): Planting density does not influence wheat yield
# H0 (3): The interaction of fertiliser type and planting density does not influence wheat yield

# Assumptions of ANOVA include:
# Normality
# Homoscedasticity
# Independence of data

aovwheat <- aov(mass ~ fertilizer * density, data = wheat)
summary(aovwheat)

# The p-value of fertilizer is 0.000273, which indicates that the types of fertilizer are associated with significantly different wheat yields.
# The p-value of density is 0.000186, which indicates that the levels of planting density are associated with significantly different wheat yields.
# The p-value for the interaction between fertilizer and density is 0.532500 (not significant), which indicates
# that the relationship between planting density and wheat yield does not depend on the type of fertiliser and vice versa.

# Plot results
tukey.plot.test1 <- TukeyHSD(aovwheat)
plot(tukey.plot.test1, "density")

# Test assumptions
# 1) Normality

plot(aovwheat, 2)
# Points fall approximately along reference line
# Data are normally distributed

# 2) Homoscedasticity

plot(aovwheat, 1)
# Reference line is approximately horizontal
# Data are approximately homoscedastic

# 3) Independence of data
# Data are not dependent upon one another
# Assumptions are met
# Null hypotheses 1 and 2 are rejected
# Null hypothesis 3 is accepted

# Therefore since fertiliser type and planting density both affect wheat yield,
# I am going to provide a summary of the data to determine the best fertiliser type and planting density

wheat_f <- wheat %>%
  group_by(fertilizer) %>%
  summarise(mean_mass = mean(mass))
wheat_f
# Fertiliser type C shows the highest accompanying wheat yield

wheat_d <- wheat %>%
  group_by(density) %>%
  summarise(mean_mass = mean(mass))
wheat_d
# Wheat planted at high density resulted in better yield than wheat planted at low density

# Question 2:

# Since I am interested in one variable which may impact the wheat yield, but which has four qualitative values
# Use of a single-factor ANOVA
# H0: Field positioning (block) does not influence wheat yield

# Assumptions of ANOVA include:
# Normality
# Homoscedasticity
# Independence of data

# Perform analysis

aovblock <- aov(mass ~ block, data = wheat)
summary(aovblock)
# The p-value of field positioning (block) is 0.00409, which indicates that the position of the wheat
# in either the north, south, east or west blocks of the field is associated with significantly different wheat yields.

# More precisely
# Use Tukey test
TukeyHSD(aovblock)

# The Tukey test shows that there is a significant difference in wheat yield between the
# south and east blocks of the field, but not in any other pairwise comparison of the blocks

# Plot results
tukey.plot.test2 <- TukeyHSD(aovblock)
plot(tukey.plot.test2, las = 1)

# Test assumptions
# 1) Normality

plot(aovblock, 2)
# Points fall approximately along reference line
# Data are normally distributed

# 2) Homoscedasticity

plot(aovblock, 1)
# Reference line is approximately horizontal
# Data are approximately homoscedastic


# 3) Independence of data
# Data are not dependent upon one another
# Assumptions are met
# Null hypotheses 1 and 2 are rejected
# Null hypothesis 3 is accepted

# So in conclusion, yes it does matter where you plant the wheat but only whether you plant it south or east.


# 2 The shells data -----------------------------------------------------------------------------------------------------------------------------------------------

# Load the data

shells <- read_csv("data/shells.csv")

# Question 3:
# Which species of mussel is the i) widest and ii) longest?

# I am going to provide a summary of the data to determine the widest and longest shell averages
# for the two mussel species

# i

t.test(left_length ~ species, data = shells)
# Choromytilus is longest
# assumption of equal vars: yes, vars more or less equal (because the var of the one is not more than
# 4 x the var of the other)
# assumption of normality
shapiro.test(shells$left_length[shells$species == "Aulacomya"])
shapiro.test(shells$left_length[shells$species == "Choromytilus"])
# not significantly different from normal

shellsw <- shells %>%
  group_by(species) %>%
  summarise(mean_width_left = mean(left_width),
            sd_width_left = sd(right_width))
shellsw
# Choromytilus species has the widest shells

# ii

t.test(left_width ~ species, data = shells)
# Choromytilus is widest
# assumption of equal vars: yes, vars more or less equal (because the var of the one is not more than
# 4 x the var of the other)
shapiro.test(shells$left_width[shells$species == "Aulacomya"])
shapiro.test(shells$left_width[shells$species == "Choromytilus"])
# not significantly different from normal

shellsl <- shells %>%
  group_by(species) %>%
  summarise(mean_length_left = mean(left_length),
            sd_length_right = sd(right_length))
shellsl
# Choromytilus species has the longest shells

# Question 4:

# Here we are interested in the correlation between the length and width of the left and right shells of two mussels species, Aulacomya sp. and Choromytilus sp
# For this I will perform a Pearson correlation test since data are continuous
# H0: For neither of the mussel species, i.e. Aulacomya sp. and Choromytilus sp,
# none of the four measurement variables, i.e. left_width, right_width, left_length or right_length,
# are correlated with one-another.

# Assumptions of correlation:
# pair-wise data
# absence of outliers
# linearity
# normality of distribution
# Continuous data (Pearson correlation)

# Testing assumptions

# 1) Pair-wise data
# Data are paired

# 2)
# There are no obvious outliers

# 3) Linearity

shells_grouped <- shells %>%
  group_by(species)

pairs(shells_grouped[,3:6], pch = 19,  cex = 0.5,
      col = c("red", "cornflowerblue"),
      upper.panel = NULL)
# Data look very linear

# 4) Normality

shells %>%
  pivot_longer(cols = left_length:right_width, names_to = "variable", values_to = "measurement") %>%
  group_by(species, variable) %>%
  summarise(p_value = round(as.numeric(shapiro.test(measurement)[2]), 3)) %>%
  head(5)
# P-values all exceed 0.05 therefore the data are normally distributed

# 5) Continuous data
# Data are continuous and normally distributed
# Therefore I am going to use a Pearson correlation

# Perform Pearson's correlation test
Aulacomya <- shells %>%
  filter(species == 'Aulacomya') %>%
  select(-ID, -species)

library(psych)
pair1 <- pairs.panels(Aulacomya, method = "pearson",
                      hist.col = "#00AFBB", density = TRUE,
                      ellipses = TRUE, stars = TRUE)
# Graphs show very strong positive correlations between all variables
# P-values are all strongly significant

Choromytilus <- shells %>%
  filter(species == 'Choromytilus') %>%
  select(-ID, -species)

pair2 <- pairs.panels(Choromytilus, method = "pearson",
                      hist.col = "#00AFBB", density = TRUE,
                      ellipses = TRUE, stars = TRUE)
# Graphs show very strong positive correlations between all variables
# P-values are all strongly significant

# Question 5:
# Considering Aulacomya sp. only, use a linear regression to predict the length of the left valve
# when the width of the left valve is 15 and 17 mm

# Using linear regression I will investigate the dependence of the length of the left valve upon the width thereof
# H0: The length of the left valve is not dependent upon its width

# Assumptions of linear regression
# Linearity
# Homoscedasticity
# Independence
# Normality

# Fit the linear model to the raw data
Amod <- lm(left_length ~ left_width, data = Aulacomya)
summary(Amod)
# For every 1.6886 mm increase in left valve width, there is an 8.0493 mm increase in left valve length
# The linear model accurately describes 72.29% of the data
# P-value is less than 0.05, therefore linear model accurately describes the data

# The predictions
pred <- data.frame(left_width = c(13, 15, 17))
pred

predict(Amod, pred)
# the left valve lengths are 30.00057 33.37769 36.75481

# Fit linear model to residuals
Aulacomya$resid <- Amod$residuals # create variable to contain residuals of Amod
Aulacomya$fitted <- Amod$fitted.values # create variable for fitted.values of Amod
Amod_resid <- lm(resid ~ left_width, data = Aulacomya) # fit model to residuals
Aulacomya$resid_resid <- Amod_resid$residuals # add residuals of Amod_resid
Aulacomya$resid_fitted <- Amod_resid$fitted.values # add fitted.values of Amod_resid
summary(Amod_resid)

# Plot fitted and residual values

Amod_plot <- ggplot(data = Aulacomya, aes(x = left_width, y = left_length)) +
  geom_point(shape = 1, colour = "cyan4") +
  geom_line(aes(y = fitted), colour = "cyan4", size = 0.6) +
  labs(title = "Aulacomya left valve",x = "Width (mm)", y = "Length (mm)")
Amod_plot

amod_resid_plot <- ggplot(data = Aulacomya, aes(x = left_width, y = resid_resid)) +
  geom_point(shape = 1, colour = "indianred3") +
  geom_line(aes(y = resid_fitted), colour = "indianred3", size = 0.6) +
  labs(title = "Aulacomya left valve",x = "Width (mm)", y = "Residuals of Length (mm) model, `Amod`")
amod_resid_plot

# Fitted line added to residual values

Aulacomya_sub <- Aulacomya %>%
  group_by(left_width) %>%
  sample_n(1) %>%
  ungroup()

ggplot(data = Aulacomya, aes(x = left_width)) +
  geom_point(aes(y = left_length), shape = 1, colour = "grey80") +
  geom_line(aes(y = fitted), colour = "cyan4", size = 0.6) +
  geom_point(aes(y = resid_resid), shape = 5, colour = "grey80") +
  geom_line(aes(y = resid_fitted), colour = "indianred3", size = 0.6) +
  geom_point(data = Aulacomya_sub, aes(y = left_length), shape = 1, colour = "cyan4", size = 3) +
  geom_point(data = Aulacomya_sub, aes(y = resid_resid), shape = 5, colour = "indianred3", size = 3) +
  geom_segment(data = Aulacomya_sub,
               aes(x = Aulacomya_sub$left_width, xend = Aulacomya_sub$left_width,
                   y = Aulacomya_sub$left_length, yend = Aulacomya_sub$resid),
               alpha = 0.4, arrow = arrow(), size = 1.0) +
  labs(x = "Width (mm)", y = "Length (mm)")


# Testing assumptions
autoplot(Amod, colour = "salmon", shape = 1, size = 0.2, ncol = 2, which = c(1:2)) + theme_pubr()

# Residuals vs Fitted: Relationship is approximately linear
# Normal Q-Q: Data are normally distributed

autoplot(Amod, colour = "salmon", shape = 1, size = 0.2, ncol = 2, which = c(3, 5)) + theme_pubr()

# Scale-Location: Residual data are aprroximately homoscedastic
# Residuals vs Leverage : There are outliers


# Data pass most of the linear regression assumptions
# Therefore, linear model is accepted
# Null hypothesis rejected
# To answer the question using amodplt:
# For every 1.6886 mm increase in left valve width, there is an 8.0493 mm increase in left valve length
# So to calculate the length corresponding to the width: (width/1.6886) * 8.0493 = length
# Therefore at 15mm width, the valve length would be 71.5mm
# At 17mm width, the valve length would be 81.04mm


# 3 The health data ------------------------------------------------------------------------------------------------------------------------------------------------

# Load the data

health <- read_delim("data/health.csv", ";", escape_double = FALSE,
                     trim_ws = TRUE)

# Question 6:

# We are interested in two variables which may impact the mental scores of people either singly or in tandem, I am going
# to make use of a two-way ANOVA
# H0 (1): Sex does not influence mental score
# H0 (2): The interaction of substance type and sex does not influence mental score

# Assumptions of ANOVA include:
# Normality
# Homoscedasticity
# Independence of data

# Perform analysis

aovhealth <- aov(Mental_score ~ Sex * Substance, data = health)
summary(aovhealth)
# The p-value of sex is  0.009181, which indicates that the genders are
# associated with significantly different mental scores.
# The p-value for the interaction between sex and substance is 0.236334 (not significant), which indicates
# that the relationship between sex and mental score does not depend on the type of substance and vice versa. -- i.e. males and females respond the same
# way to the various substances

# Therefore to answer the question: No, males and females do not suffer the same cognitive impairments if they abuse cocaine, alcohol or heroin

# Question 7:
# because there's no interaction, we can combine data for males and females, and simply ask which of the drugs is worse:

aovhealth2 <- aov(Mental_score ~ Substance, data = health)
plot(TukeyHSD(aovhealth2))

health %>%
  group_by(Substance) %>%
  summarise(mean_subs = mean(Mental_score))
# heroin is the worse, but only when compared to cocaine

# Test assumptions
# 1) Normality
plot(aovhealth2)

# Points fall approximately along reference line
# Data are normally distributed

# 3) Independence of data
# Data are not dependent upon one another
# Assumptions are met
# Null hypotheses are rejected


# 4 The air quality data ------------------------------------------------------------------------------------------------------------------------------------------
# Load the data
data(airquality, package = "datasets")
as.data.frame(airquality)

# Question 8:
# Quick pairwise comparison for rough idea of correlation

library(psych)
pair3 <- pairs.panels(airquality, method = "pearson",
                      hist.col = "#00AFBB", density = TRUE,
                      ellipses = TRUE, stars = TRUE)

# Ozone and temperature have the best correlation (0.70)

# Question 9:
# Provide a detailed correlation analysis for those two variables

# I am interested in the correlation between ozone levels and temperature
# H0: There is no correlation between ozone levels and temperature

# Testing assumptions
# 1) Pair-wise data
# Data are paired

# 2)
# There are no obvious outliers
air <- airquality %>%
  select(Ozone, Temp)

# 3) Linearity
ggplot(data = air, aes(x = Ozone, y = Temp)) +
  geom_point(shape = 1, colour = "red3") +
  labs(x = "Ozone (units)", y = "Temperature (F)") + theme_pubr()
# Plot shows approximate linear distribution

# 4) Normality
# 5) Homoscedasticity
mod <- lm(Temp ~ Ozone, data = air)
library(ggfortify)
autoplot(mod, colour = "salmon", shape = 1, size = 0.2, ncol = 2)

# double log transform...
mod2 <- lm(log(Temp) ~ log(Ozone), data = air)
autoplot(mod2, colour = "salmon", shape = 1, size = 0.2, ncol = 2)
# ... somewhat better w.r.t. normality and homoscedasticity...
# do correltion on the double log transformed data

ggplot(data = air, aes(x = log(Ozone), y = log(Temp))) +
  geom_point() +
  geom_smooth(method = "lm") + theme_pubr()
# Data are approximately homoscedastic

# 6) Continuous data
# Data are continuous and normally distributed
# Therefore I am going to use a Pearson correlation

# Perform Pearson's correlation test

with(air, cor(log(Ozone), log(Temp), use = "pairwise.complete.obs")) # "with" specifies dataset
# Correlation coefficient is 0.7
# This indicates a fairly strong positive correlation between Ozone levels and temperature

# Plot
air_log <- air %>%
  mutate(Ozone_log = log(Ozone),
         Temp_log = log(Temp))

air_pearson <- cor(air_log, use = "pairwise.complete.obs")

library(corrplot)
corrplot(air_pearson, method = "circle", type = "upper", # Show upper triangle
         number.digits = 2, addCoef.col = "red", tl.col = "black")


# 5 The crickets data ----------------------------------------------------------------------------------------------------------------------------------------------

# Load the data
crickets <- read_csv("data/crickets.csv")

# Converting temperatures to °C, (°F - 32) * (5/9) = °C
crick <- crickets %>%
  mutate(temp = ((temperature - 32) * (5/9))) %>%
  select(-temperature)

# Question 10:
# Does the chirp rate of the crickets depend on the temperature?
# To quantify this I am going to perform a simple linear regression.
# I am going to assume that the unit for chirps is chirps per minute
# H0: The chirp rate of crickets does not depend on temperature

# Assumptions of linear regression
# Linearity
# Homoscedasticity
# Independence
# Normality

# Fit the linear model to the raw data

cmod <- lm(chirps ~ temp, data = crick)

# For every 1°C increase in temperature, there is an 0.38146/minute increase in number of chirps
# The linear model accurately describes 67.42% of the data
# P-value is less than 0.05, therefore the relationship between chirp rate and temperature is significant
#
# The equation is:
# chirp_rate = 0.38146 * temperature + 6.47246

ggplot(data = crick, aes(x = temp, y = chirps)) +
  geom_point(shape = 1, colour = "grey20") +
  geom_smooth(method = "lm") +
  labs(x = "Temperature (°C)", y = "Chirps per minute")

# Testing assumptions

autoplot(cmod, colour = "salmon", shape = 1, size = 0.2, ncol = 2, which = c(1:2)) + theme_pubr()
# Residuals vs Fitted: Relationship is notlinear
# Normal Q-Q: Data are normally distributed

autoplot(cmod, colour = "salmon", shape = 1, size = 0.2, ncol = 2, which = c(3, 5)) + theme_pubr()
# Scale-Location: Residual data are aprroximately homoscedastic
# Residuals vs Leverage : There are outliers
# Data pass most of the linear regression assumptions
# Therefore, linear model is accepted
# Null hypothesis rejected

# Question 11:
# For every 0.38146°C increase in temperature, there is an 6.47246/minute increase in number of chirps
# Therefore the equation would be the following:
# y = 0.38*x + 6.47


# 6 The SST data --------------------------------------------------------------------------------------------------------------------------------------------------

# Load the data
sst <- read_csv("data/SST.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))

# Question 12:
# Do the temperatures differ between the two places?

# To determine this I am going to perform a two-sample t-test, since we have two sets of samples
# H0: The temperatures do not differ between Port Nolloth and Muizenberg

t.test(sst$Port_Nolloth, sst$Muizenberg)
# yes...

# First I have to manipulate the data slighlty
sst1 <- sst %>%
  gather(`Port_Nolloth`, `Muizenberg`, key = "site", value = "temp")

# Assumptions of a t-test:
# the dependent variable must be continuous
# the observations in the groups being compared are independent of each other
# the data are normally distributed
# that the data are homoscedastic, and there are no outliers

# Testing assumptions
# 1) The dependent variable must be continuous
# Temperature is continuous

# 2) The observations in the groups being compared are independent of each other
# The observations are independent

# 3) The data are normally distributed
# Use Shapiro-Wilk test

sst1 %>%
  group_by(site) %>%
  summarise(norm_dat = as.numeric(shapiro.test(temp)[2]))
# P-values are less than 0.05 therefore data are not normally distributed

# 4) Homoscedastic

sst1 %>%
  group_by(site) %>%
  summarise(sample_var = var(temp, use="pairwise.complete.obs"))
# One value is not in a 2-4 times range of the other therefore the data are not homoscedastic
# Because the assumptions have failed I need to use the Wilcoxon Rank Sum test

wilcox.test(temp ~ site, data = sst1,
            exact = FALSE)

# P-value is less than 0.05
# therefore there is a significant difference between the temperatures at Port Nolloth and Muizenberg

# Question 13:
# For each of the two sites, which month has the i) lowest and ii) highest temperature?

library(lubridate)
sst2 <- sst1 %>%
  mutate(month = month(date)) %>%
  group_by(site, month) %>%
  summarise(temp_mean = mean(temp, na.rm = TRUE))

ggplot(sst2, aes(x = month, y = temp_mean)) +
  geom_line(aes(colour = site))

# Port Nolloth:
# i: July
# ii: March

# Muizenberg:
# i: July
# ii: February

# Question 14:
# For each of the two sites, is the winter temperature colder than the summer temperature?
# summer: dec, jan, feb
# winter: june, jul, aug

sst_summer <- sst1 %>%
  mutate(month = month(date)) %>%
  group_by(site, month) %>%
  ungroup() %>%
  filter(month %in% c(12, 1, 2)) %>%
  mutate(seas = "sum")
sst_winter <- sst1 %>%
  mutate(month = month(date)) %>%
  group_by(site, month) %>%
  ungroup() %>%
  filter(month %in% c(6, 7, 8)) %>%
  mutate(seas = "win")

sst_seas <- rbind(sst_summer, sst_winter)

# are seasons different at Port Nolloth?
t.test(sst_seas[sst_seas$site == "Port_Nolloth", ]$temp ~ sst_seas[sst_seas$site == "Port_Nolloth", ]$seas)
# yes...

# are seasons different at Port Nolloth?
t.test(sst_seas[sst_seas$site == "Muizenberg", ]$temp ~ sst_seas[sst_seas$site == "Muizenberg", ]$seas)
# yes...

# but are the data normally distributed?
ggplot(data = sst_seas, aes(x = temp)) +
  geom_histogram(aes(colour = seas)) +
  facet_wrap(vars(site), ncol = 2)
# it is a tiny bit skewed, but it's okay...

# if some other test showed significant non-normal data, then use the wilcoxon test... (e.g. your shapiro test below)
wilcox.test(sst_seas[sst_seas$site == "Port_Nolloth", ]$temp ~ sst_seas[sst_seas$site == "Port_Nolloth", ]$seas)
wilcox.test(sst_seas[sst_seas$site == "Muizenberg", ]$temp ~ sst_seas[sst_seas$site == "Muizenberg", ]$seas)
# both also significantly different

shapiro.test(sstsummer$mean_temp)
# P-value is greater than 0.05, therefore data are normally distributed
# Therefore I can use the traditional approach instead of bootstrapping

shapiro.test(sstwinter$mean_temp)
# P-value is greater than 0.05, therefore data are normally distributed
# Therefore I can use the traditional approach instead of bootstrapping

# Question 15:
# Same as Question 14, but use 95% confidence intervals to approach this problem (and provide the supporting graphs).

sst_seas2 <- na.omit(sst_seas)
PN_seas <- groupwiseMean(temp ~ seas, data = sst_seas2[sst_seas2$site == "Port_Nolloth", ], conf = 0.95, digits = 3)
ggplot(data = PN_seas, aes(x = seas)) +
  geom_segment(aes(x = seas, xend = seas, y = Trad.lower, yend = Trad.upper), size = 1) +
  geom_point(aes(y = Mean), colour = "red3", size = 2, shape = 2) + ylab("Mean ± 95% CI")

Mu_seas <- groupwiseMean(temp ~ seas, data = sst_seas2[sst_seas2$site == "Muizenberg", ], conf = 0.95, digits = 3)
ggplot(data = Mu_seas, aes(x = seas)) +
  geom_segment(aes(x = seas, xend = seas, y = Trad.lower, yend = Trad.upper), size = 1) +
  geom_point(aes(y = Mean), colour = "red3", size = 2, shape = 2) + ylab("Mean ± 95% CI")

# Yes and yes
