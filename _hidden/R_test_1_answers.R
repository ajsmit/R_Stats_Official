# Mock Exam memo
# The steps taken are:
# 1: Setup environment
# 2: Question 1
# 3: Question 2
# 4: Question 3
# 5: Question 4
# 6: Question 5
# 7: Question 6

# 1: Setup environment---------------------------------------------------------------------------------------------------------------------------------------------

# Loading libraries
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ggfortify)
library(corrplot)
library(car)
library(rcompanion)


# Question 1 -----------------------------------------------------------------------------------------------------------------------------------------------------

# Loading data
library(coin)
data(mercuryfish)

# View the dataset in order to understand the data and observe if these variables are dependant on each other
head(mercuryfish)
tail(mercuryfish)
view(mercuryfish)

# Using a t-test demonstrate the difference in proportion of abnormal cells between two populations (`control` and `exposed`. Here we focus on the patients who were exposed to a high mercury intake and patients who were did not consume large amounts of mercury.
# Given this, a two-sided two-sample t-test will be used. Why? Because we test two groups of independent samples against each other, and we are not concerned about if the mean of the one group is larger or smaller than that of the other.

# NOTE: There are many other comparisons possible in this dataset; the one here is just one example. If you have questions about your own analysis, please ask me.

# State the assumptions of the t-test:
# - The data are normally distributed and the dependant variable is continuous
# - The groups being compared are independent of each other
# - The data are homoscedastic with no outliers

# Test for equal normality:
mercuryfish %>%
  group_by(group) %>%
  summarise(norm_dat = as.numeric(shapiro.test(abnormal)[2]))
# - The p-value is > 0.05, so the data are normally distributed.

# Test for equality of variances:
mercuryfish %>%
  group_by(group) %>%
  summarise(var_dat = var(abnormal))
# - The variance of our two samples are homoscedastic because the variance of one is not more than two to four times greater than the other.

# - The `abnormal` variable in the mercury dataset is continuous, and the groups are not dependant on each other.
# - Perform two-sample two-sided t-test to test the H0 that the proportion of cells with structural abnormalities does not differ between the `control` and `exposed` groups.

t.test(abnormal ~ group, data = mercuryfish, var.equal = TRUE)

# - The p-value = 0.005253 (< than 0.05)
# - Therefore there is a significant difference of abnormal cells between the control group and exposed group. Hence we reject the null hypothesis.


# Question 2 ------------------------------------------------------------------------------------------------------------------------------------------------------

# This is a challenging analysis. Before I started I compared individually which of the continuous variables show more-or-less linear associations with each other (not shown here). You could have used pairwise correlations for this. I proceeded with `abnormal` vs. `ccells` as these show the best linear associations.

# H0: There is no relationship between the proportion of cells with structural abnormalities with cells with asymmetrical or incomplete-symmetrical chromosome aberrations. This is true for both the treatment and control groups.

# Check that the data (abnormalities vs ccells) show are more-or-less linear relationship with each other:
ggplot(data = mercuryfish, aes(x = ccells, y = abnormal)) +
  geom_point(aes(colour = group))
# - Okay, the relationships can be linear...

# Lets see what the histograms tell us:
ggplot(data = mercuryfish, aes(x = abnormal)) +
  geom_density(alpha = .2, fill = "salmon") +
  geom_histogram(fill = "white", colour = "black", binwidth = 2) +
  facet_wrap(facets = vars(group), nrow = 1)

ggplot(data = mercuryfish, aes(x = ccells)) +
  geom_density(alpha = .2, fill = "salmon") +
  geom_histogram(fill = "white", colour = "black", binwidth = 2) +
  facet_wrap(facets = vars(group), nrow = 1)
# - These data do not seem to be normally distributed, but let's proceed regardless and test the assumptions afterwards.

# # With Tukey's method, outliers are below (1st Quartile) or above (3rd Quartile) 1.5 times the Inter-Quartile Range (IQR):
# # First, find the 25th and the 75th percentiles:
# qt <- quantile(mercuryfish$ccells, probs = c(.25, .75), na.rm = FALSE)
#
# # find the interquartile range is the difference between the 25th and the 75th percentiles:
# iqr <- IQR(mercuryfish$ccells)
#
# # apply Tukey's method:
# upper_cut <- qt[2] + (1.5 * iqr)
# lower_cut <- qt[1] - (1.5 * iqr)
#
# # filter the data:
# mercuryfish <- mercuryfish %>%
#   filter(ccells < upper_cut & ccells > lower_cut)

# Separate analyses for each group:
control <- mercuryfish %>%
  filter(group == "control")

exposed <- mercuryfish %>%
  filter(group == "exposed")

mod_control <- lm(abnormal ~ ccells, data = control)
autoplot(mod_control, colour = "salmon", shape = 1, size = 0.2, ncol = 2, which = c(1, 2, 3, 5))

mod_exposed <- lm(abnormal ~ mercury, exposed)
autoplot(mod_exposed, colour = "salmon", shape = 1, size = 0.2, ncol = 2, which = c(1, 2, 3, 5))

# The assumption tests are far from perfect, so some kind of data transformation would be needed, or we can apply a non-linear model or a GLM, neither of which I expect you to have the skills for just yet (not that it is beyond your means, though). In spite of these shortcmings, lets explain the findings and create graphs.

summary(mod_control)
# Adjusted R-squared: 0.3527, p-value for the slope: < 0.05

summary(mod_exposed)
# Adjusted R-squared: ~0, p-value for the slope: > 0.05

# Both R-squared values also indicate weak associations with the exposed group being the weaker of the two

ggplot(data = mercuryfish, aes(x = ccells, y = abnormal)) +
  geom_point(aes(colour = group)) +
  geom_smooth(aes(colour = group), method = "lm")

# - Overall, only the linear model fitted to the `exposed` group of data show a significant trend (r-adj = 0.3527, p < 0.05) trend in proportion of cells with structural abnormalities as a function of cells with asymmetrical or incomplete-symmetrical chromosome aberrations. This relationship does not hold true for the data belonging to the control group (p > 0.05). This is expected, as this is the kind of response predicted for cells that had been exposed the the carcinogen, mercury.


# Question 3 ---------------------------------------------------------------------------------------------------------------------------------------------------

# Loading data
data(glioma, package = "coin")

head(glioma)
tail(glioma)
summary(glioma)

# Do sex and group interact to affect survival time (time)?
# - Perform a multiple factor analysis ( ANOVA: two-way analysis of variance)
# - H0: Sex and group do not interact to affect survival time of glioma patients

# The below analysis proceeds with a 2-way ANOVA:
aov.1 <- aov(time ~ sex * group, data = glioma)
summary(aov.1)
# - The interaction of sex and group variables is not significant (p-value > than 0.05). Hence, only the `group` to which the patients belongs affect the survival time.

# The assumptions of an ANOVA:
# - Normality
# - Independence of data
# - Homoscedasticity

op <- par()
par(mfrow = c(2, 2))
plot(aov.1, 2) # see the Q-Q plot
# ...points fall approximately along reference line
# ...data are normally distributed
hist(residuals(aov.1), col = "red")
# ...residuals seem more-or-less normally distributed
plot(fitted(aov.1), residuals(aov.1), col = "red")
# ...variances seem okay (even) -- can confirm with Levene test, below
par(op)

# Also, can use Levene's test to test homoscedasticity
leveneTest(time ~ sex * group, data = glioma)
# p.value is greater than 0.05
# Data are homoscedastic

# A graphical summary of the results:
ggplot(data = glioma, aes(x = group, y = time)) +
  geom_boxplot(aes(colour = sex)) +
  ylab("Survival time (months)") + xlab("Treatment group")
# NOTE: that the data are not 100% normally distributed if viewed in groups (group x sex), but for the overall test, the assumptions are met according to the above test for assumptions applied the residuals after fitting the ANOVA model -- especially females belonging to the control group are very much skewed to the right.


# Question 4 ------------------------------------------------------------------------------------------------------------------------------------------------

# Do age and histology interact to affect survival time (time)?

# We use an ANCOVA for this, because we have 1 continuous covariate and 1 categorical covariate.

# H0: age and histology do not interact to affect survival time (time).

# Do the analysis and show the graphical summary:
aov.2 <- aov(time ~ histology * age, data = glioma)
summary(aov.2)
# No, there is not interaction, so the survival time does not depend on how old the individual is; survival time depends only on whether the individual belonged to histological groupings, `GBM` or `Grade3`. This can be seen in the figure below: the survival time remains constant with age for both the `GBM` and `Grade2` groups (as you can see by the wide 95% CI bands), but the mean survival time for the `Grade3` group is significantly higher (p < 0.05) for `Grade3`.

ggplot(data = glioma, aes(x = age, y = time)) +
  geom_point(aes(colour = histology)) +
  geom_smooth(aes(colour = histology), method = "lm")

# Becasue age has no effect, it might be better to plot the data as follows:
ggplot(data = glioma, aes(x = histology, y = time)) +
  geom_boxplot(aes(colour = histology))

# What about the assumptions?
op <- par()
par(mfrow = c(2, 2))
plot(aov.2, 2) # see the Q-Q plot
# ...points fall approximately along reference line
# ...data are normally distributed
hist(residuals(aov.2), col = "red")
# ...residuals seem more-or-less normally distributed
plot(fitted(aov.2), residuals(aov.2), col = "red")
# ...variances seem okay (even) -- can confirm with Levene test, below
par(op)
# Overall, not perfect, but seemingly good enough as far as assumptions go.

# NOTE: if you didn't want to do an ANCOVA, you could do an ANOVA as follows
# aov.3 <- aov(time ~ histology * as.factor(age), data = glioma)
# summary(aov.3) # ... the findings are more-or-less similar, but it is better to treat `age` as the continuous covariate that it is


# Question 5 -------------------------------------------------------------------------------------------------------------------------------------------------------

# Loading the data

data(ToothGrowth, package = "datasets")
Teeth <- as_tibble(ToothGrowth)

# - Here are two independent variables, both of which may impact tooth growth. Therefore, I make use of a two-way ANOVA.
# - H0 (1): Dose size does not influence tooth length
# - H0 (2): Supplements does not influence tooth length
# - H0 (3): The interaction of dose size and supplements does not influence tooth length

## Assumptions of ANOVA (as demonstrated in Q3 and Q4)
# - Normality
# - Independence of data
# - Homoscedasticity

aov.4 <- aov(len ~ supp * as.factor(dose), data = ToothGrowth)
summary(aov.4)
# - The p-value of supplement is < 0.05, therefore the levels of supplement are associated with significantly different tooth length.
# - The p-value of dose is < 0.05, and this indicates that the levels of dose are associated with significantly different tooth length.
# - There is a significant interaction term (p < 0.05), which is because of the fact that at the two lowest doses (0.5 and 1 mg/day), the form that the Vit C is supplied in makes a difference -- tooth growth is greater when it is supplied as Orange Juice. However, this response is not consistent across doses, as at the highest dose it makes no difference on whether Vit C is supplied as Orange Juice or pure Ascorbic Acid.

# An ANCOVA will be better still:

aov.5 <- aov(len ~ supp * dose, data = ToothGrowth)
summary(aov.5)
# The findings are the same in this instance.

ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len)) +
  geom_boxplot(aes(colour = supp))

# ... or if you had done an ANCOVA, which would have been better, you plot it as follows:
ggplot(data = ToothGrowth, aes(x = dose, y = len)) +
  geom_point(aes(colour = supp)) +
  geom_smooth(aes(colour = supp), method = "lm")

# And the assumptions?
op <- par()
par(mfrow = c(2, 2))
plot(aov.5, 2) # see the Q-Q plot
# ...points fall approximately along reference line
# ...data are normally distributed
hist(residuals(aov.5), col = "red")
# ...residuals seem more-or-less normally distributed
plot(fitted(aov.5), residuals(aov.5), col = "red")
# ...variances seem okay (even) -- can confirm with Levene test, below
par(op)
# It seems goo enough for me.


# Question 6 -------------------------------------------------------------------------------------------------------------------------------------------------------

# Load data
data(birthwt, package = "MASS")

# View Data
head(birthwt)
tail(birthwt)
summary(birthwt)

# H0(1): There is no correlation between the mothers' ages and the birth mass of infants (Pearson correlation)
# H0(2): The mean mass of infants with a smoking mother is not significantly different from those with a non-smoking mother (95% confidence interval around the mean)
# H0(3): Uterine irritability does not cause a significant difference in mean infant mass (two-sided two-sample t-test)

# H0 (1)
# - Is there a linear relationship?
ggplot(data = birthwt, aes(x = age, y = bwt)) +
  geom_point() +
  geom_smooth(method = "lm")
# - Well, yes, but far from perfect. Nevertheless, even a horizontal line can be linear, which seems to be the case here. Probably the correlation will not be significant.

# - Do the samples follow independent normal distributions?
shapiro.test(birthwt$age) # not really, no
hist(birthwt$age) # it is skewed to the right

shapiro.test(birthwt$bwt) # yes
hist(birthwt$bwt) # seems normal(-ish)

# - But one of them is not normal, so let us do a Spearman's rho Rank correlation
cor.test(birthwt$bwt, birthwt$age, method = "spearman", exact = FALSE)
# - No correlation as expected

# H0 (2)
mn_smoke <- groupwiseMean(bwt ~ smoke, data = birthwt, conf = 0.95, digits = 3)
mn_smoke
# Mean infant birth mass for non-smoking mother: 2920-3190
# Mean infant birth mass forsmoking mother: 2620-2920

ggplot(mn_smoke, aes(x = as.factor(smoke), y = Mean)) +
  geom_segment(aes(x = as.factor(smoke), xend = as.factor(smoke), y = Trad.lower, yend = Trad.upper), size = 1) +
  geom_point(aes(y = Mean), colour = "red3", size = 4, shape = 2) + ylab("Mean ± 95% CI") + xlab("Smoking?")
# - Yes, there is a significant difference in infant birth mass between smoking and non-smoking mothers.

# - Do the last one yourself.
