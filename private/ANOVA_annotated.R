# Stuff here...

# Load library
library(tidyverse)

chicks <- ChickWeight

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)

# The ANOVA testing for differences in chicken mass after 21 days of exposure to four diets indicate that diet has a significant effect on growth rate (d.f. = 3, F = 4.7, p < 0.05).

# Tukey post-hoc analysis
TukeyHSD(chicks.aov1)

plot(TukeyHSD(chicks.aov1))

# Analysis of co-variance
# This treats time as a continous co-variate

ggplot(data = chicks, aes(x = Time, y = weight)) +
  geom_point() +
  geom_smooth(method = "lm")

chicks$Diet <- as.factor(chicks$Diet)

chicks.aov2 <- aov(weight ~ Diet * Time, data = chicks)
summary(chicks.aov2)

ggplot(chicks, aes(x = Time, y = weight, group = Diet)) +
  geom_point(aes(col = Diet)) +
  # geom_line(aes(col = Diet, group = Chick)) +
  geom_smooth(method = "lm", aes(col = Diet, group = Diet))


## The easy way
# Research question: does diet have an effect on chicken mass as time progresses?
# H0: The different diets will not have an effect on chicken mass as time progresses.
# H0: The different diets will have an effect on chicken mass as time progresses.
#
# So, we require one cleverly written ANOVA model for the analysis.

str(chicks)
chicks$Time <- as.factor(chicks$Time)

days <- c(0, 6, 12, 18)

chicks_sub <- chicks %>%
  filter(Time %in% days)

aov2 <- aov(weight ~ Diet + Time, data = chicks_sub)
summary(aov2)

# A figure showing the effect of the fctor called "Diet"

ggplot(data = chicks_sub, aes(x = Diet, y = weight)) +
  geom_boxplot()

# Okay, but which diet results in the heaviest chicks? So, do a TukeyHSD:
TukeyHSD(aov2, which = "Diet")

# Okay, how does time have an influence on the chicks' mass?
ggplot(data = chicks_sub, aes(x = Time, y = weight)) +
  geom_boxplot() +
  geom_point()

TukeyHSD(aov2, which = "Time")
plot(TukeyHSD(aov2, which = "Time"))

aov3 <- aov(weight ~ Diet * Time, data = chicks_sub)
summary(aov3)

TukeyHSD(aov3)
