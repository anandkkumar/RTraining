####----
# Statistical Inference via Data Science: A ModernDive into R and the Tidyverse.
# Based on content published on https://moderndive.com/ and https://www.tidymodels.org/books/moderndive/ and

####----

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(skimr)
library(infer)
library(broom)
library(janitor)

library(nycflights13)
library(fivethirtyeight)
library(moderndive)
library(gapminder)
library(ISLR)
library(ggplot2movies)

alaska_flights <- flights %>%
  filter(carrier == "AS")

ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point()

ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  geom_point(alpha = 0.2)

ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) +
  # width & height correspond to how hard you’d like to shake the plot in horizontal x-axis units and vertical y-axis units, respectively
  geom_jitter(width = 30, height = 30)


early_january_weather <- weather %>%
  filter(origin == "EWR" & month == 1 & day <=15)

# It is preferred to use linegraphs over scatterplots when the variable on the x-axis (i.e., the explanatory variable) has an inherent ordering, such as some notion of time.
ggplot(data = early_january_weather, mapping = aes(x = time_hour, y = temp)) +
  geom_line()


ggplot(data = weather, mapping = aes(x = temp)) +
  # run colors() to see all 657 possible choice of colors in R
  geom_histogram(color = "white", fill = "steelblue")

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(bins = 40, color = "white")

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 10, color = "white")

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ month, nrow = 4)

# Boxplots require a categorical variable to be mapped to the x-position aesthetic
ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()


fruits <- tibble(
  fruit = c("apple", "apple", "orange", "apple", "orange")
)
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2)
)
# Both histograms and boxplots are tools to visualize the distribution of numerical variables.
# Another commonly desired task is to visualize the distribution of a categorical variable.
# This is a simpler task, as we are simply counting different categories within a categorical variable,
# also known as the levels of the categorical variable

# When the categorical variable whose distribution you want to visualize
#
# Is not pre-counted in your data frame, we use geom_bar().
# Is pre-counted in your data frame, we use geom_col() with the y-position aesthetic mapped to the variable that has the counts.

ggplot(data = fruits, mapping = aes(x = fruit)) +
  geom_bar()
ggplot(data = fruits_counted, mapping = aes(x = fruit, y = number)) +
  geom_col()

ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar()

ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
  # makes the width of the bars the same for a given category
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1)


named_dests <- flights %>%
  group_by(dest) %>%
  summarize(num_flights = n()) %>%
  arrange(desc(num_flights)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  rename(airport_name = name)

named_dests %>%
  # We can also return the top n values of a variable using the top_n() function
  top_n(n = 10, wt = num_flights) %>%
  arrange(desc(num_flights))

drinks_smaller <- drinks %>%
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) %>%
  select(-total_litres_of_pure_alcohol) %>%
  rename(beer = beer_servings, spirit = spirit_servings, wine = wine_servings)

drinks_smaller_tidy <- drinks_smaller %>%
  pivot_longer(names_to = "type",
               values_to = "servings",
               cols = -country)

ggplot(drinks_smaller_tidy, aes(x = country, y = servings, fill = type)) +
  geom_col(position = "dodge")

dem_score <- read_csv("https://moderndive.com/data/dem_score.csv")

guat_dem <- dem_score %>%
  filter(country == "Guatemala")

guat_dem_tidy <- guat_dem %>%
  pivot_longer(names_to = "year",
               values_to = "democracy_score",
               cols = -country,
               names_transform = list(year = as.integer))

ggplot(guat_dem_tidy, aes(x = year, y = democracy_score)) +
  geom_line() +
  labs(x = "Year", y = "Democracy Score")



### MODELING

# If we are modeling for explanation, we would be interested in both describing and quantifying the effects of the different factors.
# f we are modeling for prediction, however, we wouldn’t care so much about understanding how all the individual factors contribute to the outcome,
# but rather only whether we can make good predictions

# Linear regression involves a numerical outcome variable and explanatory variables that are either numerical or categorical

evals_minimal <- evals %>% select(ID, score, bty_avg, age)

# the inline histogram shown by default can be removed by using skim_without_charts() instead
evals_minimal %>% select(score, bty_avg) %>% skim()

# The skim() function only returns univariate summary statistics: functions that take a single variable and return some numerical summary of that variable
# However, there also exist bivariate summary statistics: functions that take in two variables and return some summary of those two variables.
# In particular, when the two variables are numerical, we can compute the correlation coefficient.
# Generally speaking, coefficients are quantitative expressions of a specific phenomenon.
# A correlation coefficient is a quantitative expression of the strength of the linear relationship between two numerical variables and ranges from -1 to 1.
# There is a certain amount of subjectivity in interpreting correlation coefficients, especially those that aren’t close to the extreme values of -1, 0, and 1.
# The correlation coefficient and the slope of a regression line always have the same sign (positive or negative), but they typically do not have the same value.
# The correlation’s interpretation is the "strength of linear association" but the slope's interpretation is the associated (but not necessary causal)
# increase in y, *on average*, for one unit increase of x

evals_minimal %>% get_correlation(formula = score ~ bty_avg) # this get_correlation method is in the moderndive package
evals_minimal %>% summarize(correlation = cor(score, bty_avg))

ggplot(evals_minimal, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Beauty Score",
       y = "Teaching Score",
       title = "Scatterplot of relationship of teaching and beauty scores")

# model formula is y ~ x
score_model <- lm(score ~ bty_avg, data = evals_minimal)

# These methods are in the moderndive package
get_regression_table(score_model)
regression_points <- get_regression_points(score_model)

regression_points %>%
  mutate(squared_residuals = residual^2) %>%
  summarize(sum_of_squared_residuals = sum(squared_residuals))

gapminder2007 <- gapminder %>%
  filter(year == 2007) %>%
  select(country, lifeExp, continent, gdpPercap)

gapminder2007 %>% select(lifeExp, continent) %>% skim()

ggplot(gapminder2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~ continent, nrow = 2) +
  labs(x = "Life expectancy", y = "Number of countries",
       title = "Histogram of distribution of worldwide life expectancies")


ggplot(gapminder2007, aes(x = continent, y = lifeExp)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Life expectancy",
       title = "Life expectancy by continent")


lifeExp_by_continent <- gapminder2007 %>%
  group_by(continent) %>%
  summarize(median = median(lifeExp),
            mean = mean(lifeExp))

lifeExp_model <- lm(lifeExp ~ continent, data = gapminder2007)

# The 5 values in the estimate column correspond to the baseline for comparison continent Africa (the intercept)
# as well as four 'offsets' from this baseline for the remaining 4 continents: the Americas, Asia, Europe, and Oceania.
# (You can change this baseline group to be another continent if you manipulate the variable continent’s factor levels)
# In general, if we fit a linear regression model using a categorical explanatory variable x that has k possible categories,
# the regression table will return an intercept and k−1 offsets.
# The sum of squared residuals is a measure of the lack of fit of a model. Larger values of the sum of squared residuals
# indicate a bigger lack of fit. The regression and its corresponding fitted values minimizes the sum of the squared residuals.

get_regression_table(lifeExp_model) # intercept corresponds to the mean life expectancy of countries in Africa of 54.8 years.
# The fitted value (*_hat) correspond to the mean life expectancies.
# These residuals can be interpreted as the deviation of a country’s life expectancy from its continent’s average life expectancy.
get_regression_points(lifeExp_model, ID = "country")


# Implementation of get_regression_table:
score_model %>%
  # this does most of the work
  tidy(conf.int = TRUE) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  clean_names() %>%
  rename(lower_ci = conf_low, upper_ci = conf_high)

# Implementation of get_regression_points:
score_model %>%
  # this does most of the work
  augment() %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  clean_names() %>%
  select(-c("std_resid", "hat", "sigma", "cooksd", "se_fit"))


evals_minimal <- evals %>% select(ID, score, age, gender)
evals_minimal %>% select(score, age, gender) %>% skim()

evals_minimal %>% get_correlation(formula = score ~ age)


ggplot(evals_minimal, aes(x = age, y = score, color = gender)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "Teaching Score", color = "Gender")

# An interaction model is a multiple regression model the relationship of the outcome variable and two explanatory variables
score_model_interaction <- lm(score ~ age * gender, data = evals_minimal)

# Since the word female comes alphabetically before male, female instructors are the baseline for comparison group.
# Thus, intercept is the intercept for only the female instructors.
# This holds similarly for age. It is the slope for age for only the female instructors.
# The value for gendermale is not the intercept for the male instructors, but rather the offset in intercept for male instructors relative to female instructors.
# Similarly, age:gendermale =is not the slope for age for the male instructors, but rather the offset in slope for the male instructors.
# The age:gendermale term in the equation for the fitted value is what’s known in statistical modeling as an interaction effect.
# We say there is an interaction effect if the associated effect of one variable depends on the value of another variable.

get_regression_table(score_model_interaction)


# When creating regression models with one numerical and one categorical explanatory variable, we are not just limited to interaction models.
# Another type of model we can use is known as a parallel slopes model. Unlike interaction models where the regression lines can have
# different intercepts and different slopes, parallel slopes models still allow for different intercepts but force all lines
# to have the same slope. The resulting regression lines are thus parallel.
ggplot(evals_minimal, aes(x = age, y = score, color = gender)) +
  geom_point() +
  geom_parallel_slopes(se = FALSE) + # this method is in the moderndive package
  labs(x = "Age", y = "Teaching Score", color = "Gender")

score_model_parallel_slopes <- lm(score ~ age + gender, data = evals_minimal)
get_regression_table(score_model_parallel_slopes)


# this dataset is in the ISLR package
credit_minimal <- Credit %>%
  as_tibble() %>%
  select(ID, debt = Balance, credit_limit = Limit,
         income = Income, credit_rating = Rating, age = Age)


credit_minimal %>% select(debt, credit_limit, income) %>% skim()

credit_minimal %>%
  # This methods is in the moderndive package
  get_correlation(debt ~ credit_limit)

credit_minimal %>%
  # This methods is in the moderndive package
  get_correlation(debt ~ income)

# Collinearity (or multicollinearity) is a phenomenon where one explanatory variable in a multiple regression model is highly correlated with another.
# Here is there is a high degree of collinearity between credit_limit and income. Thus, these two variables provide somewhat redundant information.
# Correlation coefficient is invariant to linear transformations.
credit_minimal %>%
  select(debt, credit_limit, income) %>%
  cor()

ggplot(credit_minimal, aes(x = credit_limit, y = debt)) +
  geom_jitter() +
  labs(x = "Credit limit (in $)", y = "Credit card debt (in $)",
       title = "Debt and credit limit") +
  geom_smooth(method = "lm", se = FALSE)

ggplot(credit_minimal, aes(x = income, y = debt)) +
  geom_jitter() +
  labs(x = "Income (in $1000)", y = "Credit card debt (in $)",
       title = "Debt and income") +
  geom_smooth(method = "lm", se = FALSE)

debt_model <- lm(debt ~ credit_limit + income, data = credit_minimal)

# we are now jointly interpreting the associated effect of multiple explanatory variables in the same model at the same time.
# When plotting the relationship between debt and income in isolation, there appeared to be a positive relationship.
# In the multiple regression, however, when jointly modeling the relationship between debt, credit_limit, and income,
# there appears to be a negative relationship of debt and income as evidenced by the negative slope for income.
# A phenomenon known as Simpson’s Paradox, whereby overall trends that exist in aggregate either disappear or reverse when
# the data are broken down into groups.
get_regression_table(debt_model)
get_regression_points(debt_model)

ggplot(MA_schools, aes(x = perc_disadvan, y = average_sat_math, color = size)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Percent economically disadvantaged", y = "Math SAT Score",
       color = "School size", title = "Interaction model")

ggplot(MA_schools, aes(x = perc_disadvan, y = average_sat_math, color = size)) +
  geom_point(alpha = 0.25) +
  geom_parallel_slopes(se = FALSE) +
  labs(x = "Percent economically disadvantaged", y = "Math SAT Score",
       color = "School size", title = "Parallel slopes model")

SAT_score_model_interaction <- lm(average_sat_math ~ perc_disadvan * size, data = MA_schools)
get_regression_table(SAT_score_model_interaction)

SAT_score_model_parallel_slopes <- lm(average_sat_math ~ perc_disadvan + size, data = MA_schools)
get_regression_table(SAT_score_model_parallel_slopes)


### SAMPLING
#  The concepts behind sampling form the basis of confidence intervals and hypothesis testing.

ggplot(tactile_prop_red, aes(x = prop_red)) +
  # setting boundary = 0.4 ensures a binning scheme with one of the bins’ boundaries at 0.4
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 33 proportions red")

virtual_shovel <- bowl %>% rep_sample_n(size = 50)

virtual_shovel %>%
  summarize(num_red = sum((color == "red"))) %>%
  mutate(prop_red = num_red/50)

virtual_samples <- bowl %>% rep_sample_n(size = 50, reps = 33)

virtual_prop_red <- virtual_samples %>%
  group_by(replicate) %>%
  summarize(num_red = sum(color == "red")) %>%
  mutate(prop_red = num_red/50)

ggplot(virtual_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 33 proportions red")

virtual_samples <- bowl %>% rep_sample_n(size = 50, reps = 1000)

virtual_prop_red <- virtual_samples %>%
  group_by(replicate) %>%
  summarize(num_red = sum(color == "red")) %>%
  mutate(prop_red = num_red / 50)

ggplot(virtual_prop_red, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red",
       title = "Distribution of 1000 proportions red")

virtual_samples_25 <- bowl %>% rep_sample_n(size = 25, reps = 1000)

virtual_prop_red_25 <- virtual_samples_25 %>%
  group_by(replicate) %>%
  summarize(red = sum(color == "red")) %>%
  mutate(prop_red = red / 25)

ggplot(virtual_prop_red_25, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 25 balls that were red", title = "25")

virtual_samples_50 <- bowl %>% rep_sample_n(size = 50, reps = 1000)

virtual_prop_red_50 <- virtual_samples_50 %>%
  group_by(replicate) %>%
  summarize(red = sum(color == "red")) %>%
  mutate(prop_red = red / 50)

ggplot(virtual_prop_red_50, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 50 balls that were red", title = "50")


virtual_samples_100 <- bowl %>% rep_sample_n(size = 100, reps = 1000)

virtual_prop_red_100 <- virtual_samples_100 %>%
  group_by(replicate) %>%
  summarize(red = sum(color == "red")) %>%
  mutate(prop_red = red / 100)

ggplot(virtual_prop_red_100, aes(x = prop_red)) +
  geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
  labs(x = "Proportion of 100 balls that were red", title = "100")

# There is less variation in the 1000 values of the proportion red. So as the sample size increases, our guesses at the true
# proportion of the bowl’s balls that are red get more precise.
virtual_prop_red_25 %>%
  summarize(sd = sd(prop_red))

virtual_prop_red_50 %>%
  summarize(sd = sd(prop_red))

virtual_prop_red_100 %>%
  summarize(sd = sd(prop_red))


# Definitions:
# - A population is a collection of individuals or observations we are interested in
# - Population parameter is a numerical summary quantity about the population that is unknown
# - A census is an exhaustive enumeration or counting of all N individuals or observations in the
#   population in order to compute the population parameter’s value exactly.
# - Sampling is the act of collecting a sample from the population when we don’t have the means to perform a census
# - A point estimate (AKA sample statistic) is a summary statistic computed from a sample that estimates an unknown population parameter.
# - A sample is said to be a representative sample if it roughly looks like the population.
# - We say a sample is generalizable if any results based on the sample can generalize to the population.
# - We say biased sampling occurs if certain individuals or observations in a population have a higher chance of being
#   included in a sample than others.
# - We say a sampling procedure is random if we sample randomly from the population in an unbiased fashion.

# In general:
#
# - If the sampling of a sample of size n is done at random, then
# - the sample is unbiased and representative of the population of size N, thus
# - any result based on the sample can generalize to the population, thus
# - the point estimate is a “good guess” of the unknown population parameter, thus
# - instead of performing a census, we can infer about the population using sampling.

# Statistical inference is the theory, methods, and practice of forming judgments about the parameters of a population and
# the reliability of statistical relationships, typically on the basis of random sampling.

# So as the sample size increases, the standard deviation of the estimates decreases. This type of standard deviation has
# another special name: standard error. Standard errors quantify the effect of sampling variation induced on our estimates.
# As a general rule, as sample size increases, the standard error decreases.

# It’s common for people who are new to statistical inference to call the sampling distribution the sample distribution.
# Remember that a standard error is merely a kind of standard deviation: the standard deviation of any point estimate from sampling.
# In other words, all standard errors are standard deviations, but not every standard deviation is necessarily a standard error.

# With random sampling estimates will on average be correct and thus will be centered at the true value.
# So random sampling ensures our point estimates are accurate, while on the other hand having a large sample size ensures our
# point estimates are precise. While the terms accuracy and precision may sound like they mean the same thing, there is a subtle difference.
# Accuracy describes how 'on target' our estimates are, whereas precision describes how consistent our estimates are.

# Central Limit Theorem: when sample means are based on larger and larger sample sizes, the sampling distribution of these sample means becomes
# both more and more normally shaped and more and more narrow.



### BOOTSTRAPPING

# In a real-life situation, we would not take many samples of size n, but rather take a single representative sample that’s as large as possible.
# You cannot directly study the effects of sampling variation when you only have one sample. One common method to study this is bootstrap resampling.
# From a statistical perspective, bootstrapping alludes to succeeding in being able to study the effects of sampling variation on estimates
# from the effort of a single sample. Or more precisely, it refers to constructing an approximation to the sampling distribution using only one sample.

# Once such technique is known as bootstrap resampling with replacement.
# The histogram of sample means from the resamples is called the bootstrap distribution of the sample mean.
# It is an approximation to the sampling distribution of the sample mean, in the sense that both distributions will have a similar shape and similar spread.
# The standard deviation of the bootstrap distribution is an approximate of the standard error of the sampling distribution.

# Using this bootstrap distribution, we can study the effect of sampling variation on our estimates, in particular, the standard error.
# The statistical concept of a confidence interval builds off the concept of bootstrap distributions.
# All other things being equal, higher confidence levels correspond to wider confidence intervals,
# and lower confidence levels correspond to narrower confidence intervals.
# Larger sample sizes tend to produce narrower confidence intervals for the same level of confidence i.e. the standard error decreases.

# The bootstrap distribution will likely not have the same center as the sampling distribution. In other words, bootstrapping
# cannot improve the quality of an estimate.

# Even if the bootstrap distribution might not have the same center as the sampling distribution, it will likely have very similar
# shape and spread. In other words, bootstrapping will give you a good estimate of the standard error.

x_bar <- pennies_sample %>%
  summarize(mean_year = mean(year))

ggplot(pennies_sample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Original sample of 50 pennies")

pennies_resample <- tibble(
  year = c(1976, 1962, 1976, 1983, 2017, 2015, 2015, 1962, 2016, 1976,
           2006, 1997, 1988, 2015, 2015, 1988, 2016, 1978, 1979, 1997,
           1974, 2013, 1978, 2015, 2008, 1982, 1986, 1979, 1981, 2004,
           2000, 1995, 1999, 2006, 1979, 2015, 1979, 1998, 1981, 2015,
           2000, 1999, 1988, 2017, 1992, 1997, 1990, 1988, 2006, 2000)
)

ggplot(pennies_resample, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(title = "Resample of 50 pennies")

resampled_means <- pennies_resamples %>%
  group_by(name) %>%
  summarize(mean_year = mean(year))


# The histogram of sample means from 35 resamples is called the bootstrap distribution
ggplot(resampled_means, aes(x = mean_year)) +
 geom_histogram(binwidth = 1, boundary = 1990, color = "white") +
  labs(x = "Sampled mean year")

virtual_resample <- pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 35)

virtual_resampled_means <- virtual_resample %>%
  group_by(replicate) %>%
  summarize(mean_year = mean(year))

ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "white") +
  labs(x = "Resample mean year")

virtual_resampled_means <- pennies_sample %>%
  rep_sample_n(size = 50, replace = TRUE, reps = 1000) %>%
  group_by(replicate) %>%
  summarize(mean_year = mean(year))

ggplot(virtual_resampled_means, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
  labs(x = "sample mean")

virtual_resampled_means %>%
  summarize(mean_of_means = mean(mean_year))


# Two methods for constructing confidence intervals: the percentile method and the standard error method.
# 1) Percentile method: For 95% confidence level, for example, use the middle 95% of values of the bootstrap distribution
# 2) Standard error methods: If a numerical variable follows a normal distribution,
#    then roughly 95% of values fall between ± 1.96 standard deviations of the mean.
# 3) Theoretical method: SE = [ p_hat * (1-p_hat)/n ] ^0.5.
#                        Margin of error MoE = 1.96 * SE  (for 95% confidence level)
#                        CI = p_hat ± MoE
# We can only use the standard error rule or the theoretical method, when the bootstrap distribution is roughly normally shaped.


# Statistical inference using the infer package

pennies_sample %>%
  # the specify() function is used to choose which variables in a data frame will be the focus of our statistical inference
  specify(response = year) %>%
  calculate(stat = "mean")

pennies_sample %>%
  # alternate construction with formula syntax.
  # We only have a response variable and no explanatory variable of interest and so we set the right-hand side to be NULL.
  specify(formula = year ~ NULL) %>%
  calculate(stat = "mean")


pennies_sample %>%
  specify(response = year) %>%
  # Resample a 1000 times to generate replicates with bootstrap resampling
  generate(reps = 1000, type = "bootstrap")

bootstrap_distribution <- pennies_sample %>%
  specify(response = year) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

# visualize() is a wrapper function for the ggplot() function that uses a geom_histogram() layer.
visualize(bootstrap_distribution)

percentile_ci <- bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "percentile")


visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = percentile_ci, color = "hotpink", fill = "khaki")

percentile_ci <- bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "se", point_estimate = x_bar)

visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = percentile_ci)


sample_1_bootstrap <- bowl_sample_1 %>%
  # Since it's not numeric, we need to define which event is of interest with the 'sucess' parameter
  specify(response = color, success = "red") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")

sample_1_prop <- bowl_sample_1 %>% specify(response = color, success = "red") %>% calculate(stat = "prop") %>% as.numeric

percentile_ci_1 <- sample_1_bootstrap %>%
  get_confidence_interval(level = 0.95, type = "percentile")

sample_1_bootstrap %>%
  visualize(bins = 15) +
  shade_confidence_interval(endpoints = percentile_ci_1) +
  geom_vline(xintercept = sample_1_prop, linetype = "dashed")


bowl_sample_2 <- bowl %>% rep_sample_n(size = 50)
sample_2_prop <- bowl_sample_2 %>% specify(response = color, success = "red") %>% calculate(stat = "prop") %>% as.numeric

sample_2_bootstrap <- bowl_sample_2 %>%
  specify(response = color, success = "red") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop")

percentile_ci_2 <- sample_2_bootstrap %>%
  get_confidence_interval(level = 0.95, type = "percentile")

sample_2_bootstrap %>%
  visualize(bins =  15) +
  shade_confidence_interval(endpoints = percentile_ci_2) +
  geom_vline(xintercept = sample_2_prop, linetype = "dashed")

# For every 100 95% confidence intervals, we expect that 95 of them will capture the true population mean and that five of them won’t.
# Precise mathematical interpretation: If we repeated our sampling procedure a large number of times, we expect about 95% of the
# resulting *confidence intervals* to capture the value of the population parameter.

# The 95% confidence level only relates to the reliability of the confidence interval construction procedure and
# not to a given confidence interval itself.


# In two-sample inference situation we have two separate samples with different sizes. Both groups need to be independent.

mythbusters_yawn %>%
  group_by(group, yawn) %>%
  summarize(sum = n())

# In the infer package, bootstrapping with multiple variables means that each row is potentially resampled.
bootstrap_distribution_yawning <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))

visualize(bootstrap_distribution_yawning) +
  geom_vline(xintercept = 0)

myth_ci_pctl <- bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "percentile", level = 0.95)

visualize(bootstrap_distribution_yawning) +
  shade_confidence_interval(endpoints = myth_ci_pctl)

obs_diff_in_props <- mythbusters_yawn %>%
  specify(formula = yawn ~ group, success = "yes") %>%
  calculate(stat = "diff in props", order = c("seed", "control"))

myth_ci_se <- bootstrap_distribution_yawning %>%
  get_confidence_interval(type = "se", level = 0.95, point_estimate = obs_diff_in_props)

visualize(bootstrap_distribution_yawning) +
  shade_confidence_interval(endpoints = myth_ci_pctl, color = "black", fill = NULL) +
  shade_confidence_interval(endpoints = myth_ci_se, color = "grey", fill = NULL)


# There is one value of particular interest that this 95% confidence interval contains: zero. If the difference
# in proportions (seed - control) were equal to 0, then there would be no difference in proportion yawning between
# the two groups. This would suggest that there  is no associated effect of being exposed to a yawning recruiter on
# whether you yawn yourself. Say, on the other hand, the 95% confidence interval was entirely above zero. We’d have
# evidence suggesting those exposed to yawning do yawn more often.

tactile_prop_red %>%
  rename(p_hat = prop_red) %>%
  mutate(n = 50,
         SE = sqrt(p_hat * (1-p_hat)/n),
         MoE= 1.96*SE,
         lower_ci = p_hat - MoE,
         upper_ci = p_hat + MoE)

# Bootstrapping is a kind of simulation-based inference. Two large benefits of simulation-based methods over theory-based methods are that
# (1) they are easier for people new to statistical inference to understand and
# (2) they also work in situations where theory-based methods and mathematical formulas don’t exist.



### HYPOTHESIS TESTING

# Hypothesis tests are used to infer about a population using a sample which another method for statistical inference.

# Traditional theory-based methods like the t-test and normal-theory confidence intervals are just approximations for the computer-based methods.
# However, they also require conditions to be met for their results to be valid. Computer-based methods using randomization, simulation,
# and bootstrapping have much fewer restrictions.

ggplot(promotions, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on resume")

promotions %>%
  group_by(gender, decision) %>%
  tally() # tally() here is a shortcut for summarize(n = n())

ggplot(promotions_shuffled, aes(x = gender, fill = decision)) +
  geom_bar() +
  labs(x = "Gender of name on resume")

promotions_shuffled %>%
  group_by(gender, decision) %>%
  tally()

# We can repeat this shuffling of randomly assigning gender to all decisions with equal probability and look at it sampling distribution.
# Then we can see if the actual observed value is a statistical anomaly.
# This activity is the statistical procedure known as hypothesis testing using a permutation test.
# Permutations are another form of resampling, like the bootstrap method. While the bootstrap method involves resampling with replacement,
# permutation methods involve resampling without replacement.

# 1) A hypothesis is a statement about the value of an unknown population parameter.
# 2) A hypothesis test consists of a test between two competing hypotheses: (1) a null hypothesis H0 versus (2) an alternative hypothesis Ha or H1
#    Generally the null hypothesis is a claim that there is "no effect" or "no difference of interest."
#    Furthermore, generally the alternative hypothesis is the claim one wants to establish or find evidence to support.
#    The alternative can be one-sided or a two-sided.
# 3) A test statistic is a point estimate/sample statistic formula used for hypothesis testing.
# 4) The observed test statistic is the value of the test statistic that we observed in real life.
# 5) The null distribution is the sampling distribution of the test statistic assuming the null hypothesis.
# 6) The p-value is the probability of obtaining a test statistic just as extreme or more extreme than the observed test statistic
#    assuming the null hypothesis is true.
# 7) In many hypothesis testing procedures, it is commonly recommended to set the significance level of the test beforehand, denoted by alpha.
#    It acts as a cutoff on the p-value where if p-value falls below alpha we would reject the null hypothesis.

promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  hypothesize(null = "independence") # we have two samples (the resumes with male and female names). If we only have one sample, we set the null to "point"

# The term "point" relates from the fact that for a single group of observations, you will test the value of a single point
# (e.g with the earlier example say we wanted to test if the mean year for all pennies was 1993 or not).
# The term "independence" relates to the fact that for two groups of observations, you are testing whether or not the response variable is independent
# of the explanatory variable that assigns the groups.

promotions_generate <- promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") # resampling without replacement

# Point estimates related to hypothesis testing have a specific name: test statistics
null_distribution <- promotions_generate %>%
  calculate(stat = "diff in props", order = c("male", "female"))


obs_diff_prop <- promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  calculate(stat = "diff in props", order = c("male", "female"))

visualize(null_distribution, bins = 10) +
  shade_p_value(obs_stat = obs_diff_prop, direction = "right") # "right" reflects our alternative hypothesis which one-sided. Use "both" for two-sided

null_distribution %>%
  get_p_value(obs_stat = obs_diff_prop, direction = "right")

# Since this p-value is smaller than our pre-specified significance level alpha = 0.05, we reject the null hypothesis.
# Whether we reject the null hypothesis or not depends in large part on our choice of significance level.

bootstrap_distribution <- promotions %>%
  specify(formula = decision ~ gender, success = "promoted") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("male", "female"))

percentile_ci <- bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "percentile")

# Notice a key value that is not included in the 95% confidence interval for p_m - p_f is the value 0, which indicates no discrimination.
visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = percentile_ci)

se_ci <- bootstrap_distribution %>%
  get_confidence_interval(level = 0.95, type = "se", point_estimate = obs_diff_prop)

# Again, the value 0 is not included in our confidence interval
visualize(bootstrap_distribution) +
  shade_confidence_interval(endpoints = se_ci)


# There are two possible errors in a hypothesis test:
# (1) rejecting H0 when in fact H0 is true, called a Type I error (false positive)
# (2) failing to reject H0 when in fact H0 is false, called a Type II error (false negative)

# The probability of a Type I error is denoted by alpha the significance level of the hypothesis test
# The probability of a Type II error is denoted by beta. 1 - beta is called the power of the test.
# Ideally, we want alpha and beta to be 0 but there will always be the possibility of making either error when we use sample data.
# These two error probabilities are inversely related and as one goes down the other goes up.
# What is typically done in practice is to fix the probability of a Type I error by pre-specifying a significance level alpha
# and then try to minimize beta.

# A very low value for alpha, like 0.01, means you will reject then null hypothesis less often, resulting in a conservative test.


ggplot(movies_sample, aes(x = genre, y = rating)) +
  geom_boxplot() +
  labs(y = "IMDb rating")

movies_sample %>%
  group_by(genre) %>%
  summarize(n = n(), mean_rating = mean(rating), std_dev = sd(rating))

null_distribution_movies <- movies_sample %>%
  specify(formula = rating ~ genre) %>% # since outcome is numeric you don't need a 'success' parameter
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("Action", "Romance"))

obs_diff_means <- movies_sample %>%
  specify(formula = rating ~ genre) %>%
  calculate(stat= "diff in means", order = c("Action", "Romance"))

visualize(null_distribution_movies, bins = 10) +
  shade_p_value(obs_stat = obs_diff_means, direction = "both") # use 'both' since it's two-sided test

null_distribution_movies %>%
  get_p_value(obs_stat = obs_diff_means, direction = "both")



# Any theory-based method is ultimately an approximation to the simulation-based method.
# Assume we have sample 1 and 2 with counts n_1 and n_2

# Standard error of mean (SEM) = standard deviation / sqrt(n)
# Two-sample t-statistic = [ (diff in sample means) - (diff in population means) ] / sqrt(SEM_sample_1 ^ 2 + SEM_sample_2 ^ 2)

# Under the null hypothesis, diff in population means is 0. Given the Central Limit Theorem that sample means follow a normal distribution,
# it follows that the two-sample t-statistic follows a t-distribution with degrees of freedom **roughly** equal to n_1 + n_2 - 2
# (true formula for degrees of freedom is more complicated but doesn't matter much for inference).

# The t-distribution is centered on zero. As the degrees of freedom increase, the t-distribution more and more resembles the standard normal
# z curve. The degrees of freedom measures how different the t distribution will be from a normal distribution.
# t-distributions tend to have more values in the tails of their distributions than the standard normal z curve.
# So, small sample sizes lead to small degrees of freedom and thus small sample sizes lead to t-distributions
# that are different than the z curve.

movies_sample_stats <- movies_sample %>%
  group_by(genre) %>%
  summarize(n = n(), mean_rating = mean(rating), std_dev = sd(rating))

mean_a <- movies_sample_stats[movies_sample_stats$genre == "Action", "mean_rating"]
mean_b <- movies_sample_stats[movies_sample_stats$genre == "Romance", "mean_rating"]
SEM_a <- movies_sample_stats[movies_sample_stats$genre == "Action", "std_dev"]/sqrt(movies_sample_stats[movies_sample_stats$genre == "Action", "n"])
SEM_b <- movies_sample_stats[movies_sample_stats$genre == "Romance", "std_dev"]/sqrt(movies_sample_stats[movies_sample_stats$genre == "Romance", "n"])

two_sample_t_test_statistic <- (mean_a - mean_b)/sqrt(SEM_a^2 + SEM_b^2)

null_distribution_movies_t <- movies_sample %>%
  specify(formula = rating ~ genre) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("Action", "Romance"))

visualize(null_distribution_movies_t, bins = 10, method = "both")

obs_two_sample_t <- movies_sample %>%
  specify(formula = rating ~ genre) %>%
  calculate(stat = "t", order = c("Action", "Romance"))

visualize(null_distribution_movies_t, bins = 10, method = "both") +
  shade_p_value(obs_stat = obs_two_sample_t, direction = "both")

null_distribution_movies_t %>%
  get_p_value(obs_stat = obs_two_sample_t, direction = "both")


# In order for the results of our two-sample t-test to be valid, three conditions must be met:
# 1) Nearly normal populations or large sample sizes. A general rule of thumb that works in many (but not all) situations is that
#    the sample size  n should be greater than 30.
# 2) Both samples are selected independently of each other.
# 3) All observations are independent from each other.

# For this dataset, if the same person rated multiple movies, then those observations would be related and hence not independent and
# so #3 may be violated.

# On the other hand, in most scenarios, the only assumption that needs to be met in the simulation-based method is that the sample is
# selected at random. Thus, in our experience, we prefer simulation-based methods as they have fewer assumptions.

# p-hacking is the act of cherry-picking only results that are statistically significant while dismissing those
# that aren’t, even if at the expense of the scientific ideas.
# So one should  prefer the use of confidence intervals for statistical inference, since they are much less prone to large misinterpretation.



### INFERENCE FOR REGRESSION

# The standard error of the slope b1 similarly quantifies how much variation in the fitted slope one would expect between different samples.

# In the standard regression table output, the fourth column statistic corresponds to a test statistic relating to the
# null hypothesis of the slope being zero (i.e. no true relationship). It corresponds to a standardized
# t-test statistic (similar to the theoretical one seen earlier).
# By convention, all hypothesis testing for regression assumes two-sided alternatives.


# For inference for regression, there are four conditions that need to be met (first letters spell the word LINE)
# 1) Linearity of relationship between variables
# 2) Independence of the residuals
# 3) Normality of the residuals, centered on zero
# 4) Equality of variance of the residuals (the value and spread of the residuals should not depend on the value of the explanatory variable)

# Conditions 1, 3 and 4 can be verified through residual analysis. The 2nd condition, can only be verified by understanding
# how the data was collected.

# A residual analysis is used to verify conditions 1, 3, and 4 and can be performed using appropriate data visualizations
# but there are more sophisticated statistical approaches that can also be done.

# Checking for Condition 3:
# The histogram shows that we have more positive residuals than negative, which means it tends to *underestimate*
# the true score. Furthermore, the distribution is slightly left-skewed or negatively skewed.
ggplot(regression_points, aes(x = residual)) +
  geom_histogram(binwidth = 0.25, color = "white") +
  labs(x = "Residual")

# Checking for Condition 4:
# Visually see if the spread of the residuals around 0 are constant across all values of the explanatory variable.
ggplot(regression_points, aes(x = bty_avg, y = residual)) +
  geom_point() +
  labs(x = "Beauty Score", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)

# On approach to dealing with violations of condition 2 around independence of residuals is hierarchical/multilevel modeling.
# When the other conditions are not met, it may be that we need more explanatory variables and need to a multiple regression model


# Simulation based inference for regression:

# This uses the built-in R code methods which does not use simulation but a theory-based approach.
get_regression_table(score_model)

evals_minimal <- evals %>% select(ID, score, bty_avg, age)

# It is important to note in this case that the bootstrapping with replacement is done row-by-row. Thus, the original pairs of
# score and bty_avg values are always kept together, but different pairs of score and bty_avg values may be resampled multiple times.
bootstrap_distribution_slope <- evals_minimal %>%
  specify(formula = score ~ bty_avg) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope")

visualize(bootstrap_distribution_slope)

percentile_ci <- bootstrap_distribution_slope %>%
  get_confidence_interval(level = 0.95, type = "percentile")

observed_slope <- evals_minimal %>%
  specify(formula = score ~ bty_avg) %>%
  calculate(stat = "slope")

se_ci <- bootstrap_distribution_slope %>%
  get_confidence_interval(level = 0.95, type = "se", point_estimate = observed_slope)

visualize(bootstrap_distribution_slope) +
  shade_confidence_interval(endpoints = percentile_ci, fill = NULL, linetype = "solid", color = "grey80") +
  shade_confidence_interval(endpoints = se_ci, fill = NULL, linetype = "dashed", color = "grey60") +
  # Use the CI for bty_avg from get_regression_table(score_model)
  shade_confidence_interval(endpoints = c(0.035, 0.099), fill = NULL, linetype = "dotted", color = "black")

null_distribution_slope <- evals_minimal %>%
  specify(formula = score ~ bty_avg) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "slope")

visualize(null_distribution_slope) +
  shade_p_value(obs_stat = observed_slope, direction = "both")

# Since the observed fitted slope 0.067 falls far to the right of this null distribution and thus the shaded region doesn’t overlap it,
# we’ll have a p-value of 0
null_distribution_slope %>%
  get_p_value(obs_stat = observed_slope, direction = "both")

# When the conditions for inference for regression are met and the null distribution has a bell shape,
# we are likely to see similar results between the simulation-based results we just demonstrated and the theory-based results
# shown in the regression table with get_regression_table() method


# Theory based inference for regression:

# SE for fitted slope SE_slope = sample_sd_y / sample_sd_x * sqrt(1 - sample_correlation_x_y^2) / sqrt (n - 2)
# As sample size increases, SE decreases as in the previous examples.
# The t-statistic used for the hypothesis testing t = slope_estimate - slope_population / SE_slope
# Under the null hypothesis, slope_population is zero.
# The t-distribution can be proven to have degrees of freedom = n-2
# To compute the p-value, we need to compare the observed test statistic to the appropriate null distribution


### CASE STUDIES

house_prices %>%
  select(price, sqft_living, condition) %>%
  skim()

ggplot(house_prices, aes(x = price)) +
  geom_histogram(color = "white") +
  labs(x = "price (USD)", title = "House price")

ggplot(house_prices, aes(x = sqft_living)) +
  geom_histogram(color = "white") +
  labs(x = "living space (square feet)", title = "House size")

ggplot(house_prices, aes(x = condition)) +
  geom_bar() +
  labs(x = "condition", title = "House condition")

# Log transformations allow us to focus on changes in orders of magnitude i.e. they allow us to focus on multiplicative
# changes instead of additive ones. log transformations are monotonic, meaning they preserve orders.

house_prices <- house_prices %>%
  mutate(
    log10_price = log10(price),
    log10_size = log10(sqft_living)
  )

# The distribution is much less skewed, and in this case, more symmetric and more bell-shaped
ggplot(house_prices, aes(x = log10_price)) +
  geom_histogram(color = "white") +
  labs(x = "log10 price (USD)")

ggplot(house_prices, aes(x = log10_size)) +
  geom_histogram(color = "white") +
  labs(x = "log10 living space (square feet)")

# interaction model
ggplot(house_prices, aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(y = "log10 price",
       x = "log10 size",
       title = "House prices in Seattle")

ggplot(house_prices, aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ condition)
labs(y = "log10 price",
     x = "log10 size",
     title = "House prices in Seattle")

# parallel slopes model
ggplot(house_prices, aes(x = log10_size, y = log10_price, color = condition)) +
  geom_point(alpha = 0.05) +
  geom_parallel_slopes(se = FALSE) +
  labs(y = "log10 price",
       x = "log10 size",
       title = "House prices in Seattle")

# interaction model
price_interaction_model <- lm(log10_price ~ log10_size * condition, data = house_prices)

# the baseline for comparison group for the categorical variable condition are the condition 1 houses
get_regression_table(price_interaction_model)
