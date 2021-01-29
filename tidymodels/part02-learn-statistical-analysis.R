####----
# tidymodels: Learn - Perform Statistical Analysis
# Based on content published at https://www.tidymodels.org/learn/
####----

####----
# Section 1: Correlation and regression fundamentals with tidy data principles
# Based on content published at https://www.tidymodels.org/learn/statistics/tidy-analysis/

####----

library(tidymodels)

Orange <- as_tibble(Orange)

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) +
  geom_line()

# the age and circumference are quite correlated, as is to be expected
cor(Orange$age, Orange$circumference)

Orange %>%
  group_by(Tree) %>%
  summarize(correlatino = cor(age, circumference))

# Instead of simply estimating a correlation, we want to perform a hypothesis test with cor.test():
ct <- cor.test(Orange$age, Orange$circumference)
tidy(ct)

# We can also perform the hypothesis for each type of tree and the easiest way is to use nesting.
# Nesting creates a list-column of data frames; unnesting flattens it back out into regular columns
# Nesting is implicitly a summarising operation: you get one row for each group defined by the non-nested columns
nested_Orange <- Orange %>%
  nest(data = c(age, circumference)) # or alternatively, data = c(-Tree)

nested_Orange <- nested_Orange %>%
  mutate(test = map(data, ~ cor.test(.x$age, .x$circumference)),
         tidied = map(test, tidy))
nested_Orange

# View the unnested data
nested_Orange %>%
  unnest(cols = tidied) %>%
  select(-data, -test)

### REGRESSION MODELS

lm_fit <- lm(age ~ circumference, data = Orange)
tidy(lm_fit)

# multiple regressions for each type of tree

nested_Orange <- Orange %>%
  nest(data = c(-Tree)) %>%
  mutate(fit = map(data, ~ lm(age ~ circumference, data = .x)),
         tidied = map(fit, tidy))

# View the unnested data
nested_Orange %>%
  unnest(cols = tidied) %>%
  select(-data, -fit)

# Multiple regression on the mtcars data set for each type of transmission, specified by the 'am' column
mtcars %>%
  nest(data = c(-am)) %>%
  mutate(fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(cols = tidied) %>%
  select(-data, -fit)


# We can also add in the augmented and glanced data in addition to the tidied data set.

# Glance on a model object and returns summaries that include goodness of fit measures,
# p-values for hypothesis tests on residuals, or model convergence information.

# Augment on a model object adds information about each observation in the dataset. Most commonly, this includes
# predicted values in the .fitted column, residuals in the .resid column, and standard errors
# for the fitted values in a .se.fit column

regressions <- mtcars %>%
  nest(data = c(-am)) %>%
  mutate(fit = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),
         tidied = map(fit, tidy),
         glanced = map(fit, glance),
         augmented = map(fit, augment))

regressions %>%
  unnest(cols = c(glanced, augmented)) %>%
  select(-data, -fit, -tidied)



####----
# Section 2: K-means clustering with tidy data principles
# Based on content published at https://www.tidymodels.org/learn/statistics/k-means/

####----

set.seed(27)

# Generate some random two-dimensional data with three clusters. Data in each cluster will come from
# a multivariate gaussian distribution, with different means for each cluster.

centers <- tibble(cluster = factor(1:3), # cluster identifiers
                  num_points = c(100, 150, 50), # number of points in each cluster
                  x1 = c(5, 0, -3), # x1 coordinate of cluster center
                  x2 = c(-1, 1, -2)) # x2 coordinate of cluster center

labelled_points <- centers %>%
  mutate(x1 = map2(num_points, x1, rnorm),
         x2 = map2(num_points, x2, rnorm)) %>%
  select(-num_points) %>%
  unnest(cols = c(x1, x2))

ggplot(labelled_points, aes(x = x1, y = x2, color = cluster)) +
  geom_point(alpha = 0.3)

points <- labelled_points %>%
  select(-cluster)

# the centers argument can either be the centers in each cluster or if it's a number like here, it represents
# the number of clusters, in which case a random center is chosen for each cluster
kclust <- kmeans(points, centers = 3)
summary(kclust)

# kclust has several components of different length
# - cluster (300 values) contains information about each point
# - centers, withinss, and size (3 values) contain information about each cluster
# - totss, tot.withinss, betweenss, and iter (1 value) contain information about the full clustering
# - The value ifault indicates possible algorithm problems

tidy(kclust) # summaries for each cluster
augment(kclust, points) # add cluster classifications to all the points

glance(kclust) # single-row summary

# The data from glance() fills a different but equally important purpose;
# It lets us view trends of some summary statistics across values of k. Of particular interest is
# the total within sum of squares, saved in the tot.withinss column.
# This represents the variance within the clusters.



# Exploratory clustering for different numbers of clusters

kclusts <- tibble(k = 1:9) %>%
  mutate(kclust = map(k, ~ kmeans(points, .x)),
         tidied = map(kclust, tidy),
         glanced = map(kclust, glance),
         augmented = map(kclust, augment, points))

clusters <- kclusts %>%
  unnest(cols = tidied)

assignments <- kclusts %>%
  unnest(cols = augmented)

clusterings <- kclusts %>%
  unnest(cols = glanced)

# Visualize the cluster classifications for each value of k
p1 <- ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.3) +
  facet_wrap(~ k)
p1

# Add in the cluster centers
p1 + geom_point(data = clusters, shape = "x", size = 2)


# The variance within the clusters decreases as k increases, but there is bend around k = 3, after which the
# additional clusters have little value
ggplot(clusterings, aes(x = k, y = tot.withinss)) +
  geom_line() +
  geom_point()



####----
# Section 3: Bootstrap resampling and tidy regression models
# Based on content published at https://www.tidymodels.org/learn/statistics/bootstrap/

####----

# Fit a nonlinear model to the weight/mileage relationship in the mtcars data set

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Use the method of nonlinear least squares to fit a model.
nlsfit <- nls(mpg ~ k/wt + b, data = mtcars, start = list(k=1, b=0))
summary(nlsfit)
augment(nlsfit) # show's the original, fitted values and residuals

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  # the prediction is done on the original data points when new points are not specified in the call to predict
  geom_line(aes(y = predict(nlsfit)), color = "blue")

# Alternative approach
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_line(data = augment(nlsfit), aes(x = wt, y = .fitted), color = "blue")


set.seed(27)
# Setting 'apparent' to TRUE gives an extra resample where the analysis and holdout subset are the entire data set
mtcars_resampled <- bootstraps(mtcars, times = 2000, apparent = TRUE)

# individual splits can be retrieved with the analysis() and assessment() methods
analysis(mtcars_resampled$splits[[1]])
assessment(mtcars_resampled$splits[[1]])

fit_nls_on_bootstrap <- function(split) {
  nls(mpg ~ k/wt + b, data = analysis(split), start = list(k=1, b=0))
}

mtcars_models <- mtcars_resampled %>%
  mutate(model = map(splits, fit_nls_on_bootstrap),
         ceofficient_info = map(model, tidy))

mtcars_coefficients <- mtcars_models %>%
  unnest(ceofficient_info)

# Get confidence intervals for each term estimated 'k' and 'b'
percentile_intervals <- int_pctl(mtcars_models, statistics = ceofficient_info)

# Visualize the distribution of the estimates with confidence intervals
ggplot(mtcars_coefficients, aes(estimate)) +
  geom_histogram(bins = 30, color = "white") +
  facet_wrap(~ term, scales = "free") +
  geom_vline(data = percentile_intervals, aes(xintercept = .lower), color = "blue") +
  geom_vline(data = percentile_intervals, aes(xintercept = .upper), color = "blue")

# Inspect the fitted curves for some of the models at random
mtcars_models_augmented <- mtcars_models %>%
  sample_n(200) %>%
  mutate(augmented = map(model, augment)) %>%
  unnest(augmented)

ggplot(mtcars_models_augmented, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_line(aes(y = .fitted, color = id), alpha = 0.2) +
  theme(legend.position = "none")

# fit a cubic smoothing spline to data
fit_spline_on_bootstrap <- function(split) {
  data <- analysis(split)
  smooth.spline(data$wt, data$mpg, df = 4)
}

mtcars_splines <- mtcars_resampled %>%
  sample_n(200) %>%
  mutate(spline = map(splits, fit_spline_on_bootstrap),
         augmented = map(spline, augment))

# The w column in the weight vector, which defaults to 1 if not specified in the call to smooth.spline
mtcars_splines_augmented <- mtcars_splines %>%
  unnest(augmented)

ggplot(mtcars_splines_augmented, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = .fitted, color = id), alpha = 0.2) +
  theme(legend.position = "none")



####----
# Section 4: Hypothesis testing using resampling and tidy data
# Based on content published at https://www.tidymodels.org/learn/statistics/infer/

####----

glimpse(gss)

# Determining whether the mean number of hours worked per week is 40 hours

null_distribution <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 5000, type = "bootstrap") %>%
  calculate(stat = "mean")

visualize(null_distribution)

point_estimate <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

visualize(null_distribution) +
  shade_p_value(obs_stat = point_estimate, direction = "two-sided")

null_distribution %>%
  get_p_value(obs_stat = point_estimate, direction = "two-sided")

null_distribution %>%
  get_confidence_interval(point_estimate = point_estimate, level = 0.95, type = "se")

null_f_distribution <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "F")

null_f_distribution_theoretical <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F")

F_hat <- gss %>%
  specify(age ~ partyid) %>%
  calculate(stat = "F")

visualize(null_f_distribution_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

# Show both the theoretical and resampled distributions
visualize(null_f_distribution, method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")



####----
# Section 5: Statistical analysis of contingency tables
# Based on content published at https://www.tidymodels.org/learn/statistics/xtabs/

####----

# Conduct a chi-squared test of independence and a chi-squared goodness of fit test

data(ad_data, package = "modeldata")
glimpse(ad_data)

observed_chisq_stat <- ad_data %>%
  specify(Class ~ Genotype) %>%
  calculate(stat = "Chisq")

null_distribution <- ad_data %>%
  specify(Class ~ Genotype) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 5000, type = "permute") %>%
  calculate(stat = "Chisq")

null_distribution_theoretical <- ad_data %>%
  specify(Class ~ Genotype) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "Chisq")

visualize(null_distribution) +
  shade_p_value(obs_stat = observed_chisq_stat, direction = "greater")

visualize(null_distribution_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = observed_chisq_stat, direction = "greater")

visualize(null_distribution, method = "both") +
  shade_p_value(obs_stat = observed_chisq_stat, direction = "greater")

# this p-value is the approximate probability that we would see a statistic as or more extreme than the observed 21.577
null_distribution %>%
  get_p_value(obs_stat = observed_chisq_stat, direction = "greater")

# Equivalently, we can use the chisq_test method
chisq_test(ad_data, formula = Class ~ Genotype)


# Chi-squared goodness of fit test

# Song, Y., Stampfer, M. J., & Liu, S. (2004). Meta-Analysis: Apolipoprotein E Genotypes
# and Risk for Coronary Heart Disease. Annals of Internal Medicine, 141(2), 137.
meta_rates <- c("E2E2" = 0.71, "E2E3" = 11.4, "E2E4" = 2.32,
                "E3E3" = 61.0, "E3E4" = 22.6, "E4E4" = 2.22)
meta_rates <- meta_rates/sum(meta_rates)

# The null hypothesis is that Genotype follows the same frequency distribution as the meta-analysis
observed_chisq_stat <- ad_data %>%
  specify(response = Genotype) %>%
  hypothesize(null = "point", p = meta_rates) %>%
  calculate(stat = "Chisq")

null_distribution <- ad_data %>%
  specify(response = Genotype) %>%
  hypothesize(null = "point", p = meta_rates) %>%
  generate(reps = 5000, type = "simulate") %>%
  calculate(stat = "Chisq")

visualize(null_distribution) +
  shade_p_value(obs_stat = observed_chisq_stat, direction = "greater")

null_distribution %>%
  get_p_value(obs_stat = observed_chisq_stat, direction = "greater")

chisq_test(ad_data, response = Genotype, p = meta_rates)
