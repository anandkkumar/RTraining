####----
# tidymodels: Get started
# Based on content published at https://www.tidymodels.org/start/models/
####----


####----
# Section 1: Build a model

####----

library(tidymodels)
library(readr)
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker) # for visualizing regression results

urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
  setNames(c("food_regime", "initial_volume", "width")) %>%
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


# Since the slopes appear to be different for at least two of the feeding regimes, let’s build a model that allows for two-way interactions
ggplot(urchins, aes(x = initial_volume, y = width, color = food_regime)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# ANOVA is a general technique that can be used to test the hypothesis that the means among two or more groups are equal,
# under the assumption that the sampled populations are normally distributed.

lm_mod <- linear_reg() %>%
  set_engine("lm")

# The modeling code uses the pipe to pass around the model object (similar to how the dplyr methods pass around the data object)

# This fitted object lm_fit (which is parnsip object) has the lm model output built-in, which you can access with lm_fit$fit
lm_fit <- lm_mod %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

# Inspect regression results
tidy(lm_fit, conf.int = TRUE)

# Visualize the results
tidy(lm_fit) %>%
  dwplot(dot_args = list(size = 2, color = "blue"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, color = "grey50", linetype = 2))


# Make predictions for new data
new_points <- data.frame(initial_volume = 20,
                         food_regime = factor(c("Initial", "Low", "High"), levels = levels(urchins$food_regime)) )

mean_pred <- predict(lm_fit, new_data = new_points)
conf_int_pred <- predict(lm_fit, new_data = new_points, type = "conf_int")

plot_data <- new_points %>%
  bind_cols(mean_pred) %>%
  bind_cols(conf_int_pred)

ggplot(plot_data, aes(x = food_regime)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = 0.2) +
  labs(y = "urchin size")


# Bayesian approach:

# In a Bayesian analysis, a prior distribution needs to be declared for each model parameter that represents
# the possible values of the parameters (before being exposed to the observed data)
# To take a conservative approach we make the priors wide using a Cauchy distribution (which is the same as a t-distribution with a single degree of freedom)


# In a Bayesian analysis, a prior distribution needs to be declared for each model parameter that represents
# the possible values of the parameters (before being exposed to the observed data)
# To take a conservative approach we make the priors wide using a Cauchy distribution (which is the same as a t-distribution with a single degree of freedom)

# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

# This kind of Bayesian analysis (like many models) involves randomly generated numbers in its fitting procedure.
# We can use set.seed() to ensure that the same (pseudo-)random numbers are generated each time we run this code.
set.seed(123)

bayes_mod <- linear_reg() %>%
  set_engine("stan", prior_intercept = prior_dist, prior = prior_dist)

bayes_fit <- bayes_mod %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

# Inspect regression results
tidy(bayes_fit,conf.int = TRUE)

bayes_mean_pred <- predict(bayes_fit, new_data = new_points)
bayes_conf_int_pred <- predict(bayes_fit, new_data = new_points, type = "conf_int")

bayes_plot_data <- new_points %>%
  bind_cols(bayes_mean_pred) %>%
  bind_cols(bayes_conf_int_pred)

ggplot(bayes_plot_data, aes(x = food_regime)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = 0.2) +
  labs(y = "urchin size") +
  ggtitle("Bayesian model with t(1) prior distribution")



####----
# Section 2: Preprocess your data with recipes

####----

library(tidymodels)
library(nycflights13)
library(skimr)


flight_data <- flights %>%
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = as.Date(time_hour)
  ) %>%
  inner_join(weather, by = c("origin", "time_hour")) %>%
  select(dep_time, flight, origin, dest, air_time, distance,
         carrier, date, arr_delay, time_hour) %>%
  na.omit() %>%
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)

# Because we’ll be using a simple logistic regression model, the variables 'dest' and 'carrier' will be converted to dummy variables

set.seed(555)
# This puts 3/4 of the data into the training set
data_split <- initial_split(flight_data, prop = 3/4)

# Create the two sets
train_data <- training(data_split)
test_data  <- testing(data_split)

# Outcome variable is the 'arr_delay'. The . indicates we want to use all other variables as predictors (for now)
# The data is contained in the recipe object under 'template'
flights_recipe <- recipe(arr_delay ~ ., data = train_data)

# There are two variables that we don’t want to use as predictors in our model, but as identification variables: 'flight' and 'time_hour
# We can use the update_role() function to let recipes know that flight and time_hour are variables with a custom role
# that we called "ID" (can really be any character value)

flights_recipe <- flights_recipe %>%
  update_role(flight, time_hour, new_role = "ID")

# Inspect the variables and roles
summary(flights_recipe)


flights_recipe <- flights_recipe %>%
  # step_date creates a specification of a recipe step that will convert date data into one or more factor or numeric variables
  step_date(date, features = c("dow", "month")) %>% # create two factor columns for day-of-week and month, which we will use instead
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  step_rm(date)

# Because we plan to train a logistic regression model, that predictors will ultimately need to be numeric, as opposed to factor variables
flights_recipe <- flights_recipe %>%
  # all_nominal(), selects all variables that are either factors or characters
  # -all_outcomes() removes any outcome variables from this recipe step
  # combining the two, this steps ends creating dummy variables for all of the factor or character columns unless they are outcomes
  step_dummy(all_nominal(), -all_outcomes())

# It is possible that dummy variables might be created for values that don’t exist in the training set because it's present in the test set
# When the recipe is applied to the training set, a column is made for all factor levels come from complete data set (not the training set),
# but this column will contain all zeros if it's missing in the training set. This i called a 'zero-variance predictor' that has no information.
# While some R functions will not produce an error for such predictors, it usually causes warnings and other issues.
# So we apply a step_zv() that will remove columns from the data when the training set data have a single value.

flights_recipe <- flights_recipe %>%
  step_zv(all_predictors())


# use logistic regression to model the flight data
lr_mod <- logistic_reg() %>%
  set_engine("glm")

# Will use our recipe across several steps as we train and test our model by
# 1) Process the recipe using the training set
# 2) Apply the recipe to the training set
# 3) Apply the recipe to the test set


# A model workflow pairs a model and recipe together. Recipes can be used for different models.
flights_wflow <- workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(flights_recipe)

# Train the model
flights_fit <- flights_wflow %>%
  fit(data = train_data)

# Inspect the fit and prepared recipe
flights_fit %>%
  pull_workflow_fit() %>%
  tidy()

flights_fit %>%
  pull_workflow_prepped_recipe()

# Make predictions using our model
predictions <- predict(flights_fit, test_data) # predictions (based on probability > .50)
# The predictions above can also be retrieved with type = 'class' for logistic regression.
# Setting type = NULL, the default, enables it infers it automatically.

prediction_probs <- predict(flights_fit, test_data, type = "prob") # probabilities

flights_pred <- predictions %>%
  bind_cols(prediction_probs) %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

# We can now evaluate the performance of our workflow for example using the area under the ROC curve (True Positive rate v/s False Positive rate).
# The ROC curve uses the class probability estimates to give us a sense of performance across the entire set of potential probability cutoffs. It only works for binary classifiers. It's a probability curve that plots the TPR v/s FPR for various threshold values.

# There is no common convention on which factor level should automatically be considered the "event" or "positive" result
# when computing binary classification metrics. In yardstick, the default is to use the first level.
# To alter this, you can change the argument event_level to 'second' to consider the last level of the factor the level of interest.

# Plot the ROC curve
flights_pred %>%
  roc_curve(truth = arr_delay, .pred_late) %>% # We use .pred_late as the 'positive' result because that's the first level
  autoplot()

# Get area under the ROC curve
# An area of 1 represents a perfect test; an area of .5 represents a worthless test.
# A rough guide for classifying the accuracy of a diagnostic test is the traditional:
#   .90 -   1 = excellent (A)
#   .80 - .90 = good (B)
#   .70 - .80 = fair (C)
#   .60 - .70 = poor (D)
#   .50 - .60 = fail (F)
flights_pred %>%
  roc_auc(truth = arr_delay, .pred_late)

# To check model accuracy, in addition to the ROC curve we can also look at overall classification accuracy.
# Overall accuracy uses the hard class predictions to measure performance.
flights_pred %>%
  accuracy(truth = arr_delay, .pred_class)


# We can also test the workflow WITHOUT our recipe which had the additional factors we added in.
flights_wflow_alt <- workflow() %>%
  add_model(lr_mod) %>%
  add_formula(arr_delay ~ .)

# Remove identification variables
# We have 7 predictor variables and one outcome
train_data_alt <- train_data %>% select(dep_time, origin, dest, air_time, distance, carrier, date, arr_delay)
test_data_alt <- test_data %>% select(dep_time, origin, dest, air_time, distance, carrier, date, arr_delay)

flights_fit_alt <- flights_wflow_alt %>%
  fit(data = train_data_alt)

predictions_alt <- predict(flights_fit_alt, test_data_alt)
prediction_probs_alt <- predict(flights_fit_alt, test_data_alt, type = "prob")

flights_pred_alt <- predictions_alt %>%
  bind_cols(prediction_probs_alt) %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))

flights_pred_alt %>%
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()

# The value of the ROC area under the curve & overall accuracy here is smaller than the earlier fit that used our recipe.
# So we have indeed, marginally, improved the model's prediction power.
flights_pred_alt %>%
  roc_auc(truth = arr_delay, .pred_late)

flights_pred_alt %>%
  accuracy(truth = arr_delay, .pred_class)



####----
# Section 3: Evaluate your model with resampling

####----

# Once we have a model trained, we need a way to measure how well that model predicts new data.

# The main outcome variable of interest for us here is called 'class', which is a factor.
data(cells, package = "modeldata")


# The rates of the classes are somewhat imbalanced; there are more poorly segmented cells than well-segmented cells.
cells %>% count(class) %>%
  mutate(prop = n/sum(n))

# There are different ways to create these partitions of the data for training and test data sets.
# The most common approach is to use a random sample (for example with the rsample package).
# Since the rates of the classes are imbalanced, we can used stratified sampling instead, which
# retains similar proportions in the training & test data sets.
set.seed(123)
# We remove 'case' column from the data set since we are creating our own test and training data set
cell_split <- initial_split(cells %>% select(-case), strata = class)
cell_train <- training(cell_split)
cell_test <- testing(cell_split)

# Confirm the proportions are similar to the full data set
cell_train %>% count(class) %>%
  mutate(prop = n/sum(n))
cell_test %>% count(class) %>%
  mutate(prop = n/sum(n))

# Random forest models are ensembles of decision trees. A large number of decision tree models are created for the ensemble
# based on slightly different versions of the training set. When creating the individual decision trees, the fitting process
# encourages them to be as diverse as possible. The collection of trees are combined into the random forest model and,
# when a new sample is predicted, the votes from each tree are used to calculate the final predicted value for the new sample.

# For categorical outcome variables like class in our cells data example, the majority vote across all the trees
# in the random forest determines the predicted class for the new sample.

# One of the benefits of a random forest model is that it is very low maintenance; it requires very little pre-processing of the data
# and the default parameters tend to give reasonable results.
# At the same time, the number of trees in the ensemble should be large (in the thousands) and
# this makes the model moderately expensive to compute.

# We don't use any recipe here and just use the default parameters for the random forest
rf_mod <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification") # since the outcome is categorical

set.seed(123)
rf_fit <- rf_mod %>%
  fit(class ~ ., data = cell_train)

# Overall accuracy uses the hard class predictions to measure performance.
# The hard class predictions tell us whether our model predicted PS or WS for each cell.
# A simple 50% probability cutoff is used to categorize a cell as poorly segmented (PS).

# Remember to use the test data set when making predictions.
# Models like random forests, neural networks, and other black-box methods can essentially memorize the training set.
# Re-predicting that same set should always result in nearly perfect results.
# The training set does not have the capacity to be a good arbiter of performance.
# It is not an independent piece of information; predicting the training set can only reflect what the model already knows.

# The results for the test set are more realistic
rf_pred <- predict(rf_fit, cell_test) %>%
  bind_cols(predict(rf_fit, cell_test, type = "prob")) %>%
  bind_cols(cell_test %>% select(class))

rf_pred %>%
  roc_auc(truth = class, .pred_PS)

rf_pred %>%
  accuracy(truth = class, .pred_class)


# Resampling methods, such as cross-validation and the bootstrap, are empirical simulation systems.
# They create a series of data sets similar to the training/testing split discussed previously; a subset of the data
# are used for creating the model and a different subset is used to measure performance.
# Resampling is always used with the training set.

set.seed(345)
# Use a 10-fold cross-validation resampling technique
cell_folds <- vfold_cv(cell_train, v = 10)

# Examine individual split's analysis & assessment sets
analysis(cell_folds$splits[[1]])
assessment(cell_folds$splits[[1]])

# There are several options for building an object for resampling:
# - Resample a model specification preprocessed with a formula or recipe, or
# - Resample a workflow() that bundles together a model specification and formula/recipe.

# Create a workflow but without a recipe
rf_wflow <- workflow() %>%
  add_model(rf_mod) %>%
  add_formula(class ~ .)

set.seed(456)
rf_fit_resampled <- rf_wflow %>%
  fit_resamples(cell_folds) # this method is in the 'tune' package

# Inspect individual metrics for each fold
rf_fit_resampled$.metrics

# Collect aggregate metrics
collect_metrics(rf_fit_resampled)
# These performance metrics are now more realistic (i.e. lower) than our computing performance metrics earlier.
# If we wanted to try different model types for this data set, we could more confidently compare performance metrics
# computed using resampling to choose between models.
# The performance metrics from the test set are also much closer to the performance metrics computed using this resampling.

# Resampling allows us to simulate how well our model will perform on new data, and
# the test set acts as the final, unbiased check for our model’s performance.




####----
# Section 4: Tune model parameters

####----

# Some model parameters cannot be learned directly from a data set during model training; these kinds of parameters
# are called hyperparameters. Some examples of hyperparameters include the number of predictors
# that are sampled at splits in a tree-based model or the learning rate in a boosted tree model.

# Instead of learning these kinds of hyperparameters during model training, we can estimate the best values for these
# values by training many models on resampled data sets and exploring how well all these models perform. This process is called tuning.

library(vip)


# Random forest models are a tree-based ensemble method, and typically perform well with default hyperparameters.
# However, the accuracy of some other tree-based models, such as boosted tree models or decision tree models,
# can be sensitive to the values of hyperparameters.

# There are several hyperparameters for decision tree models that can be tuned for better performance
# e.g. the complexity parameter (or cost complexity) and the tree depth.

# Tuning these hyperparameters can improve model performance because decision tree models are prone to overfitting.
# Tuning the value of cost_complexity helps by pruning back our tree. It adds a cost, or penalty, to error rates of
# more complex trees; a cost closer to zero decreases the number tree nodes pruned and is more likely to result
# in an overfit tree. However, a high cost increases the number of tree nodes pruned and can result in the
# opposite problem—an underfit tree.
# Tuning tree_depth, on the other hand, helps by stopping our tree from growing after it reaches a certain depth.

# Create a decision tree model whose hyperparameters we will tune
dt_mod <- decision_tree(cost_complexity = tune(),
                        tree_depth = tune()) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# After the tuning process, we will select a single numeric value for each of these hyperparameters.
# We can create a regular grid of values to try using some convenience functions for each hyperparameter
dt_grid <- grid_regular(cost_complexity(), tree_depth(), levels = 5) # returns 5 x 5 combinations since we have two tuned parameters

# We can train many models using resampled data and see which models turn out best.
set.seed(345)
tree_wflow <- workflow() %>%
  add_model(dt_mod) %>%
  add_formula(class ~ .)

dt_tuned_resampled <- tree_wflow %>%
  tune_grid(resamples = cell_folds, grid = dt_grid)

# Collect aggregate metrics to view the metrics for all the candidate models
collect_metrics(dt_tuned_resampled)

# Visualize the different models
# We can see that our 'stubbiest' tree, with a depth of 1, is the worst model according to both metrics
# and across all candidate values of cost_complexity.
collect_metrics(dt_tuned_resampled) %>%
  mutate(tree_depth = factor(tree_depth)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_x_log10(labels = scales::label_number()) +
  facet_wrap(~ .metric, scales = "free", nrow = 2)


# Get best tree (top 5 candidates)
dt_tuned_resampled %>%
  show_best("roc_auc")
dt_tuned_resampled %>%
  show_best("accuracy")

# These are the values for tree_depth and cost_complexity that maximize AUC in this data set of cell images
dt_best <- dt_tuned_resampled %>%
  select_best("roc_auc")

best_tree_wflow <- tree_wflow %>%
  finalize_workflow(dt_best)

dt_fit <- best_tree_wflow %>%
  fit(data = cell_train)

# Inspect the fit
dt_fit %>%
  pull_workflow_fit()

# Visualize variables that are important in this final model
dt_fit %>%
  pull_workflow_fit() %>%
  vip()

# Use last_fit() with our finalized model to fit the finalized model on the full training data set and
# evaluates the finalized model on the testing data.
dt_final_fit <- best_tree_wflow %>%
  last_fit(cell_split)

# Collect aggregate metrics
collect_metrics(dt_final_fit)

# Inspect ROC area under the curve and overall accuracy
collect_predictions(dt_final_fit) %>%
  roc_auc(class, .pred_PS)
collect_predictions(dt_final_fit) %>%
  accuracy(class, .pred_class)

# Visualize the ROC curve
collect_predictions(dt_final_fit) %>%
  roc_curve(class, .pred_PS) %>%
  autoplot()




####----
# Section 5: A predictive modeling case study

####----

# Goal is to build a model to predict which actual hotel stays included children and/or babies, and which did not.

hotels <- read_csv('https://tidymodels.org/start/case-study/hotels.csv') %>%
  mutate_if(is.character, as.factor)

# There is severe class imbalance but we the data as is as use stratified random sample.
# However, there are techniques in recipes to 'upsample' or 'downsample' for dealing with the imbalance itself.
hotels %>%
  count(children) %>%
  mutate(prop = n/sum(n))

set.seed(123)
hotels_split <- initial_split(hotels, strata = children)
hotels_train <- training(hotels_split)
hotels_test <- testing(hotels_split)


hotels_train %>%
  count(children) %>%
  mutate(prop = n/sum(n))

hotels_test %>%
  count(children) %>%
  mutate(prop = n/sum(n))

# Rather than using multiple iterations of resampling with k-hold cross-validation, create a resample called a validation set.
set.seed(234)
hotels_validation <- validation_split(hotels_train, strata = children, prop = 0.80) # 20% of the data goes into a validation set

# Since the outcome variable children is categorical, logistic regression would be a good first model to start.
# The generalized linear model via penalized maximum likelihood method of estimating the logistic regression slope parameters
# uses a penalty on the process so that less relevant predictors are driven towards a value of zero.
# One of the glmnet penalization methods, called the lasso method, can actually set the predictor slopes
# to zero if a large enough penalty is used


# Setting mixture to a value of one means that the glmnet model will potentially remove irrelevant predictors and choose a simpler model
lr_penalized_mod <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")


holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter",
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

hotels_recipe <- recipe(children ~ ., data = hotels_train) %>%
  step_date(arrival_date, features = c("dow", "month", "year")) %>%
  step_holiday(arrival_date, holidays = holidays) %>%
  step_rm(arrival_date) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>% # This is important because, for penalized models, the predictors should be centered and scaled
  step_normalize(all_predictors()) # centers and scales numeric variables

lr_penalized_wflow <- workflow() %>%
  add_model(lr_penalized_mod) %>%
  add_recipe(hotels_recipe)

# Since we have only one hyperparameter to tune here, we can set the grid up manually with 30 candidate values
lr_penalized_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_penalized_tuned_resampled <- lr_penalized_wflow %>%
  tune_grid(resamples = hotels_validation,
            grid = lr_penalized_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc, accuracy)) # this is the default as well for classification models


# Collect aggregate metrics to view the metrics for all the candidate models
collect_metrics(lr_penalized_tuned_resampled)

# Visualize the different models.
# Model performance is generally better at the smaller penalty values.
# We also see a steep drop in the area under the ROC curve towards the highest penalty values.
# This happens because a large enough penalty will remove *all* predictors from the model (ROC area = 0.5 means no better than predicting by chance)
collect_metrics(lr_penalized_tuned_resampled) %>%
  ggplot(aes(penalty, mean)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_log10(labels = scales::label_number()) +
  facet_wrap(~ .metric, scales = "free", nrow = 2)

# Get best tree (top 15 candidates)
lr_penalized_tuned_resampled %>%
  show_best("roc_auc", n = 15) %>%
  arrange(penalty)

# Every candidate model in this tibble likely includes more predictor variables than the model in the row below it.
# View the numerically best one
lr_penalized_tuned_resampled %>%
  select_best("roc_auc")

# However, we may want to choose a penalty value further along the x-axis, closer to where we
# start to see the decline in model performance, so that it effectively the same performance as the numerically best model,
# but might eliminate more predictors e.g. For example, candidate model 12
lr_penalized_best <- lr_penalized_tuned_resampled %>%
  show_best("roc_auc", n = 15) %>%
  arrange(penalty) %>%
  slice(12)

# Visualize the ROC curve directly from the tuned model since we saved the predictions with them
collect_predictions(lr_penalized_tuned_resampled) %>%
  roc_curve(children, .pred_children) %>%
  autoplot()

# Inspect ROC area under the curve and overall accuracy
collect_predictions(lr_penalized_tuned_resampled) %>%
  roc_auc(children, .pred_children)
collect_predictions(lr_penalized_tuned_resampled) %>%
  accuracy(children, .pred_class)

# ROC curve for predictions corresponding to the chosen best model
lr_penalized_auc <- collect_predictions(lr_penalized_tuned_resampled, parameters = lr_penalized_best) %>%
  roc_curve(children, .pred_children) %>%
  mutate(model = "Penalized Logistic Regression")

autoplot(lr_penalized_auc)

# An effective and low-maintenance modeling technique is a random forest. Compared to logistic regression, a random forest model
# is more flexible. A random forest is an ensemble model typically made up of thousands of decision trees, where each individual tree
# sees a slightly different version of the training data and learns a sequence of splitting rules to predict new data.
# Each tree is non-linear, and aggregating across trees makes random forests also non-linear but more robust and stable compared to
# individual trees. Tree-based models like random forests require very little preprocessing and can effectively handle
# many types of predictors (sparse, skewed, continuous, categorical, etc.)

# Random forest models can be computationally expensive to train and to tune.
# The tune package can do parallel processing for you, and allows users to use multiple cores or separate machines to fit models.
# But, here we are using a single validation set, so parallelization isn’t an option using the tune package.
# For this specific case study, a good alternative is provided by the engine itself. The ranger package offers a
# built-in way to compute individual random forest models in parallel.

cores <- parallel::detectCores()/4

rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger", num.threads = cores) %>%
  set_mode("classification") # since the outcome is categorical

# if you use any other resampling method, let tune do the parallel processing ins\tead of relying on the modeling engine

# Shows which parameters will be tuned.
# The mtry hyperparameter sets the number of predictor variables that each node in the decision tree 'sees' and can learn about,
# so it can range from 1 to the total number of features present; when mtry = all possible features, the model is
# the same as bagging decision trees.
# The min_n hyperparameter sets the minimum n to split at any node.
parameters(rf_mod)

hotels_recipe <- recipe(children ~ ., data = hotels_train) %>%
  step_date(arrival_date, features = c("dow", "month", "year")) %>%
  step_holiday(arrival_date, holidays = holidays) %>%
  step_rm(arrival_date)

rf_wflow <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(hotels_recipe)

set.seed(345)
# We tune with 25 candidate models with space-filling design
rf_tuned_resampled <- rf_wflow %>%
  tune_grid(resamples = hotels_validation,
            grid = 25,
            control = control_grid(save_pred= TRUE),
            metrics = metric_set(roc_auc))

# these values for area under the ROC look more promising than our top model using penalized logistic regression
rf_tuned_resampled %>%
  show_best("roc_auc")

# The plot shows that we should pick small values for the two tuning parameters
rf_tuned_resampled %>%
  autoplot()

rf_best <- rf_tuned_resampled %>%
  select_best("roc_auc")

# all predictions
collect_predictions(rf_tuned_resampled)

# predictions corresponding to the best model
collect_predictions(rf_tuned_resampled, parameters = rf_best)

# plot the ROC curve
collect_predictions(rf_tuned_resampled, parameters = rf_best) %>%
  roc_curve(children, .pred_children) %>%
  autoplot()

# ROC area under the curve
collect_predictions(rf_tuned_resampled, parameters = rf_best) %>%
  roc_auc(children, .pred_children)

# ROC curve for predictions corresponding to the chosen best model
rf_auc <- collect_predictions(rf_tuned_resampled, parameters = rf_best) %>%
  roc_curve(children, .pred_children) %>%
  mutate(model = "Random Forest")

# Compare the validation set ROC curves for our top penalized logistic regression model and random forest models
# The random forest is uniformly better across event probability thresholds.
bind_rows(lr_penalized_auc, rf_auc) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path() + # geom_path() connects the observations in the order in which they *appear* in the data
  geom_abline(linetype = 3) +
  coord_equal()

# Use last_fit() with our finalized model to fit the finalized model on the full training data set and
# evaluates the finalized model on the testing data.
best_rf_wflow <- rf_wflow %>%
  finalize_workflow(rf_best)

rf_final_fit <- best_rf_wflow %>%
  last_fit(hotels_split)

# This ROC AUC value is pretty close to what we saw when we tuned the random forest model with the validation set
collect_metrics(rf_final_fit)


# Rebuild our model object using the best hyperparameter values from our random forest model.
# When we set the engine, we add a new argument: importance = "impurity". This will provide
# variable importance scores for this last model, which gives some insight
# into which predictors drive model performance.
rf_mod_final <- rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = 1000) %>%
  set_engine("ranger", num.threads = cores, importance = "impurity") %>%
  set_mode("classification")

# update the original workflow with the final model
best_rf_wflow <- rf_wflow %>%
  update_model(rf_mod_final)

set.seed(345)
rf_final_fit <- best_rf_wflow %>%
  last_fit(hotels_split)

# This ROC AUC value is pretty close to what we saw when we tuned the random forest model with the validation set
collect_metrics(rf_final_fit)

rf_final_fit %>%
  pluck(".workflow", 1) %>% # pull first list element from the list column named .workflow. This is a workflow object.
  pull_workflow_fit() %>%
  vip(num_features = 20)

# Plot ROC curve
collect_predictions(rf_final_fit) %>%
  roc_curve(children, .pred_children) %>%
  autoplot()
