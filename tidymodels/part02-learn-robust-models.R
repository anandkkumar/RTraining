####----
# tidymodels: Learn - Create Robust Models
# Based on content published at https://www.tidymodels.org/learn/
####----

####----
# Section 1: Regression models two ways
# Based on content published at https://www.tidymodels.org/learn/models/parsnip-ranger-glmnet/

####----

library(tidymodels)

# Load Ames housing data
data(ames)

set.seed(4595)
# The parameter 'prop' represents the  proportion of data to be retained for modeling/analysis
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.75)

ames_train <- training(data_split)
ames_test <- training(data_split)

rf_model <- rand_forest(mode = "regression")

# The model will be fit with the ranger package by default.
# Since we didn’t add any extra arguments to fit, many of the arguments will be
# set to their defaults from the function ranger::ranger()
translate(rf_model)

predictors <- c("Longitude", "Latitude", "Lot_Area", "Neighborhood", "Year_Sold")

# 'fit_xy' is a the non-formula interface in the parsnip package.
# It doesn’t do anything to the predictors before passing them to the underlying model function.
rf_xy_fit <- rf_model %>%
  set_engine("ranger") %>%
  fit_xy(x = ames_train[, predictors],
         y = log10(ames_train$Sale_Price))

# Use the fit to make predictions on the test data and add it as an additional column
test_results <- ames_test %>%
  select(Sale_Price) %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  bind_cols(
    predict(rf_xy_fit, new_data = ames_test[, predictors]) %>%
      rename(`random forest` = .pred)
  )

# summarize performance
test_results %>% metrics(truth = Sale_Price, estimate = `random forest`)


# When the model it being fit by parsnip, data descriptors are made available.
# These attempt to let you know what you will have available when the model is fit.
# preds() - the number of predictor variables in the data set that are associated
#           with the predictors *prior* to dummy variable creation.
# .cols() - the number of predictor columns *after* dummy variables (or other encodings) are created
rf_model_alt <- rand_forest(mode = "regression", mtry = .preds(), trees = 1000)

rf_fit <- rf_model_alt %>%
  set_engine("ranger") %>%
  fit(log10(Sale_Price) ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
      data = ames_train)

# Use the fit to make predictions on the test data and add it as an additional column
test_results_alt <- ames_test %>%
  select(Sale_Price) %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  bind_cols(
    predict(rf_fit, new_data = ames_test[, predictors]) %>%
      rename(`random forest` = .pred)
  )


# summarize performance
test_results_alt %>% metrics(truth = Sale_Price, estimate = `random forest`)


# Recipes operations:
# The prep() function takes that defined object with *training* data and computes everything so that the preprocessing steps can be executed
# The bake() function takes a prepped recipe (one that has had all quantities estimated from training data) and
# applies it to new_data, which can the training data or the test data (which is the normal workflow)
# The juice() function is a shortcut to bake with the training data.

# Regularized regression: When regularization is used, the predictors should
# first be centered and scaled before being passed to the model
norm_recipe <- recipe(Sale_Price ~ Longitude + Latitude + Lot_Area + Neighborhood + Year_Sold,
                      data = ames_train) %>%
  # step_other creates a specification that will potentially pool infrequently occurring values into an "other" category.
  step_other(Neighborhood) %>%
  step_dummy(all_nominal()) %>%
  # normalizes numeric data to have a mean of zero
  step_center(all_predictors()) %>%
  # normalizes numeric data to have a standard deviation of one
  step_scale(all_predictors()) %>%
  step_log(Sale_Price, base = 10) %>%
  # estimate the means and standard deviations
  prep(training = ames_train, retain = TRUE)


# mixture: A number between zero and one (inclusive) that is the proportion of L1 regularization (i.e. lasso)
# in the model. When mixture = 1, it is a pure lasso model while mixture = 0 indicates
# that ridge regression is being used.
# If penalty were not specified, all of the lambda values would be computed
glmn_fit <- linear_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  fit(Sale_Price ~ ., data = bake(norm_recipe, new_data = NULL))

test_normalized <- bake(norm_recipe, new_data = ames_test, all_predictors())

test_results <- test_results %>%
  bind_cols(
    predict(glmn_fit, new_data = test_normalized) %>%
      rename(glmnet = .pred)
  )

test_results %>% metrics(truth = Sale_Price, estimate = glmnet)

test_results %>%
  gather(model, prediction, -Sale_Price) %>%
  ggplot(aes(x = prediction, y = Sale_Price)) +
  geom_abline(col = "green", linetype = 2) +
  geom_point(alpha = .4) +
  facet_wrap(~model) +
  coord_fixed()
