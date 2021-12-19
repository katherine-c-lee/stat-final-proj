# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
school_test = read_tsv("data/clean/school_test.tsv") %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                factor)) %>%
  select(-c(ncessch, year, county_code)) %>%
  mutate(crimes_per1000_sqrt = sqrt(crimes_per1000)) %>%
  select(-c(crimes_per1000))

# load all models
load("results/ridge_fit.Rda")
load("results/lasso_fit.Rda")
load("results/elnet_fit.Rda")
load("results/optimal_tree.Rda")
load("results/rf_fit_tuned.Rda")
load("results/gbm_fit_tuned.Rda")

########################### Regression-Based Methods ########################### 
# for benchmark, find RMSE when just predicting the mean of the training set
simple_pred = mean(school_train$crimes_per1000_sqrt)
simple_rmse_sqrt = sqrt(
  mean((simple_pred-(school_test$crimes_per1000_sqrt))^2))
simple_rmse = sqrt(
  mean((simple_pred^2-(school_test$crimes_per1000_sqrt)^2)^2))


lm_RMSE_sqrt = sqrt(
  mean((lm_predictions-(school_test$crimes_per1000_sqrt))^2))
lm_RMSE = sqrt(
  mean((lm_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

# evaluate OLS RMSE
lm_predictions = predict(lm_fit, newdata = school_test)
lm_RMSE_sqrt = sqrt(
  mean((lm_predictions-(school_test$crimes_per1000_sqrt))^2))
lm_RMSE = sqrt(
  mean((lm_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = school_test, 
                            s = "lambda.1se") %>% as.numeric()

ridge_pred_train = predict(ridge_fit, 
                           newdata = school_train, 
                           s = "lambda.1se") %>% as.numeric()

ridge_RMSE = sqrt(
  mean((ridge_predictions-(school_test$crimes_per1000_sqrt))^2))

ridge_RMSE_fix = sqrt(
  mean((ridge_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

school_train %>%
ggplot(aes(x = crimes_per1000_sqrt, y = ridge_pred_train)) +
  geom_point() +
  geom_abline(slope = 1)

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = school_test, 
                            s = "lambda.1se") %>% as.numeric()
lasso_RMSE = sqrt(
  mean((lasso_predictions-(school_test$crimes_per1000_sqrt))^2))

lasso_RMSE_fix = sqrt(
  mean((lasso_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

# evaluate elnet RMSE
#elnet_predictions = predict(elnet_fit, 
                            #alpha = extract_best_elnet(elnet_fit)$alpha,
                            #newdata = school_test,
                            #s = "lambda.1se") %>% as.numeric()
#elnet_rmse = sqrt(mean((elnet_predictions - school_test$crimes_per1000_sqrt)^2))
#elnet_rmse_fix = sqrt(
  #mean((elnet_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

# Summary of regression methods
tibble(Method = c("OLS", "Intercept-Only", "Ridge", "Lasso"), 
       `Test RMSE (Sqrt Transformed)` = 
         c(lm_RMSE_sqrt, simple_rmse_sqrt, ridge_RMSE, lasso_RMSE),
       `Test RMSE` = 
         c(lm_RMSE, simple_rmse, ridge_RMSE_fix, lasso_RMSE_fix)) %>%
  write_tsv("results/model-evaluation-regression.tsv")

##################### Tree-based methods ######################################

# evaluate tree RMSE
tree_pred_sqrt = predict(optimal_tree, newdata = school_test)
tree_rmse_sqrt = sqrt(
  mean((tree_pred_sqrt-school_test$crimes_per1000_sqrt)^2))
tree_RMSE = sqrt(
  mean((tree_pred_sqrt^2-(school_test$crimes_per1000_sqrt)^2)^2))

# evaluate Random Forest RMSE
rf_predictions = predict(rf_fit_tuned, newdata = school_test)
rf_RMSE_sqrt = sqrt(mean((rf_predictions-school_test$crimes_per1000_sqrt)^2))
rf_RMSE = sqrt(mean((rf_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

# evaluate Boosting RMSE
# n.trees = 242 comes from files 5-tree-modeling, line 201
gbm_predictions = predict(gbm_fit_tuned, n.trees = 242, 
                              newdata = school_test) 
gbm_RMSE_sqrt = sqrt(mean((gbm_predictions-school_test$crimes_per1000_sqrt)^2))
gbm_RMSE = sqrt(mean((gbm_predictions^2-(school_test$crimes_per1000_sqrt)^2)^2))

tibble(Method = c("Regression Tree", "Random Forest", "Boosting"), 
       `Test RMSE (Sqrt Transformed)` = 
         c(tree_rmse_sqrt, rf_RMSE_sqrt, gbm_RMSE_sqrt),
       `Test RMSE` = c(tree_RMSE, rf_RMSE, gbm_RMSE)) %>%
  write_tsv("results/model-evaluation-tree.tsv")

################# General Evaluation ########################################

tibble(Method = 
         c("OLS", "Intercept-Only", "Ridge", "Lasso", 
           "Tree", "Random Forest", "Boosting"), 
       `Test RMSE` = c(lm_RMSE, simple_rmse, ridge_RMSE_fix, lasso_RMSE_fix,
                       tree_RMSE, rf_RMSE, gbm_RMSE)) %>%
  write_tsv("results/model-evaluation-total.tsv")
