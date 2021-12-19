# load libraries
library(cowplot)                                  # for side by side plots
library(glmnetUtils)                              # to run ridge and lasso
source("code/functions/plot_glmnet.R")            # for lasso/ridge trace plots
library(tidyverse)

# read in cleaned data
school_train = read_tsv("data/clean/school_train.tsv") %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                factor)) %>%
  select(-c(ncessch, year, county_code)) %>%
  filter(crimes_per1000 < max(crimes_per1000)) %>%
  mutate(crimes_per1000_sqrt = sqrt(crimes_per1000)) %>%
  select(-c(crimes_per1000)) %>%

###################################### OLS #####################################
lm_fit = lm(formula = crimes_per1000_sqrt ~ ., data = school_train)
summary(lm_fit)

############################### RIDGE REGRESSION ###############################
# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(crimes_per1000_sqrt ~ .,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = school_train, 
                      show_col_types = FALSE)
plot(ridge_fit)
coef(ridge_fit, s = "lambda.1se") %>% head()
coef(ridge_fit, s = "lambda.min") %>% head()
save(ridge_fit, file = "results/ridge_fit.Rda")

############################### LASSO REGRESSION ###############################
# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(crimes_per1000_sqrt ~ .,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = school_train,
                      show_col_types = FALSE)

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, school_train, features_to_plot = 6, 
                lambda = lasso_fit$lambda.min)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 7, 
       height = 5)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, school_train, 
                                 lambda = lasso_fit$lambda.min)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/lasso-features-table.tsv")

################################# ELASTIC NET ##################################
set.seed(1)
elnet_fit = cva.glmnet(crimes_per1000_sqrt ~ .,  # formula notation, as usual
                       nfolds = 10,               # number of folds
                       data = school_train)   # data to run on
# reduces to lasso
elnet_fit$alpha
plot_cva_glmnet(elnet_fit)
elnet_fit_best = extract_best_elnet(elnet_fit)
elnet_fit_best$alpha
plot(elnet_fit_best)
e_p = plot_glmnet(elnet_fit_best, school_train, features_to_plot = 6)
ggsave(filename = "results/elnet-trace-plot.png", 
       plot = e_p, 
       device = "png", 
       width = 6, 
       height = 4)
# save elnet fit object
save(elnet_fit, file = "results/elnet_fit.Rda")
