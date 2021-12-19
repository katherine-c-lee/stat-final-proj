library(tidyverse)
library(cowplot)

# read in cleaned data
school_train = read_tsv("data/clean/school_train.tsv") %>%
  mutate(across(c("urban_centric_locale", "school_type", "charter"), 
                factor)) %>%
  select(-c(ncessch, year, county_code)) %>%
  filter(crimes_per1000 < max(crimes_per1000)) %>%
  mutate(crimes_per1000_sqrt = sqrt(crimes_per1000)) %>%
  select(-c(crimes_per1000))

############################### REGRESSION TREE ################################  
library(rpart)             # install.packages("rpart")
library(rpart.plot)        # install.packages("rpart.plot")

tree_fit = rpart(crimes_per1000_sqrt ~ ., data = school_train)
printcp(tree_fit)
cp_table = printcp(tree_fit) %>% as_tibble()
tree_fit$variable.importance

# choose the number of terminal nodes that minimize the CV error
treenodes_cv_error <- cp_table %>% 
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()

ggsave(filename = "results/treenodes_cv_error.png", 
       plot = treenodes_cv_error, 
       device = "png", 
       width = 6, 
       height = 4)

# extract the optimal lambda
optimal_tree_info = cp_table %>%
  arrange(xerror) %>%
  head(1)
optimal_tree_info

# finding the optimal pruned tree
optimal_tree = prune(tree_fit, cp = optimal_tree_info$CP)
rpart.plot(optimal_tree) 
save(optimal_tree, file = "results/optimal_tree.Rda")

################################ RANDOM FOREST #################################
library(randomForest)       # install.packages("randomForest")

# train using 5000 observations
n1 = 5000
train_samples1 = sample(1:n1, round(0.8*n1))

## Default random forest
school_train1 <- school_train[train_samples1,]
rf_fit = randomForest(crimes_per1000_sqrt ~ ., data = school_train1)
rf_fit$mtry
plot(rf_fit)

# Tuning the mtry parameter by fitting many random forests
mvalues = seq(1,30, by = 2)
oob_errors = numeric(length(mvalues))
ntree = 500

for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(crimes_per1000_sqrt ~ ., mtry = m, data = school_train1)
  oob_errors[idx] = rf_fit$mse[ntree]
}

p2 = tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  geom_hline(yintercept = min(oob_errors), colour = "blue", 
             linetype = "dashed") +
  theme_bw() +
  labs(y = "OOB Error")

ggsave(filename = "results/m_oob.png", 
       plot = p2, 
       device = "png", 
       width = 6, 
       height = 4)

# refit random forest with tuned parameters
rf_fit_tuned = randomForest(crimes_per1000_sqrt ~ ., 
                            importance = TRUE,
                            ntree = 500,
                            mtry = 11, # from OOB error plot above
                            data = school_train1)
save(rf_fit_tuned, file = "results/rf_fit_tuned.Rda")
plot(rf_fit_tuned)

rf_fit_tuned$importance %>%
  head()
varImpPlot(rf_fit_tuned, n.var = 10)

################################## BOOSTING ####################################
library(gbm)       # install.packages("gbm")

set.seed(1)
gbm_fit = gbm(crimes_per1000_sqrt ~ .,
              distribution = "gaussian",
              n.trees = 500,
              interaction.depth = 1,
              shrinkage = 0.1,
              cv.folds = 5,
              data = school_train1)

opt_num_trees = gbm.perf(gbm_fit)
opt_num_trees

ntrees = 500
tibble(Iteration = 1:ntrees, CV = gbm_fit$cv.error) %>%
  ggplot(aes(x = Iteration, y = CV)) + geom_line() +
  theme_bw()

set.seed(1)
gbm_fit_slow = gbm(crimes_per1000_sqrt ~ .,
                   distribution = "gaussian",
                   n.trees = 500,
                   interaction.depth = 1,
                   shrinkage = 0.01,
                   cv.folds = 5,
                   data = school_train1)
gbm.perf(gbm_fit_slow)

# tuning interaction depth
set.seed(1)
gbm_fit_1 = gbm(crimes_per1000_sqrt ~ .,
                distribution = "gaussian",
                n.trees = 1000,
                interaction.depth = 1,
                shrinkage = 0.1,
                cv.folds = 5,
                data = school_train1)
gbm_fit_2 = gbm(crimes_per1000_sqrt ~ .,
                distribution = "gaussian",
                n.trees = 1000,
                interaction.depth = 2,
                shrinkage = 0.1,
                cv.folds = 5,
                data = school_train1)
gbm_fit_3 = gbm(crimes_per1000_sqrt ~ .,
                distribution = "gaussian",
                n.trees = 1000,
                interaction.depth = 3,
                shrinkage = 0.1,
                cv.folds = 5,
                data = school_train1)  

ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3)
)
cv_errors  

interaction_depth_tuning <- cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  geom_line() + 
  theme_bw() + 
  geom_hline(yintercept = min(gbm_fit_1$cv.error), color = "red", 
             linetype = "dashed") +
  geom_vline(xintercept = which.min(gbm_fit_1$cv.error), linetype = "dotted",
             color = "red") +
  geom_hline(yintercept = min(gbm_fit_2$cv.error), color = "green", 
             linetype = "dashed") +
  geom_vline(xintercept = which.min(gbm_fit_2$cv.error), linetype = "dotted",
             color = "green") +
  geom_hline(yintercept = min(gbm_fit_3$cv.error), color = "blue", 
             linetype = "dashed") +
  geom_vline(xintercept = which.min(gbm_fit_3$cv.error), linetype = "dotted",
             color = "blue") +
  labs(x = "Number of Trees", y = "CV Error")

ggsave(filename = "results/interaction_depth_tuning.png", 
       plot = interaction_depth_tuning, 
       device = "png", 
       width = 7, 
       height = 4)

optimal_trees = which.min(gbm_fit_3$cv.error)
optimal_trees
gbm_fit_tuned = gbm_fit_3

save(gbm_fit_tuned, file = "results/gbm_fit_tuned.Rda")

# boosting model interpretation

optimal_num_trees = gbm.perf(gbm_fit_3, plot.it = FALSE) 

summary(gbm_fit_tuned, 
        n.trees = optimal_num_trees, 
        plotit = FALSE) %>%
  as_tibble() %>%
  head(10) %>%
  rename("Variable" = var, "Relative Influence" = rel.inf) %>%
  write_tsv("results/top-10-features-boosting.tsv")

# partial dependence plots (interaction depth 3)
partial1 = plot(gbm_fit_tuned, i.var = "avg_suspensions", 
                n.trees = optimal_num_trees)
partial2 = plot(gbm_fit_tuned, i.var = "threats_no_weapon_incidents", 
     n.trees = optimal_num_trees)
p_partials = plot_grid(partial1, partial2)
ggsave(filename = "results/partial-dependence.png", 
       plot = p_partials, 
       device = "png", 
       width = 8, 
       height = 3)