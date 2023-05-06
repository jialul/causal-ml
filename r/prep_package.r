# https://r-pkgs.org/
library(devtools)
library(roxygen2)
library(testthat)

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")

# Testing package
devtools::document()
devtools::load_all()

# install the package
devtools::check()
install()

# import pkg  
usethis::use_package("rlearner") 
usethis::use_tidy_description() 

# write readme file
# usethis::use_data(star, star)
devtools::build_readme()

##
## sample split
##

library(tidyverse)
library(evalITR)
load("data/star.rda")

# specifying the outcome
# outcomes <- c("g3tlangss","g3treadss","g3tmathss")
outcomes = "g3tlangss"

# specifying the treatment
treatment <- "treatment"

# specifying covariates
covariates <-  star %>% dplyr::select(-c("g3tlangss",
                "g3treadss","g3tmathss","treatment")) %>% 
                colnames()

# data
star_df = star %>% dplyr::select(-c(g3treadss,g3tmathss))

# estimate ITR 

# User input formula
user_formula <- as.formula(paste("g3tlangss ~ ", paste0(covariates, collapse = "+"), " + treatment"))

user_formula

# grid search
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

# fit control
fitControl = caret::trainControl(method = "none")

fitControl <- caret::trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 3,
                           ## repeated ten times
                           repeats = 3)
# fit
fit <- estimate_itr(
               treatment = "treatment",
                form = user_formula,
                # trControl = fitControl,
               data = star_df,
               algorithms = c(
                "lasso", "rf"),
                # "rlasso", "xlasso", "ulasso", "tlasso"),
               budget = 0.2,
               split_ratio = 0.7
              #  preProcess = c("center", "scale")
              #  tuneGrid = gbmGrid,
              #  verbose = FALSE
               )

# test
est <- evaluate_itr(fit)

summary(est)

# test
test_out <- test_itr(fit)

summary(test_out)


# plot GATE estimates
summary(est)$GATE %>% 
  mutate(group = as_factor(group)) %>%
  ggplot(., aes(
    x = group, y = estimate,
    ymin = lower , ymax = upper, color = algorithm)) +
  ggdist::geom_pointinterval(
    width=0.5,    
    position=position_dodge(0.5),
    interval_size_range = c(0.8, 1.5),
    fatten_point = 2.5) +
  theme_bw() +    
  # remove grid
  theme(panel.grid = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Group", y = "GATE estimate") +
  # add dashed line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "#4e4e4e") +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#076f00", "#0072B2")) 

ggsave("man/figures/gate.png", width = 5, height = 4)


fit$estimates$models$gbm$finalModel$trees


debugonce(estimate_itr)
debugonce(itr_single_outcome)
debugonce(run_rlearner)
debugonce(train_rlearner)
debugonce(test_rlearner)


total_data_elements_lasso[["X"]] %>% as.matrix() %>% dim

# get caret model
fit$estimates$models$rf %>% 
  ggplot() + theme_bw() 

ggsave("man/figures/rf.png", width = 5, height = 4)


set.seed(2021)
fit_cv <- estimate_itr(
               treatment = treatment,
               form = user_formula,
               data = star_data,
               algorithms = c("causal_forest"),
               trcontrol = fitControl,
               budget = 0.2,
               n_folds = 3)

# evaluate ITR 
est_cv <- evaluate_itr(fit_cv)

# plot the AUPEC 
plot(est_cv)

ggsave("man/figures/plot_5folds.png", width = 5, height = 4)



##
## use parallel 
## 

library(furrr)
library(future.apply)

library(tidyverse)
library(evalITR)

nworkers <- 4
plan(multisession, workers =nworkers)

star <- readRDS("/Users/alana/Dropbox/research/research projects/ITR/code/causal-ml/data/star/star.rds")

outcomes <- c("g3tlangss",
                "g3treadss","g3tmathss")

covariates <-  star %>% dplyr::select(-c(all_of(outcomes),"treatment")) %>% colnames()




##
## original algorithms
##

algs <- c("causal_forest", 
                  "bartc",
                  # "bart",
                  # "svm",
                  "lasso",
                  # "boost", 
                  "random_forest",
                  "bagging",
                  "cart")



ggsave("plot_5folds.png")
saveRDS(est_itr, "est_5folds.rds")






