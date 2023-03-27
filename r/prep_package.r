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
# usethis::use_package("e1071") 
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
fit <- run_itr(outcome = outcomes,
               treatment = treatment,
               covariates = covariates,
               data = star_df,
               algorithms = c("caret"),
               plim = 0.2,
               ratio = 0.7,
              # n_folds = 5,
              #  trainControl_method = "repeatedcv",
              trainControl_method = "none",
              train_method = "gbm")
              # number = 3,
              # repeated ten times
              # repeats = 3)

est <- estimate_itr(fit)

summary(est)
plot(est)

?train

?trainControl

fit %>% str

debugonce(run_itr)
debugonce(itr_single_outcome)
debugonce(run_caret)
debugonce(train_caret)

debugonce(estimate_itr)
debugonce(compute_qoi)


##
## cross validation
##

outcomes <- c("g3tlangss")
            #     "g3treadss","g3tmathss")

star_df = star %>% select(-c(g3treadss,g3tmathss)) %>% rename(Y = g3tlangss)

fitControl <- caret::trainControl(method = "none")


formula = as.formula(paste("Y ~ (", paste0(covariates, collapse = "+"), ")*treatment"))

fit <- caret::train(formula, data = star_df, 
                method = "gbm", 
                trControl = fitControl,
                ## This last option is actually one
                ## for gbm() that passes through
                verbose = FALSE)

test_t0_data = star_df %>% mutate(treatment = 0)
test_t1_data = star_df %>% mutate(treatment = 1)

total_t0_data = star_df %>% mutate(treatment = 0)
total_t1_data = star_df %>% mutate(treatment = 1)


## predict 
Y0t_test = predict(
  fit, 
  test_t0_data,
  type = "raw")
Y1t_test = predict(
  fit, 
  test_t1_data,
  type = "raw")

tau_test = Y1t_test - Y0t_test
plim = 0.2

## compute quantities of interest 
That     =  as.numeric(tau_test > 0)
That_p   = numeric(length(That))
That_p[sort(tau_test,decreasing =TRUE,index.return=TRUE)$ix[1:(floor(plim*length(tau_test))+1)]] = 1

## output 
cf_output <- list(
  tau      = tau_test,
  tau_cv   = tau_test, 
  That_cv  = That, 
  That_pcv = That_p
  )








# estimate ITR 
set.seed(2021)
fit <- run_itr(outcome = outcomes,
               treatment = treatment,
               covariates = covariates,
               data = star,
               algorithms = c("caret"),
               plim = 0.2,
               n_folds = 5)

est <- estimate_itr(fit)

summary(est)
plot(est)



debugonce(plot.itr)

?AUPECcv

  graphLabels <- data.frame(
    type = algorithms,
    Pval = map(
      fit[[m]]$AUPEC, ~.x$outputdf) %>%
      bind_rows() %>%
      mutate(Pval = paste0("AUPEC = ", round(aupec, 2), " (s.e. = ", round(sd, 2), ")")) %>% pull(Pval))

  Tcv = data %>% pull(treatment) %>% as.numeric()
  Ycv = data %>% pull(outcome) %>% as.numeric()

  bind_rows(map(fit[[m]]$AUPEC, ~.x$outputdf)) %>% 
    mutate(type = algorithms) %>%
    inner_join(bind_rows(
      map(fit[[m]]$AUPEC, ~.x$outputdf)),
      by = "type"
    ) %>%
    mutate(AUPECmin = aupec.y - 1.96*sd.y,
          AUPECmax = aupec.y + 1.96*sd.y) %>%
    rename(aupec = aupec.y) -> data



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
## for continous outcomes
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

##
## for binary outcomes
##


# binary outcomes
algs <- c(
                  "causal_forest", 
                  # "bartc",
                  "bart",
                  # "svm",
                  "lasso",
                  "boost", 
                  "random_forest",
                  "bagging",
                  "cart"
                  )

system.time(
fit_star <- run_itr(
               outcome = "g3tlangss_binary",
               treatment = "treatment",
               covariates = covariates,
               data = star_new,
               algorithms = algs,
               plim = 0.2,
               n_folds = 2))

# compute qoi
est_itr <- estimate_itr(
            fit = fit_star,
            outcome = "g3tlangss_binary",
            algorithms = algs)            

# get estimates
summary(est_itr)

# plot aupec for binary outcomes
plot(x = est_itr, 
      outcome = "g3tlangss_binary",
      treatment = "treatment",
      data = star_new, 
      algorithms = algs)





