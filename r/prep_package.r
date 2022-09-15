# https://r-pkgs.org/
library(devtools)
library(roxygen2)
library(testthat)

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools")

# Testing package
devtools::document()
devtools::load_all()

# Testing what users will see
devtools::install()


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

star_new <- star %>% 
  mutate(
    g3tlangss_binary = (g3tlangss > mean(g3tlangss))*1)
  mutate(g3tlangss_binary = as_factor(g3tlangss_binary))

covariates <-  star %>% dplyr::select(-c(all_of(outcomes),"treatment")) %>% colnames()

fit_star <- run_itr(outcome = "g3tlangss_binary",
               treatment = "treatment",
               covariates = covariates,
               data = star_new,
               algorithms = c(
                  # "causal_forest", 
                  # "bart",
                  # "svm",
                  # "lasso",
                  # "boost", 
                  # "random_forest",
                  # "bagging",
                  "cart"),
               plim = 0.2,
               n_folds = 3)

debugonce(compute_qoi)
debugonce(run_itr)

outcome = vector()

predict_outcome = predict(fit, kyphosis)

convert_outcome <- function(x, predict_outcome){
  if(predict_outcome[x,1] >= 0.5){
  outcome[x] = colnames(predict_outcome)[1] %>% as.numeric()
}else {
   outcome[x] = colnames(predict_outcome)[2] %>% as.numeric()
}
}

sapply(seq(1:nrow(predict_outcome)),convert_outcome, predict_outcome = predict_outcome)



# get estimates
summary(fit_star)



# plot aupec
plot(x = fit_star, 
      outcome = "g3tlangss_binary",
      treatment = "treatment",
      data = star_new, 
      algorithms = c(
              # "causal_forest",
              # "bart",
              # "svm",
              # "lasso",
              # "boost", 
              # "random_forest",
              # "bagging",
              "cart"))

# install the package
devtools::document()
check()
install()

# import pkg  
usethis::use_package("cli") 
usethis::use_tidy_description() 

# write readme file
# usethis::use_data(star, star)
devtools::build_readme()
