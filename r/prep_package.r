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

outcomes <- c("g3tlangss")
# outcomes <- c("g3tlangss",
#                 "g3treadss","g3tmathss")

covariates <-  star %>% dplyr::select(-c(all_of(outcomes),"treatment")) %>% colnames()

fit_star <- run_itr(outcome = outcomes,
               treatment = "treatment",
               covariates = covariates,
               data = star,
               algorithms = c(
                  "causal_forest", 
                  # "bart",
                # #  "svm",
                  # "lasso",
                  # "boost", 
                  # "random_forest",
                  # "bagging",
                  "cart"),
               plim = 0.2,
               n_folds = 3)

class(fit_star)

# fit_star$qoi[[1]]$AUPEC %>%
#   map(.,~as_tibble(.)) %>%
#   bind_rows() 
  
# fit_star$qoi[[1]]$AUPEC %>%
#   {{temp <<-.}} %>%
#   map(., ~.x$outputdf$type %>% unique)

# get PAPE estimates
summary(fit_star, 1, type = "PAPE")

# get PAPE estimates
summary(fit_star, 1, type = "PAPEp")

# get PAPDp estimates
summary(fit_star, 1, type = "PAPDp")

# get AUPEC estimates
summary(fit_star, 1, type = "AUPEC")

plot(x = fit_star, 
      outcome = outcomes[1],
      treatment = "treatment",
      data = star, 
      algorithms = c(
              "causal_forest",
              # "bart",
              # # "svm",
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
