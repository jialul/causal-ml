# https://r-pkgs.org/
library(devtools)
library(roxygen2)
library(testthat)

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/quarto/bin/tools")

# Testing package
devtools::document()
devtools::check()

devtools::load_all()

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

# star_new$g3tlangss_binary %>% class

covariates <-  star %>% dplyr::select(-c(all_of(outcomes),"treatment")) %>% colnames()

##
## for continous outcomes
##

algs <- c(
                  # "causal_forest", 
                  # "bartc",
                  # "bart",
                  # "svm",
                  # "lasso",
                  # "boost", 
                  "random_forest")
                  # "bagging",
                  # "cart")

# train models
system.time(
fit_star <- run_itr(
               outcome = "g3tlangss",
               treatment = "treatment",
               covariates = covariates,
               data = star,
               algorithms = algs,
               plim = 0.2,
               n_folds = 3)
)

# compute qoi
est_itr <- estimate_itr(
            fit = fit_star,
            outcome = "g3tlangss",
            algorithms = c(
                        "causal_forest", 
                        # "bartc",
                        # "bart",
                        # "svm",
                        # "lasso",
                        # "boost", 
                        # "random_forest",
                        # "bagging",
                        "cart"))


# get estimates
summary(est_itr)

# plot aupec for continous outcomes
plot(x = est_itr, 
      outcome = "g3tlangss",
      treatment = "treatment",
      data = star, 
      algorithms = c(
                        "causal_forest", 
                        # "bartc",
                        # "bart",
                        # "svm",
                        # "lasso",
                        # "boost", 
                        # "random_forest",
                        # "bagging",
                        "cart"))

##
## for binary outcomes
##

library(randomForest)
iris %>% 
   mutate(species_binary = ifelse(Species == "setosa", 1,0)) -> iris_new

mod <- rpart(
   formula = species_binary ~ Sepal.Length,
   data = iris_new[1:100,],
   method = "class")

predict(mod,iris_new[101:150,], type = "prob")[, 2]

predict(mod,iris_new[101:150,], type = "prob")[, 2]  %>% class


debugonce(run_itr)          
debugonce(run_bartmachine)
debugonce(test_bart)

  ## outcome
  outcome = testing_data_elements_cart[["data"]][["Y"]]

  if(length(unique(outcome)) > 2){
      ## predict 
      Y0t_total = predict(fit_train, newdata=total_data_elements_cart[["data0t"]])
      Y1t_total = predict(fit_train, newdata=total_data_elements_cart[["data1t"]])

      tau_total=Y1t_total - Y0t_total + runif(n_df,-1e-6,1e-6)
  }else {


#   Y0t_total = predict(
#     fit_train,
#     newdata = total_data_elements_bagging[["data0t"]],
#     type = "prob")[, 2]
#   Y1t_total = predict(
#     fit_train,
#     newdata = total_data_elements_bagging[["data1t"]],
#     type = "prob")[, 2]

# binary outcomes
algs <- c(
                  # "causal_forest", 
                  # "bartc",
                  # "bart",
                  # "svm",
                  # "lasso",
                  # "boost", 
                  # "random_forest",
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


# install the package
devtools::document()
check()
install()

# import pkg  
usethis::use_package("haven") 
usethis::use_tidy_description() 

# write readme file
# usethis::use_data(star, star)
devtools::build_readme()

