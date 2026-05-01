setwd("C:/Users/andyh/Documents/OMAHASERIES/Code")
library("usethis")
library("gert")
library("gitcreds")

# initial setup
usethis::create_github_token()
gitcreds::gitcreds_set()