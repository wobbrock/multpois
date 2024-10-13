## code to prepare `.\inst\extdata\icecream.csv` dataset goes here
icecream <- read.csv(".\\inst\\extdata\\icecream.csv")
icecream$PId = factor(icecream$PId)
icecream$Pref = factor(icecream$Pref)
icecream$Age = factor(icecream$Age)
icecream$Season = factor(icecream$Season)
contrasts(icecream$Age) <- "contr.sum"
contrasts(icecream$Season) <- "contr.sum"
usethis::use_data(icecream, overwrite=TRUE)
