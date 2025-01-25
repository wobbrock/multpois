## code to prepare `.\inst\extdata\icecream.csv` dataset goes here
icecream <- read.csv(".\\inst\\extdata\\icecream.csv")
icecream$PId = factor(icecream$PId)
icecream$Pref = factor(icecream$Pref, levels=c("vanilla","chocolate","strawberry"))
icecream$Age = factor(icecream$Age, levels=c("adult","child"))
icecream$Season = factor(icecream$Season, levels=c("fall","winter","spring","summer"))
contrasts(icecream$Age) <- "contr.sum"
contrasts(icecream$Season) <- "contr.sum"
usethis::use_data(icecream, overwrite=TRUE)
