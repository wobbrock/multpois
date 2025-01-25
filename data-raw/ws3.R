## code to prepare `.\inst\extdata\ws3.csv` dataset goes here
ws3 <- read.csv(".\\inst\\extdata\\ws3.csv")
ws3$PId = factor(ws3$PId)
ws3$Y = factor(ws3$Y, levels=c("yes","no","maybe"))
ws3$X1 = factor(ws3$X1)
ws3$X2 = factor(ws3$X2)
contrasts(ws3$X1) <- "contr.sum"
contrasts(ws3$X2) <- "contr.sum"
usethis::use_data(ws3, overwrite=TRUE)
