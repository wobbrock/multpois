## code to prepare `.\inst\extdata\bs3.csv` dataset goes here
bs3 <- read.csv(".\\inst\\extdata\\bs3.csv")
bs3$PId = factor(bs3$PId)
bs3$Y = factor(bs3$Y)
bs3$X1 = factor(bs3$X1)
bs3$X2 = factor(bs3$X2)
contrasts(bs3$X1) <- "contr.sum"
contrasts(bs3$X2) <- "contr.sum"
usethis::use_data(bs3, overwrite=TRUE)
