## code to prepare `.\inst\extdata\bs2.csv` dataset goes here
bs2 <- read.csv(".\\inst\\extdata\\bs2.csv")
bs2$PId = factor(bs2$PId)
bs2$Y = factor(bs2$Y, levels=c("yes","no"))
bs2$X1 = factor(bs2$X1)
bs2$X2 = factor(bs2$X2)
contrasts(bs2$X1) <- "contr.sum"
contrasts(bs2$X2) <- "contr.sum"
usethis::use_data(bs2, overwrite=TRUE)
