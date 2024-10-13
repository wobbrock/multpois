## code to prepare `.\inst\extdata\ws2.csv` dataset goes here
ws2 <- read.csv(".\\inst\\extdata\\ws2.csv")
ws2$PId = factor(ws2$PId)
ws2$Y = factor(ws2$Y)
ws2$X1 = factor(ws2$X1)
ws2$X2 = factor(ws2$X2)
contrasts(ws2$X1) <- "contr.sum"
contrasts(ws2$X2) <- "contr.sum"
usethis::use_data(ws2, overwrite=TRUE)
