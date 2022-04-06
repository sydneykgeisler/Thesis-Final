## code to prepare `DATASET` dataset goes here

Thatch <- as.factor(c(rep(2, 16), rep(5, 16), rep(8, 16)))
NSource <- rep(c(rep("AmmSulph", 4), rep("IBDU", 4), rep("SCUrea", 4),
                 rep("Urea", 4)), 3)
estY <- c(rep(6, 12), rep(4, 4), rep(c(rep(6, 4), rep(7, 4), rep(8, 4),
                                       rep(5, 4)), 2))
Field <- as.factor(rep(1:4, 12))
ex <- cbind.data.frame(Thatch, NSource, estY, Field)

usethis::use_data(ex, overwrite = TRUE)


