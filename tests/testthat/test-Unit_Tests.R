test_that("SAS Power Calculations Match power_mm Calculations", {

  estY_edit <- rep(c(rep(6, 12), rep(4, 4)), 3)
  epsilon <- seq(from = 0, to = 2, by = 0.1)

  eps_vec <- vector("list", length(epsilon))
  ex_diff <- vector("list", length(epsilon))
  power_mat <- matrix(NA, nrow = length(epsilon), ncol = 1)

  for (i in 1:length(epsilon)) {

    eps_vec[[i]] <- c(rep(0, times = 20), rep(epsilon[i], times = 4),
                      rep(2 * epsilon[i], times = 4), rep(epsilon[i], times = 4),
                      rep(0, times = 4), rep(epsilon[i], times = 4),
                      rep(2 * epsilon[i], times = 4), rep(epsilon[i], times = 4))
    ex_diff[[i]] <- cbind.data.frame(ex$Thatch, ex$NSource, estY_edit +
                                       eps_vec[[i]], ex$Field)
    colnames(ex_diff[[i]]) <- c("Thatch", "NSource", "estY", "Field")

    power_mat[i] <- power_mm(Formula = estY ~ NSource*Thatch + (1 | Field) +
                               (1|Field:NSource), Varcomp = c(0.008, 0.07),
                             Resid_var = 0.2, Data = ex_diff[[i]],
                             Family = "gaussian", Effect = "NSource:Thatch",
                             Alpha = 0.05)

  }

  power_mat <- round(power_mat, digits = 5)

  SAS_power <- matrix(c(0.05000, 0.05831, 0.08594, 0.13989, 0.22762, 0.35080,
                        0.49965, 0.65317, 0.78739, 0.88630, 0.94753, 0.97928,
                        0.99304, 0.99802, 0.99952, 0.99990, 0.99998, 1.00000,
                        1.00000, 1.00000, 1.00000),
                      nrow = length(epsilon), ncol = 1)
  expect_equal(SAS_power, power_mat)
})
