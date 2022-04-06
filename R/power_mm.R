#' power_mm
#'
#' Calculates the power of a mixed effects model.
#'
#' @param Formula The formula to be passed into blmer.
#' @param Varcomp A vector of variance components to be fixed into the random
#' effects model. Values will fixed based on the order of random effects
#' specified in the formula statement.
#' @param Resid_var The residual variance value to be fixed. If the example does
#' not have a residual variance, type NULL.
#' @param Data The dataset to be used in blmer.
#' @param Family Specifies the distribution of the data.
#' @param Effect The random effect we are most interested in. This will be used
#' to calculate the power.
#' @param Alpha The critical value for the F-statistic. The default is 0.05.
#'
#' @export

power_mm <- function(Formula, Varcomp, Resid_var = NULL, Data, Family, Effect, Alpha) {

  if(Family == "gaussian") {
    b1 <- lme4::lmer(Formula, data = Data)
  } else {
    b1 <- lme4::glmer(Formula, data = Data, family = Family)
  }

  Resid_var_TEMP_notlikely <<- Resid_var

  random_effects <- names(lme4::ranef(b1))
  new_random_effects <- random_effects
  for (k in 1:length(random_effects)) {
    if(is.element(":", unlist(strsplit(random_effects[k], "")))) {
      new_random_effects[k] <- paste("`", random_effects[k], "`", sep = "")
    }
  }

  form_char <- vector("list", length(Varcomp - 1))

  if(is.null(Resid_var_TEMP_notlikely) == TRUE) {
    fn <- function(x, use.var) {
      dnorm(x, sqrt(use.var), 1e-6, log = TRUE)
    }
  } else {
    fn <- function(x, use.var) {
      dnorm(x, sqrt(use.var / Resid_var_TEMP_notlikely), 1e-6, log = TRUE)
    }
  }

  fn.list <- lapply(1:length(Varcomp),
                    function(y) functional::Curry(fn, use.var = Varcomp[y]))

  for (i in 1:length(Varcomp)) {

    x <- 1:i

    custom_list <- rep("custom_prior", times = i)
    var_list <- paste(custom_list, x, sep = "")

    assign(var_list[i], fn.list[[i]])

    form_char[[i]] <- paste(new_random_effects[i], " ~ custom(", var_list[i],
                            ", chol = TRUE", ", scale = 'log')", sep = "")

  }

  form_list <- lapply(form_char, noquote)

  if (Family == "gaussian") {
    if (length(new_random_effects > 1)) {
      b2 <- blme::blmer(Formula, data = Data,
                  resid.prior = point(sqrt(Resid_var_TEMP_notlikely)),
                  cov.prior = form_list)
    } else {
      b2 <- blme::blmer(Formula, data = Data,
                  resid.prior = point(sqrt(Resid_var_TEMP_notlikely)),
                  cov.prior = new_random_effects ~ custom(custom_prior1,
                                                          chol = TRUE,
                                                          scale = "log"))
    }
  } else {
    if (length(new_random_effects > 1)) {
      b2 <- blme::bglmer(Formula, data = Data, family = Family,
                   resid.prior = point(sqrt(Resid_var_TEMP_notlikely)),
                   cov.prior = form_list)
    } else {
      b2 <- blme::bglmer(Formula, data = Data, family = Family,
                   resid.prior = point(sqrt(Resid_var_TEMP_notlikely)),
                   cov.prior = new_random_effects ~ custom(custom_prior1,
                                                           chol = TRUE,
                                                           scale = "log"))
    }
  }

  aov <- anova(b2)
  reff_index <- which(rownames(aov) == Effect)
  f <- aov$`F value`[reff_index]

  noncent_param <- aov$npar[reff_index] * f

  b3 <- lmerTest::lmer(Formula, data = Data)

  aov_b3 <- anova(b3, ddf = "Kenward-Roger")

  reff_index_b3 <- which(rownames(aov_b3) == Effect)

  ndf <- aov$npar[reff_index_b3]

  ddf <- aov_b3$DenDF[reff_index_b3]

  FCrit <- qf(1 - Alpha, ndf, ddf, 0)

  power <- 1 - pf(FCrit, ndf, ddf, noncent_param)

  power

}
