# expect_equal
# expect_error
# expect_match
# expect_true
# expect_false

###
test_that("require a data frame", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  mx = matrix(nrow=5, ncol=5)
  expect_error(glmer.mp(Y ~ X + (1|PId), data=mx), "'data' must be a long-format data frame.")
})

###
test_that("require a dependent variable", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  expect_error(glmer.mp( ~ X + (1|PId), data=df), "'formula' must have a dependent variable on the left-hand side.")
})

###
test_that("require only one dependent variable", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y1 = factor(c(a,b)),
    Y2 = factor(c(b,a))
  )
  expect_error(glmer.mp(cbind(Y1,Y2) ~ X + (1|PId), data=df), "'formula' must only have one dependent variable.")
})

###
test_that("require a nominal dependent variable", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b)),
    Z = round(rnorm(60, mean=200, sd=40), digits=2)
  )
  expect_error(glmer.mp(Z ~ X + (1|PId), data=df), "'formula' must have a nominal dependent variable of type 'factor'.")
})

###
test_that("require a random factor", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  expect_error(glmer.mp(Y ~ X, data=df), "'formula' must have at least one random factor")
})

###
test_that("disallow family arguments", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  expect_error(glmer.mp(Y ~ X + (1|PId), data=df, family=binomial), "'...' cannot contain a 'family' argument.")
})

###
test_that("correctly match p-values", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  suppressMessages({
    suppressWarnings({
      m1 = lme4::glmer(Y ~ X + (1|PId), data=df, family=binomial)
      m2 = glmer.mp(Y ~ X + (1|PId), data=df)
      a1 = car::Anova(m1, type=3)
      a2 = Anova.mp(m2, type=3)
      expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`) <= 0.05)
    })
  })
})

###
test_that("correctly handle missing rows", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  r = sample.int(nrow(df), 10) # rows to remove
  df = dplyr::filter(.data=df, !dplyr::row_number() %in% r) # remove rows
  suppressMessages({
    suppressWarnings({
      m1 = lme4::glmer(Y ~ X + (1|PId), data=df, family=binomial)
      m2 = glmer.mp(Y ~ X + (1|PId), data=df)
      a1 = car::Anova(m1, type=3)
      a2 = Anova.mp(m2, type=3)
      expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`) <= 0.05)
    })
  })
})

###
test_that("correctly handle NA responses", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  r = sample.int(nrow(df), 10) # rows to inject NAs into
  df[r,]$Y = NA
  suppressMessages({
    suppressWarnings({
      m1 = lme4::glmer(Y ~ X + (1|PId), data=df, family=binomial)
      m2 = glmer.mp(Y ~ X + (1|PId), data=df)
      a1 = car::Anova(m1, type=3)
      a2 = Anova.mp(m2, type=3)
      expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`) <= 0.05)
    })
  })
})

