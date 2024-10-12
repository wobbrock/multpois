# expect_equal
# expect_error
# expect_match
# expect_true
# expect_false

###
test_that("require a dependent variable", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  expect_error(glm.mp( ~ X, data=df), "glm.mp requires a formula with a dependent variable on the left-hand side.")
})

###
test_that("require only one dependent variable", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y1 = factor(c(a,b)),
    Y2 = factor(c(b,a))
  )
  expect_error(glm.mp(cbind(Y1,Y2) ~ X, data=df), "glm.mp is only valid for one dependent variable.")
})

###
test_that("require a nominal dependent variable", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b)),
    Z = round(rnorm(60, mean=200, sd=40), digits=2)
  )
  expect_error(glm.mp(Z ~ X, data=df), "glm.mp is only valid for nominal dependent variables")
})

###
test_that("disallow random factors", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  expect_error(glm.mp(Y ~ X + (1|PId), data=df), "glm.mp is only valid for formulas without random factors.")
})

###
test_that("correctly match model deviances", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  m1 = glm(Y ~ X, data=df, family=binomial)
  m2 = glm.mp(Y ~ X, data=df)
  expect_equal(m1$deviance, m2$deviance)
})

###
test_that("correctly handle unbalanced data", {
  set.seed(123)
  a = sample(c("yes","no"), size=40, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=20, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",40), rep("b",20))),
    Y = factor(c(a,b))
  )
  m1 = glm(Y ~ X, data=df, family=binomial)
  m2 = glm.mp(Y ~ X, data=df)
  expect_equal(m1$deviance, m2$deviance)
})

###
test_that("correctly handle missing rows", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  r = sample.int(nrow(df), 10) # rows to remove
  df = dplyr::filter(.data=df, !dplyr::row_number() %in% r) # remove rows
  m1 = glm(Y ~ X, data=df, family=binomial)
  m2 = glm.mp(Y ~ X, data=df)
  expect_equal(m1$deviance, m2$deviance)
})

###
test_that("correctly handle NA responses", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  r = sample.int(nrow(df), 10) # rows to make NA responses
  df[r,]$Y = NA
  m1 = glm(Y ~ X, data=df, family=binomial)
  m2 = glm.mp(Y ~ X, data=df)
  expect_equal(m1$deviance, m2$deviance)
})

