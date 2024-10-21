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
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  mx = matrix(nrow=5, ncol=5)
  expect_error(glm.mp(Y ~ X, data=mx), "'data' must be a long-format data frame.")
})

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
  expect_error(glm.mp( ~ X, data=df), "'formula' must have a dependent variable on the left-hand side.")
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
  expect_error(glm.mp(cbind(Y1,Y2) ~ X, data=df), "'formula' must only have one dependent variable.")
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
  expect_error(glm.mp(Z ~ X, data=df), "'formula' must have a nominal dependent variable of type 'factor'.")
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
  expect_error(glm.mp(Y ~ X + (1|PId), data=df), "'formula' cannot have random factors.")
})

###
test_that("disallow family arguments", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  expect_error(glm.mp(Y ~ X, data=df, family=binomial), "'...' cannot contain a 'family' argument.")
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
  m1 = stats::glm(Y ~ X, data=df, family=binomial)
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
  m1 = stats::glm(Y ~ X, data=df, family=binomial)
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
  m1 = stats::glm(Y ~ X, data=df, family=binomial)
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
  m1 = stats::glm(Y ~ X, data=df, family=binomial)
  m2 = glm.mp(Y ~ X, data=df)
  expect_equal(m1$deviance, m2$deviance)
})

