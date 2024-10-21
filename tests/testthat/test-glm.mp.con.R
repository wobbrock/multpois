# expect_equal
# expect_error
# expect_match
# expect_true
# expect_false

###
test_that("require the pairwise keyword", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  m = glm.mp(Y ~ X, data=df)
  expect_error(glm.mp.con(m, ~ X, adjust="none"), "'pairwise' is required on the left hand side of the ~ .")
})

###
test_that("require a glm model", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b)),
    Z = round(rnorm(60, mean=200, sd=40), digits=2)
  )
  m = stats::lm(Z ~ X, data=df)
  expect_error(glm.mp.con(m, pairwise ~ X, adjust="none"), "'model' must be created by glm.mp.")
})

###
test_that("require a model with an alt factor", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  m = glm(Y ~ X, data=df, family=binomial)
  expect_error(glm.mp.con(m, pairwise ~ X, adjust="none"), "'model' must be created by glm.mp.")
})

###
test_that("ensure all contrast terms are in model", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = factor(c(rep("c",20), rep("d",20), rep("e",20))),
    X3 = factor(c(rep("f",15), rep("g",15), rep("h",15), rep("i",15))),
    Y = factor(c(a,b))
  )
  m = glm.mp(Y ~ X1*X2, data=df)
  expect_error(glm.mp.con(m, pairwise ~ X3, adjust="none"), "'formula' terms must be present in 'model'.")
})

###
test_that("ensure contrast terms are factors", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = round(rnorm(60, mean=40, sd=10), digits=2),
    Y = factor(c(a,b))
  )
  m = glm.mp(Y ~ X1*X2, data=df)
  expect_error(glm.mp.con(m, pairwise ~ X1*X2, adjust="none"), "'formula' terms must be factors:")
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
test_that("match p-values for between-Ss. contrast", {
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
  c1 = emmeans::emmeans(m1, pairwise ~ X, adjust="none")
  c2 = glm.mp.con(m2, pairwise ~ X, adjust="none")
  expect_true(abs(as.data.frame(c1$contrasts)$p.value - c2$contrasts$p.value) <= 0.05)
})

