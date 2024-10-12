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
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  suppressMessages({
    m = glmer.mp(Y ~ X + (1|PId), data=df)
    expect_error(glmer.mp.con(m, ~ X, adjust="none"), "glmer.mp.con requires the 'pairwise' keyword")
  })
})

###
test_that("require a glmerMod model", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b)),
    Z = round(rnorm(60, mean=200, sd=40), digits=2)
  )
  suppressMessages({
    m = lme4::lmer(Z ~ X + (1|PId), data=df)
    expect_error(glmer.mp.con(m, pairwise ~ X, adjust="none"), "glmer.mp.con requires a model created by glmer.mp.")
  })
})

###
test_that("require a model with an alt factor", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  suppressMessages({
    m = lme4::glmer(Y ~ X + (1|PId), data=df, family=binomial)
    expect_error(glmer.mp.con(m, pairwise ~ X, adjust="none"), "glmer.mp.con requires a model created by glmer.mp.")
  })
})

###
test_that("ensure all contrast terms are in model", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = factor(c(rep("c",20), rep("d",20), rep("e",20))),
    X3 = factor(c(rep("f",15), rep("g",15), rep("h",15), rep("i",15))),
    Y = factor(c(a,b))
  )
  suppressMessages({
    m = glmer.mp(Y ~ X1*X2 + (1|PId), data=df)
    expect_error(glmer.mp.con(m, pairwise ~ X3, adjust="none"), "glmer.mp.con requires formula terms to be present in the model.")
  })
})

###
test_that("ensure contrast terms are factors", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = round(rnorm(60, mean=40, sd=10), digits=1),
    Y = factor(c(a,b))
  )
  suppressMessages({
    m = glmer.mp(Y ~ X1*X2 + (1|PId), data=df)
    expect_error(glmer.mp.con(m, pairwise ~ X1*X2, adjust="none"), "glmer.mp.con requires formula terms to be factors")
  })
})

###
test_that("match p-values for within-Ss. contrast", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(rep(1:30, times=2)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b))
  )
  suppressMessages({
    m1 = lme4::glmer(Y ~ X + (1|PId), data=df, family=binomial)
    m2 = glmer.mp(Y ~ X + (1|PId), data=df)
    c1 = emmeans::emmeans(m1, pairwise ~ X, adjust="none")
    c2 = glmer.mp.con(m2, pairwise ~ X, adjust="none")
    expect_true(abs(as.data.frame(c1$contrasts)$p.value - c2$contrasts$p.value) <= 0.05)
  })
})


