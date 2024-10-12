# expect_equal
# expect_error
# expect_match
# expect_true
# expect_false

###
test_that("require a glm or glmerMod model", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X = factor(c(rep("a",30), rep("b",30))),
    Y = factor(c(a,b)),
    Z = round(rnorm(60, mean=200, sd=40), digits=2)
  )
  m = lm(Z ~ X, data=df)
  suppressMessages({
    expect_error(Anova.mp(m), "Anova.mp requires a model created by glm.mp or glmer.mp.")
  })
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
  suppressMessages({
    suppressWarnings({
      m1 = glm(Y ~ X, data=df, family=binomial)
      m2 = lme4::glmer(Y ~ X + (1|PId), data=df, family=binomial)
      expect_error(Anova.mp(m1), "Anova.mp requires a model created by glm.mp or glmer.mp.")
      expect_error(Anova.mp(m2), "Anova.mp requires a model created by glm.mp or glmer.mp.")
    })
  })
})

###
test_that("match p-values for one-way between-Ss.", {
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
  a1 = car::Anova(m1, type=3)
  a2 = Anova.mp(m2, type=3)
  expect_true(abs(a1$`Pr(>Chisq)` - a2$`Pr(>Chisq)`) <= 0.10)
})

###
test_that("match p-values for two-way between-Ss.", {
  set.seed(123)
  ac = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.2, 0.8))
  ad = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.6, 0.4))
  bc = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.4, 0.6))
  bd = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.8, 0.2))
  df = data.frame(
    PId = factor(seq(1, 60, 1)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = factor(rep(c(rep("c",15), rep("d",15)), times=2)),
    Y = factor(c(ac,ad,bc,bd))
  )
  m1 = glm(Y ~ X1*X2, data=df, family=binomial)
  m2 = glm.mp(Y ~ X1*X2, data=df)
  a1 = car::Anova(m1, type=3)
  a2 = Anova.mp(m2, type=3)
  expect_true(abs(a1$`Pr(>Chisq)`[1] - a2$`Pr(>Chisq)`[1]) <= 0.10) #X1
  expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`[2]) <= 0.10) #X2
  expect_true(abs(a1$`Pr(>Chisq)`[3] - a2$`Pr(>Chisq)`[3]) <= 0.10) #X1:X2
})

###
test_that("match p-values for one-way within-Ss.", {
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
      expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`) <= 0.10)
    })
  })
})

###
test_that("match p-values for two-way within-Ss.", {
  set.seed(123)
  ac = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.2, 0.8))
  ad = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.6, 0.4))
  bc = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.4, 0.6))
  bd = sample(c("yes","no"), size=15, replace=TRUE, prob=c(0.8, 0.2))
  df = data.frame(
    PId = factor(rep(1:15, times=4)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = factor(rep(c(rep("c",15), rep("d",15)), times=2)),
    Y = factor(c(ac,ad,bc,bd))
  )
  suppressMessages({
    suppressWarnings({
      m1 = lme4::glmer(Y ~ X1*X2 + (1|PId), data=df, family=binomial)
      m2 = glmer.mp(Y ~ X1*X2 + (1|PId), data=df)
      a1 = car::Anova(m1, type=3)
      a2 = Anova.mp(m2, type=3)
      expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`[1]) <= 0.10) #X1
      expect_true(abs(a1$`Pr(>Chisq)`[3] - a2$`Pr(>Chisq)`[2]) <= 0.10) #X2
      expect_true(abs(a1$`Pr(>Chisq)`[4] - a2$`Pr(>Chisq)`[3]) <= 0.10) #X1:X2
    })
  })
})

###
test_that("match p-values for split-plot design", {
  set.seed(123)
  a = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.7, 0.3))
  b = sample(c("yes","no"), size=30, replace=TRUE, prob=c(0.3, 0.7))
  df = data.frame(
    PId = factor(rep(1:30, each=2)),
    X1 = factor(c(rep("a",30), rep("b",30))),
    X2 = factor(rep(c("c","d"), times=30)),
    Y = factor(c(a,b))
  )
  suppressMessages({
    suppressWarnings({
      m1 = lme4::glmer(Y ~ X1*X2 + (1|PId), data=df, family=binomial)
      m2 = glmer.mp(Y ~ X1*X2 + (1|PId), data=df)
      a1 = car::Anova(m1, type=3)
      a2 = Anova.mp(m2, type=3)
      expect_true(abs(a1$`Pr(>Chisq)`[2] - a2$`Pr(>Chisq)`[1]) <= 0.10) #X1
      expect_true(abs(a1$`Pr(>Chisq)`[3] - a2$`Pr(>Chisq)`[2]) <= 0.10) #X2
      expect_true(abs(a1$`Pr(>Chisq)`[4] - a2$`Pr(>Chisq)`[3]) <= 0.10) #X1:X2
    })
  })
})




