
# multpois

<!-- badges: start -->
<!-- badges: end -->
*Jacob O. Wobbrock, University of Washington <wobbrock@uw.edu>*

The goal of multpois is to use the multinomial-Poisson trick to provide for the
analysis of nominal response data with or without repeated measures. Such 
responses, which often arise from surveys or experiments, consist of unordered
categories. Although dichotomous responses can be analyzed with glm() or 
lme4::glmer() using the family=binomial option, there is no analogous 
family=multinomial option for polytomous responses. In the case of purely 
between-subjects data, nnet::multinom() can be used, but it cannot take random
factors and therefore cannot handle repeated measures. To address this issue,
the multpois package provides for the equivalent of a family=multinomial option
in glm() or lme4::glmer() via the multinomial-Poisson trick, which converts
nominal response data into counts of categorical alternatives and analyzes these
counts using (mixed) Poisson regression. Omnibus tests of main effects and 
interactions are provided through analysis of variance-style output. Post hoc
pairwise comparisons are also provided through contrast testing.

## Installation

You can install the multpois package like so:

``` r
install.packages("multpois")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(multpois)
set.seed(123) # for repeatable results

## a generic 2x2 between-subjects example
ac = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.1, 0.6, 0.3))
ad = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.4, 0.4, 0.2))
bc = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.5, 0.1, 0.4))
bd = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.1, 0.5, 0.4))
df1 = data.frame(
  PId = factor(seq(1, 60, 1)),
  X1 = factor(c(rep("a",30), rep("b",30))),
  X2 = factor(rep(c(rep("c",15), rep("d",15)), times=2)),
  Y = factor(c(ac, ad, bc, bd), levels=c("yes","no","maybe"))
)
View(df1)
mosaicplot( ~ X1 + X2 + Y, data=df1, cex=1, col=c("lightgreen","pink","lightyellow"))
m = glm.mp(Y ~ X1*X2, data=df1)
Anova.mp(m, type=3)
glm.mp.con(m, pairwise ~ X1*X2, adjust="holm")

## a generic 2x2 within-subjects example
ac = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.2, 0.6, 0.2))
ad = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.4, 0.4, 0.2))
bc = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.5, 0.2, 0.3))
bd = sample(c("yes","no","maybe"), size=15, replace=TRUE, prob=c(0.2, 0.5, 0.3))
df2 = data.frame(
  PId = factor(rep(1:15, times=4)),
  X1 = factor(c(rep("a",30), rep("b",30))),
  X2 = factor(rep(c(rep("c",15), rep("d",15)), times=2)),
  Y = factor(c(ac, ad, bc, bd), levels=c("yes","no","maybe"))
)
View(df2)
mosaicplot( ~ X1 + X2 + Y, data=df2, cex=1, col=c("lightgreen","pink","lightyellow"))
m = glmer.mp(Y ~ X1*X2 + (1|PId), data=df2)
Anova.mp(m, type=3)
glmer.mp.con(m, pairwise ~ X1*X2, adjust="holm")
```

