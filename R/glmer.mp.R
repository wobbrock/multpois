##
## glmer.mp
##
## Author: Jacob O. Wobbrock
##

#' @title
#' Build a multinomial-Poisson GLMM for nominal response data
#'
#' @description
#' This function uses the multinomial-Poisson trick to analyze \strong{nominal response} data using a Poisson
#' generalized linear mixed model (GLMM). The nominal response should be a factor with two or more unordered
#' categories. The independent variables should have at least one within-subjects factor or numeric predictor.
#' There also must be a repeated subject identifier to be used as a random factor.
#'
#' @param formula A formula object in the style of, e.g., \code{Y ~ X1*X2 + (1|PId)}, where \code{X1} and
#' \code{X2} are factors or predictors and \code{PId} is a factor serving as a subject identifier. The
#' response \code{Y} must be of type \code{factor}. See the \code{formula} entry for \code{\link[lme4]{glmer}}.
#'
#' @param data A data frame in long-format. See the \code{data} entry for \code{\link[lme4]{glmer}}.
#'
#' @param ... Additional arguments to be passed to \code{\link[lme4]{glmer}}. Generally, these are
#' unnecessary but are provided for advanced users. They should not specify \code{formula}, \code{data},
#' or \code{family} arguments. See \code{\link[lme4]{glmer}} for valid arguments.
#'
#' @returns A mixed-effects Poisson regression model of type \code{\link[lme4]{merMod}}, specifically
#' of \emph{subclass} \code{glmerMod}. See the return value for \code{\link[lme4]{glmer}}.
#'
#' @details
#' This function should be used for nominal response data with repeated measures. In essence, it provides
#' for the equivalent of \code{\link[lme4]{glmer}} with \code{family=multinomial}, were that option to
#' exist. (That option does not exist, which is a key motivation for developing this function.)
#'
#' For polytomous response data with only between-subjects factors, use \code{\link{glm.mp}} or
#' \code{\link[nnet]{multinom}}.
#'
#' Users wishing to verify the correctness of \code{glmer.mp} should compare its \code{\link{Anova.mp}}
#' results to \code{\link[car]{Anova}} results for models built with \code{\link[lme4]{glmer}} using
#' \code{family=binomial} for dichotomous responses. The results should be similar.
#'
#' \emph{Post hoc} pairwise comparisons for factors can be conducted with \code{\link{glmer.mp.con}}.
#'
#' @note It is common to receive a \code{boundary (singular) fit} message. This generally can be ignored
#' provided the test output looks sensible. Less commonly, the procedure can fail to converge, which
#' can happen when counts of one or more categories are very small or zero in some conditions. In such
#' cases, any results should be regarded with caution.
#'
#' @note Sometimes, convergence issues can be remedied by changing the optimizer or its control parameters.
#' For example, the following code uses the \code{bobyqa} optimizer instead of the \code{Nelder_Mead} optimizer
#' and increases the maximum number of function evaluations:
#'
#' \code{m = glmer.mp(Y ~ X + (1|PId), data=df, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))}
#'
#' See \code{\link[lme4]{glmerControl}} for more information.
#'
#' @references Baker, S.G. (1994). The multinomial-Poisson transformation.
#' \emph{The Statistician 43} (4), pp. 495-504. \doi{10.2307/2348134}
#'
#' @references Chen, Z. and Kuo, L. (2001). A note on the estimation of the
#' multinomial logit model with random effects. \emph{The American Statistician
#' 55} (2), pp. 89-95. \url{https://www.jstor.org/stable/2685993}
#'
#' @references Guimaraes, P. (2004). Understanding the multinomial-Poisson
#' transformation. \emph{The Stata Journal 4} (3), pp. 265-273.
#' \url{https://www.stata-journal.com/article.html?article=st0069}
#'
#' @references Lee, J.Y.L., Green, P.J.,and Ryan, L.M. (2017). On the “Poisson
#' trick” and its extensions for fitting multinomial regression models. \emph{arXiv
#' preprint} available at \doi{10.48550/arXiv.1707.08538}
#'
#' @author Jacob O. Wobbrock
#'
#' @seealso [Anova.mp()], [glmer.mp.con()], [glm.mp()], [glm.mp.con()], [lme4::glmer()]
#'
#' @examples
#' library(car)
#' library(lme4)
#' library(lmerTest)
#'
#' ## within-subjects factors (x1,X2) with dichotomous response (Y)
#' data(ws2, package="multpois")
#'
#' ws2$PId = factor(ws2$PId)
#' ws2$Y = factor(ws2$Y)
#' ws2$X1 = factor(ws2$X1)
#' ws2$X2 = factor(ws2$X2)
#' contrasts(ws2$X1) <- "contr.sum"
#' contrasts(ws2$X2) <- "contr.sum"
#'
#' m1 = glmer(Y ~ X1*X2 + (1|PId), data=ws2, family=binomial)
#' Anova(m1, type=3)
#'
#' m2 = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws2) # compare
#' Anova.mp(m2, type=3)
#'
#' ## within-subjects factors (x1,X2) with polytomous response (Y)
#' data(ws3, package="multpois")
#'
#' ws3$PId = factor(ws3$PId)
#' ws3$Y = factor(ws3$Y)
#' ws3$X1 = factor(ws3$X1)
#' ws3$X2 = factor(ws3$X2)
#' contrasts(ws3$X1) <- "contr.sum"
#' contrasts(ws3$X2) <- "contr.sum"
#'
#' m3 = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws3)
#' Anova.mp(m3, type=3)
#'
#' @importFrom stats terms
#' @importFrom stats contrasts
#' @importFrom stats 'contrasts<-'
#' @importFrom stats as.formula
#' @importFrom stats poisson
#'
#' @export glmer.mp
glmer.mp <- function(formula, data, ...)
{
  # ensure data is a proper data frame
  if (!is.data.frame(data)) {
    stop("'data' must be a long-format data frame.")
  }

  # ensure there is some D.V.
  t = terms(formula)
  if (attr(t, "response") != 1) {
    stop("'formula' must have a dependent variable on the left-hand side.")
  }

  # ensure there is only one D.V.
  DV = all.vars(formula[[2]])
  if (length(DV) != 1) {
    stop("'formula' must only have one dependent variable. You have ", length(DV), ".")
  }

  # ensure D.V. is nominal
  if (!is.factor(data[[DV]])) {
    stop("'formula' must have a nominal dependent variable of type 'factor'.\n\t", DV, " is of type ", class(data[[DV]]))
  }

  # get the independent variables from the formula
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]

  # ensure there is a random factor in the formula
  hasrnd = plyr::laply(IVs, function(term) as.list(term)[[1]] == quote(`|`))
  if (!any(hasrnd)) {
    stop("'formula' must have at least one random factor, e.g., (1|PId) or (X|PId).")
  }

  # ensure any optional arguments do not specify a formula, data frame, or family
  optargs = list(...)
  if (exists("formula", where=optargs)) {
    stop("'...' cannot contain a 'formula' argument.")
  }
  if (exists("data", where=optargs)) {
    stop("'...' cannot contain a 'data' argument.")
  }
  if (exists("family", where=optargs)) {
    stop("'...' cannot contain a 'family' argument.")
  }

  # transform data table
  df = dfidx::dfidx(data, choice=DV, shape="wide", drop.index=FALSE, idnames=c("chid","alt"))

  # copy over the factor contrasts from the source table to the new table
  for (i in 1:length(colnames(data))) {
    if (is.factor(data[[i]]) & colnames(data)[i] != DV) {
      contrasts(df[[i]]) <- contrasts(data[[i]])
    }
  }
  # also set the new "alt" factor contrasts
  contrasts(df$alt) <- "contr.sum"

  # add in "alt"
  s = paste0(DV, " ~ alt")
  tlabs = attr(terms(formula), "term.labels")
  for (i in 1:length(tlabs)) {
    if (!grepl("|", tlabs[i], fixed=TRUE)) { # fixed factors
      s = paste0(s, " + ", tlabs[i], " + alt:", tlabs[i])
    }
    else { # random factors
      lhs = trimws(strsplit(tlabs[i], "|", fixed=TRUE)[[1]][1])
      rhs = trimws(strsplit(tlabs[i], "|", fixed=TRUE)[[1]][2])
      s = paste0(s, " + (", lhs, " + alt | ", rhs, ")")
    }
  }
  f = as.formula(s) # convert to formula

  # build and return our model
  m = lme4::glmer(formula=f, data=df, family=poisson, ...) # m-P trick
  return (m)
}
