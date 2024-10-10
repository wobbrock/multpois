##
## glm.mp
##
## Author: Jacob O. Wobbrock
##

#' @title
#' Build a multinomial-Poisson GLM for nominal response data
#'
#' @description
#' This function uses the multinomial-Poisson trick to analyze \strong{nominal response} data using a Poisson
#' generalized linear model (GLM). The nominal response should have two or more unordered categories. The
#' independent variables should be between-subjects factors.
#'
#' @param formula A formula object in the style of, e.g., \code{Y ~ X1*X2}, where \code{X1} and \code{X2}
#' are factors. The response \code{Y} must be a nominal variable, i.e., of type \code{factor}. See
#' the \code{formula} entry for \code{\link[stats]{glm}}.
#'
#' @param data A data frame in long-format. See the \code{data} entry for \code{\link[stats]{glm}}.
#'
#' @returns A Poisson regression model of type \code{\link[stats]{glm}}. See the return value for
#' \code{\link[stats]{glm}}.
#'
#' @details
#' This function should be used for nominal response data with only between-subjects factors. For data
#' with repeated measures, use \code{\link{glmer.mp}}, which can take random factors and thus
#' handle correlated responses.
#'
#' Users wishing to verify the correctness of \code{glm.mp} should compare its \code{\link{Anova.mp}} results to
#' \code{\link[car]{Anova}} results for models built with \code{\link[stats]{glm}} using \code{family=binomial}
#' (for dichotomous responses) or \code{\link[nnet]{multinom}} (for polytomous responses). In general, the
#' results should be very close or match.
#'
#' @references Baker, S.G. (1994). The multinomial-Poisson transformation.
#' \emph{The Statistician 43} (4), pp. 495-504. \url{https://doi.org/10.2307/2348134}
#'
#' @references Guimaraes, P. (2004). Understanding the multinomial-Poisson
#' transformation. \emph{The Stata Journal 4} (3), pp. 265-273.
#' \url{https://www.stata-journal.com/article.html?article=st0069}
#'
#' @references Lee, J.Y.L., Green, P.J.,and Ryan, L.M. (2017). On the “Poisson
#' trick” and its extensions for fitting multinomial regression models. \emph{arXiv
#' preprint} available at \url{https://doi.org/10.48550/arXiv.1707.08538}
#'
#' @seealso [Anova.mp()], [glm.mp.con()], [glmer.mp()], [glmer.mp.con()], [stats::glm()], [nnet::multinom()]
#'
#' @examples
#' library(car)
#' library(nnet)
#'
#' ## between-subjects factors (X1,X2) with dichotomous response (Y)
#' data(bs2, package="multpois")
#'
#' bs2$PId = factor(bs2$PId)
#' bs2$Y = factor(bs2$Y)
#' bs2$X1 = factor(bs2$X1)
#' bs2$X2 = factor(bs2$X2)
#' contrasts(bs2$X1) <- "contr.sum"
#' contrasts(bs2$X2) <- "contr.sum"
#'
#' m1 = glm(Y ~ X1*X2, data=bs2, family=binomial)
#' Anova(m1, type=3)
#'
#' m2 = glm.mp(Y ~ X1*X2, data=bs2) # compare
#' Anova.mp(m2, type=3)
#'
#' ## between-subjects factors (X1,X2) with polytomous response (Y)
#' data(bs3, package="multpois")
#'
#' bs3$PId = factor(bs3$PId)
#' bs3$Y = factor(bs3$Y)
#' bs3$X1 = factor(bs3$X1)
#' bs3$X2 = factor(bs3$X2)
#' contrasts(bs3$X1) <- "contr.sum"
#' contrasts(bs3$X2) <- "contr.sum"
#'
#' m3 = multinom(Y ~ X1*X2, data=bs3, trace=FALSE)
#' Anova(m3, type=3)
#'
#' m4 = glm.mp(Y ~ X1*X2, data=bs3) # compare
#' Anova.mp(m4, type=3)
#'
#' @export
glm.mp <- function(formula, data)
{
  # ensure there is only one D.V.
  t = terms(formula)
  if (attr(t, "response") != 1) {
    stop("glm.mp is only valid for one dependent variable. You have ", attr(t, "response"), ".")
  }

  # get the one D.V.
  DV = formula[[2]] # D.V.

  # ensure D.V. is nominal
  dvtype = as.list(class(data[[DV]]))
  if (any(dvtype != "factor")) {
    stop("glm.mp is only valid for nominal dependent variables. ", DV, " is of type ", paste0(unlist(dvtype), collapse =", "), ".")
  }

  # get the independent variables from the formula
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]

  # ensure there are no random factors in the formula
  hasrnd = plyr::laply(IVs, function(term) as.list(term)[[1]] == quote(`|`))
  if (any(hasrnd)) {
    stop("glm.mp is only valid for formulas without random factors.")
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

  # create the new formula with the "alt" factor
  f = update.formula(formula, . ~ . * alt)

  # build and return our model
  m = glm(f, data=df, family=poisson) # m-P trick
  return (m)
}
