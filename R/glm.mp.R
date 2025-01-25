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
#' generalized linear model (GLM). The nominal response should be a factor with two or more unordered
#' categories. The independent variables should be between-subjects factors and/or numeric predictors.
#'
#' @param formula A formula object in the style of, e.g., \code{Y ~ X1*X2}, where \code{X1} and \code{X2}
#' are factors or predictors. The response \code{Y} must be of type \code{factor}. See the \code{formula}
#' entry for \code{\link[stats]{glm}}.
#'
#' @param data A data frame in long-format. See the \code{data} entry for \code{\link[stats]{glm}}.
#'
#' @param ... Additional arguments to be passed to \code{\link[stats]{glm}}. Generally, these are
#' unnecessary but are provided for advanced users. They should not specify \code{formula}, \code{data},
#' or \code{family} arguments. See \code{\link[stats]{glm}} for valid arguments.
#'
#' @returns A Poisson regression model of type \code{\link[stats]{glm}}. See the return value for
#' \code{\link[stats]{glm}}.
#'
#' @details
#' This function should be used for nominal response data with only between-subjects factors or predictors.
#' In essence, it provides for the equivalent of \code{\link[stats]{glm}} with \code{family=multinomial},
#' were that option to exist. (That option does not exist, but \code{\link[nnet]{multinom}} serves the same
#' purpose.)
#'
#' For data with repeated measures, use \code{\link{glmer.mp}}, which can take random factors and thus handle
#' correlated responses.
#'
#' Users wishing to verify the correctness of \code{glm.mp} should compare \code{\link{Anova.mp}} results
#' to \code{\link[car]{Anova}} results for models built with \code{\link[stats]{glm}} using
#' \code{family=binomial} (for dichotomous responses) or \code{\link[nnet]{multinom}} (for polytomous
#' responses). The results should be similar.
#'
#' \emph{Post hoc} pairwise comparisons for factors can be conducted with \code{\link{glm.mp.con}}.
#'
#' @references Baker, S.G. (1994). The multinomial-Poisson transformation.
#' \emph{The Statistician 43} (4), pp. 495-504. \doi{10.2307/2348134}
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
#' @importFrom stats terms
#' @importFrom stats contrasts
#' @importFrom stats 'contrasts<-'
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @importFrom stats poisson
#'
#' @export glm.mp
glm.mp <- function(formula, data, ...)
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

  # ensure there are no random factors in the formula
  hasrnd = plyr::laply(IVs, function(term) as.list(term)[[1]] == quote(`|`))
  if (any(hasrnd)) {
    stop("'formula' cannot have random factors.")
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

  # create the new formula with the "alt" factor
  s = paste0(DV, " ~ alt")
  tlabs = attr(terms(formula), "term.labels")
  for (i in 1:length(tlabs)) {
    s = paste0(s, " + ", tlabs[i], " + alt:", tlabs[i]) # fixed factors
  }
  f = as.formula(s) # convert to formula

  # build and return our model
  m = glm(formula=f, data=df, family=poisson, ...) # m-P trick
  return (m)
}
