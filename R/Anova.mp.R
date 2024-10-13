##
## Anova.mp
##
## Author: Jacob O. Wobbrock
##

#' @title
#' Get ANOVA-style results for a multinomial-Poisson model
#'
#' @description
#' Get ANOVA-style results for a model returned by \code{\link{glm.mp}} or \code{\link{glmer.mp}}.
#' The output table contains chi-square results for the main effects and interactions indicated by
#' the given model.
#'
#' @param model A model built by \code{\link{glm.mp}} or \code{\link{glmer.mp}}. (The underlying model
#' will have been built by \code{\link[stats]{glm}} or \code{\link[lme4]{glmer}}, with
#' \code{family=poisson}.)
#'
#' @param type The \code{type} parameter passed to \code{\link[car]{Anova}}. The default is type 3.
#'
#' @returns An ANOVA-style table of chi-square results for models built by \code{\link{glm.mp}} or
#' \code{\link{glmer.mp}}. See the return value for \code{\link[car]{Anova}}.
#'
#' @details
#' The \code{Anova.mp} function uses \code{\link[car]{Anova}} behind the scenes to produce an ANOVA-style
#' table with chi-square results.
#'
#' Users wishing to verify the correctness of these results can compare \code{\link[car]{Anova}} results
#' for dichotomous response models built with \code{\link[stats]{glm}} or \code{\link[lme4]{glmer}} (using
#' \code{family=binomial}) to \code{Anova.mp} results for models built with \code{\link{glm.mp}} or
#' \code{\link{glmer.mp}}, respectively. The results should generally match, or be very similar.
#'
#' Users can also compare \code{\link[car]{Anova}} results for polytomous response models built with
#' \code{\link[nnet]{multinom}} to \code{Anova.mp} results for models built with \code{\link{glm.mp}}.
#' Again, the results should generally match, or be very similar.
#'
#' There is no similarly easy comparison for polytomous response models with repeated measures. The
#' lack of options was a key motivation for developing \code{\link{glmer.mp}} in the first place.
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
#' @seealso [glm.mp()], [glm.mp.con()], [glmer.mp()], [glmer.mp.con()], [car::Anova()]
#'
#' @examples
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
#' m1 = glm.mp(Y ~ X1*X2, data=bs3)
#' Anova.mp(m1, type=3)
#'
#' ## within-subjects factors (X1,X2) with polytomous response (Y)
#' data(ws3, package="multpois")
#'
#' ws3$PId = factor(ws3$PId)
#' ws3$Y = factor(ws3$Y)
#' ws3$X1 = factor(ws3$X1)
#' ws3$X2 = factor(ws3$X2)
#' contrasts(ws3$X1) <- "contr.sum"
#' contrasts(ws3$X2) <- "contr.sum"
#'
#' m2 = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws3)
#' Anova.mp(m2, type=3)
#'
#' @importFrom stats model.frame
#'
#' @export Anova.mp
Anova.mp <- function(model, type=c(3, 2, "III", "II"))
{
  # ensure the model is of class "glm" or "glmerMod"
  mtype = as.list(class(model))
  if (!any(mtype == "glm" | mtype == "glmerMod")) {
    stop("Anova.mp requires a model created by glm.mp or glmer.mp.")
  }

  # get the data frame used to create the model
  df = model.frame(model)

  # df must contain an "alt" factor column or this isn't a model built by glm.mp or glmer.mp
  if (!exists("alt", df)) {
    stop("Anova.mp requires a model created by glm.mp or glmer.mp.")
  }

  # get the Anova type
  type = as.character(type)
  type = match.arg(type)

  # run the Anova
  a = car::Anova(model, type)

  # update our output heading
  attr(a, "heading")[3] = "via the multinomial-Poisson trick"
  h = attr(a, "heading") # save

  # insert N for chisq result
  a = dplyr::mutate(.data=a, .after="Df", N=nrow(df)/length(levels(df$alt)))

  # extract relevant effect entries
  a = a[grep(":alt", rownames(a), fixed=TRUE),]
  rownames(a) = sub(":alt", "", rownames(a))

  # make consistent
  colnames(a)[1] = "Chisq"

  attr(a, "heading") = h # restore
  return (a)
}

