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
#' @param type In the case of a type II or type III ANOVA, this value will be the \code{type} parameter
#' passed to \code{\link[car]{Anova}}. In the case of a type I ANOVA, for models built with
#' \code{\link{glm.mp}}, the \code{\link[stats]{anova.glm}} function will be called; for models built with
#' \code{\link{glmer.mp}}, the \code{\link[lme4]{anova.merMod}} function will be called.
#' The default is type 3. See the Details section for \code{\link[car]{Anova}}.
#'
#' @returns An ANOVA-style table of chi-square results for models built by \code{\link{glm.mp}} or
#' \code{\link{glmer.mp}}. See the return values for \code{\link[car]{Anova}}, \code{\link[stats]{anova.glm}},
#' or \code{\link[lme4]{anova.merMod}}.
#'
#' @details
#' For type II or III ANOVAs, the \code{Anova.mp} function uses \code{\link[car]{Anova}} behind the scenes to
#' produce an ANOVA-style table with chi-square results. For type I ANOVAs, it uses
#' \code{\link[stats]{anova.glm}} or \code{\link[lme4]{anova.merMod}}.
#'
#' Users wishing to verify the correctness of these results can compare \code{\link[car]{Anova}} results
#' for dichotomous response models built with \code{\link[stats]{glm}} or \code{\link[lme4]{glmer}} (using
#' \code{family=binomial}) to \code{Anova.mp} results for models built with \code{\link{glm.mp}} or
#' \code{\link{glmer.mp}}, respectively. The results should be similar.
#'
#' Users can also compare \code{\link[car]{Anova}} results for polytomous response models built with
#' \code{\link[nnet]{multinom}} to \code{Anova.mp} results for models built with \code{\link{glm.mp}}.
#' Again, the results should be similar.
#'
#' There is no similarly easy comparison for polytomous response models with repeated measures. This
#' lack of options was a key motivation for developing \code{\link{glmer.mp}}.
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
#' @importFrom stats formula
#' @importFrom stats pchisq
#' @importFrom stats anova
#'
#' @export Anova.mp
Anova.mp <- function(model, type=c(3, 2, 1, "III", "II", "I"))
{
  # ensure the model is of class "glm" or "glmerMod"
  mtype = as.list(class(model))
  if (!any(mtype == "glm" | mtype == "glmerMod")) {
    stop("'model' must be created by glm.mp or glmer.mp.")
  }

  # get the data frame used to create the model
  df = model.frame(model)

  # df must contain an "alt" factor column or this isn't a model built by glm.mp or glmer.mp
  if (!exists("alt", where=df)) {
    stop("'model' must be created by glm.mp or glmer.mp.")
  }

  # get the ANOVA type
  type = as.character(type)
  type = match.arg(type)
  type = if (type %in% c(1,"I")) {1}
  else if (type %in% c(2,"II"))  {2}
  else if (type %in% c(3,"III")) {3}

  # run the Anova
  a = NULL
  if (type == 1) # type I ANOVA
  {
    if (any(mtype == "glm")) { # glm.mp
      a = anova(model, test="Chisq")

      # insert N for Chisq result
      a = dplyr::mutate(.data=a, .after="Df", "N"=nrow(df)/length(levels(df$alt)))

      # extract the rows that are interactions with "alt"
      a = a[grep("alt:", rownames(a), fixed=TRUE),]
      rownames(a) = sub("alt:", "", rownames(a), fixed=TRUE)

      # rename the Deviance column to be Chisq and move it first
      colnames(a) = sub("Deviance", "Chisq", colnames(a), fixed=TRUE)
      a = dplyr::relocate(.data=a, "Chisq", .before="Df")

      # rename the Pr(>Chi) column to Pr(>Chisq) for consistency
      colnames(a) = sub("Pr(>Chi)", "Pr(>Chisq)", colnames(a), fixed=TRUE)

      # remove unwanted columns
      a[,"Resid. Df"] = NULL
      a[,"Resid. Dev"] = NULL
    }
    else if (lme4::isGLMM(model)) { # glmer.mp
      a = anova(model, refit=FALSE)

      # extract the rows that are interactions with "alt"
      a = a[grep("alt:", rownames(a), fixed=TRUE),]
      rownames(a) = sub("alt:", "", rownames(a), fixed=TRUE)

      # rename the npar column to be Df
      colnames(a) = sub("npar", "Df", colnames(a), fixed=TRUE)

      # insert N for Chisq result
      a = dplyr::mutate(.data=a, .after="Df", "N"=nrow(df)/length(levels(df$alt)))

      # rename the F value column to be Chisq and move it first
      colnames(a) = sub("F value", "Chisq", colnames(a), fixed=TRUE)
      a = dplyr::relocate(.data=a, "Chisq", .before="Df")

      # add a p-value column
      a = dplyr::mutate(.data=a, .after="N", "Pr(>Chisq)"=1-pchisq(a$Chisq,a$Df))

      # remove unwanted columns
      a[,"Sum Sq"] = NULL
      a[,"Mean Sq"] = NULL
    }
    else {
      stop("'model' must be created by glm.mp or glmer.mp.")
    }
    # make our output message the same as for the Type II and III tests
    DV = formula(model)[[2]]
    attr(a, "heading") = paste0("Analysis of Deviance Table (Type I tests)\n\nResponse: ",
                                DV, "\nvia the multinomial-Poisson trick")
  }
  else # type II or III ANOVA
  {
    a = car::Anova(model, type)

    # update our output heading
    attr(a, "heading")[3] = "via the multinomial-Poisson trick"
    h = attr(a, "heading") # save

    # insert N for chisq result
    a = dplyr::mutate(.data=a, .after="Df", "N"=nrow(df)/length(levels(df$alt)))

    # extract the rows that are interactions with "alt"
    a = a[grep("alt:", rownames(a), fixed=TRUE),]
    rownames(a) = sub("alt:", "", rownames(a), fixed=TRUE)

    # make consistent
    colnames(a)[1] = "Chisq"

    attr(a, "heading") = h # restore
  }
  return (a)
}


