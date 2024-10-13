##
## glmer.mp.con
##
## Author: Jacob O. Wobbrock
##

#' @title
#' Contrast tests for multinomial-Poisson GLMMs
#'
#' @description
#' This function conducts \emph{post hoc} pairwise comparisons on generalized linear mixed models (GLMMs)
#' built with \code{\link{glmer.mp}}. Such models have \strong{nominal response} types, i.e., \code{factor}s
#' with unordered categories.
#'
#' @param model A multinomial-Poisson generalized linear mixed model created by \code{\link{glmer.mp}}.
#'
#' @param formula A formula object in the style of, e.g., \code{pairwise ~ X1*X2}, where \code{X1} and
#' \code{X2} are factors in \code{model}. The \code{pairwise} keyword \strong{must} be used on the left-hand
#' side of the formula. See the \code{specs} entry for \code{\link[emmeans]{emmeans}}.
#'
#' @param adjust A string indicating the \emph{p}-value adjustment to use. Defaults to \code{"holm"}. See the
#' Details section for \code{\link[stats]{p.adjust}}.
#'
#' @returns Pairwise comparisons for all levels indicated by the factors in \code{formula}.
#'
#' @details
#' \emph{Post hoc} pairwise comparisons should be conducted \emph{only} after a statistically significant
#' omnibus test using \code{\link{Anova.mp}}. Comparisons are conducted in the style of
#' \code{\link[emmeans]{emmeans}} but not using this function; rather, the multinomial-Poisson trick is used
#' on a subset of the data relevant to each pairwise comparison.
#'
#' Users wishing to verify the correctness of \code{glmer.mp.con} should compare its results to
#' \code{\link[emmeans]{emmeans}} results for models built with \code{\link[lme4]{glmer}} using
#' \code{family=binomial} for dichotomous responses. The results should be similar.
#'
#' @note It is common to receive \code{boundary (singular) fit} messages. These generally can be ignored
#' provided the test outputs look sensible. Less commonly, the procedures can fail to converge, which
#' can happen when counts of one or more categories are very small or zero in some conditions. In such
#' cases, any results should be regarded with caution.
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
#' @seealso [Anova.mp()], [glmer.mp()], [glm.mp()], [glm.mp.con()], [emmeans::emmeans()]
#'
#' @examples
#' library(car)
#' library(lme4)
#' library(lmerTest)
#' library(emmeans)
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
#' emmeans(m1, pairwise ~ X1*X2, adjust="holm")
#'
#' m2 = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws2)
#' Anova.mp(m2, type=3)
#' glmer.mp.con(m2, pairwise ~ X1*X2, adjust="holm") # compare
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
#' glmer.mp.con(m3, pairwise ~ X1*X2, adjust="holm")
#'
#' @importFrom stats model.frame
#' @importFrom stats terms
#' @importFrom stats contrasts
#' @importFrom stats 'contrasts<-'
#' @importFrom stats as.formula
#' @importFrom stats poisson
#' @importFrom stats p.adjust
#'
#' @export glmer.mp.con
glmer.mp.con <- function(model, formula, adjust=c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"))
{
  # require the pairwise keyword
  if (formula[[2]] != "pairwise") {
    stop("glmer.mp.con requires the 'pairwise' keyword on the left hand side of the ~ .")
  }

  # ensure the model is of class "glmerMod"
  mtype = as.list(class(model))
  if (!any(mtype == "glmerMod")) {
    stop("glmer.mp.con requires a model created by glmer.mp.")
  }

  # get the data frame used for the model
  df = model.frame(model)

  # df must contain an "alt" factor column or this isn't a model built by glmer.mp
  if (!exists("alt", df)) {
    stop("glmer.mp.con requires a model created by glmer.mp.")
  }

  # ensure there is a random factor in the original model
  f0 = formula(model)
  t0 = terms(f0)
  iv0 = as.list(attr(t0, "variables"))[c(-1,-2)]
  hasrnd = plyr::laply(iv0, function(term) as.list(term)[[1]] == quote(`|`))
  if (!any(hasrnd)) {
    stop("glmer.mp.con requires a model with a random factor, e.g., (1|S) or (X|S).")
  }

  # get our contrast formula I.V.s
  t = terms(formula)
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]

  # ensure all contrast I.V.s were in the original model formula
  if (!any(IVs %in% iv0)) {
    stop("glmer.mp.con requires formula terms to be present in the model.")
  }

  # ensure all contrast formula I.V.s are factors
  ivnotfac = plyr::laply(IVs, function(term) !is.factor(df[[term]]))
  if (any(ivnotfac)) {
    snf = ""
    for (i in 1:length(ivnotfac)) {
      if (ivnotfac[i]) {
        snf = paste0(snf, '\n\t', IVs[[i]], " is of type ", class(df[[ IVs[[i]] ]]))
      }
    }
    stop("glmer.mp.con requires formula terms to be factors:", snf)
  }

  # build our new composite factor name and column values
  facname = IVs[[1]]
  facvals = df[[ IVs[[1]] ]]
  if (length(IVs) > 1) {
    for (i in 2:length(IVs)) {
      facname = paste0(facname, ".", IVs[[i]])
      facvals = paste0(facvals, ".", df[[ IVs[[i]] ]])
    }
  }

  # set the new column and its values as a factor and get its levels
  df[[facname]] = as.factor(facvals)
  lvls = levels(df[[facname]])

  # get our dependent variable
  DV = f0[[2]]

  # get our model's original terms so we can preserve random effects
  tlabs = attr(t0, "term.labels")

  # now do each of the pairwise comparisons and store them in an output table
  resdf <- data.frame(Contrast=character(), Chisq=numeric(), Df=numeric(), N=integer(), p.value=numeric())

  for (i in 1:(length(lvls) - 1))
  {
    for (j in (i + 1):length(lvls))
    {
      # get the relevant subset of rows for this comparison
      d0 = df[df[[facname]] == lvls[i] | df[[facname]] == lvls[j],]

      # update factor levels and contrasts for subset
      d0[[facname]] = factor(d0[[facname]])
      contrasts(d0[[facname]]) <- "contr.sum"

      # create our model formula for this comparison
      s = paste0(DV, " ~ ", facname, " + alt + ", facname, ":alt")
      for (k in 1:length(tlabs)) {
        if (grepl("|", tlabs[k], fixed=TRUE)) {
          s = paste0(s, " + (", tlabs[k], ")")
        }
      }
      f = as.formula(s) # convert to formula

      # finally, create our model and examine its effects
      m = lme4::glmer(f, data=d0, family=poisson) # m-P trick
      a = car::Anova(m, type=3)
      a = a[grep(":alt", rownames(a)),] # get relevant entry

      # improve our row
      rownames(a)[1] = paste0(lvls[i], " - ", lvls[j]) # update contrast label
      colnames(a)[3] = "p.value" # simplify p-value label
      a = dplyr::mutate(a, .after="Df", N=nrow(d0)/length(levels(df$alt))) # insert N

      # create a new output row entry and add it to our output table
      r = list(Contrast = rownames(a)[1],
               Chisq = round(as.numeric(format(a$Chisq, scientific=FALSE)), 6),
               Df = a$Df,
               N = a$N,
               p.value = round(a$p.value, 6)
      )
      resdf = rbind(resdf, r, stringsAsFactors=FALSE) # add row to table
    }
  }

  # apply p-value adjustment
  adjust = match.arg(adjust)
  resdf$p.value = p.adjust(resdf$p.value, method=adjust)

  # assemble our return value as a list
  retval = list(
    heading = "Pairwise comparisons via the multinomial-Poisson trick",
    contrasts = resdf,
    notes = paste0("P value adjustment: ", adjust, " method for ", length(rownames(resdf)), " tests")
  )

  return (retval)
}
