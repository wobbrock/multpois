##
## glm.mp.con
##
## Author: Jacob O. Wobbrock
##

#' @title
#' Contrast tests for multinomial-Poisson GLM
#'
#' @description
#' This function conducts \emph{post hoc} pairwise comparisons on generalized linear models (GLMs) built
#' with \code{\link{glm.mp}}. Such models have \strong{nominal response} types, i.e., \code{factor}s with
#' unordered categories.
#'
#' @param model A multinomial-Poisson generalized linear model created by \code{\link{glm.mp}}.
#'
#' @param formula A formula object in the style of, e.g., \code{pairwise ~ X1*X2}, where \code{X1} and
#' \code{X2} are factors in \code{model}. The \code{pairwise} keyword \emph{must} be used on the left-hand
#' side of the formula. See the \code{specs} entry for \code{\link[emmeans]{emmeans}}.
#'
#' @param adjust A string indicating the \emph{p}-value adjustment to use. Defaults to \code{"holm"}. See the
#' details for \code{\link[stats]{p.adjust}}.
#'
#' @param ... Additional arguments to be passed to \code{\link[stats]{glm}}. Generally, these are
#' unnecessary but are provided for advanced users. They must not pass \code{formula}, \code{data},
#' or \code{family} arguments. See \code{\link[stats]{glm}} for valid arguments.
#'
#' @returns Pairwise comparisons for all levels indicated by the factors in \code{formula}.
#'
#' @details
#' \emph{Post hoc} pairwise comparisons should be conducted \emph{only} after a statistically significant
#' omnibus test using \code{\link{Anova.mp}}. Comparisons are conducted in the style of
#' \code{\link[emmeans]{emmeans}} but not using this function; rather, the multinomial-Poisson trick is used
#' on the subset of the data relevant to each pairwise comparison.
#'
#' Users wishing to verify the correctness of \code{glm.mp.con} should compare its results to
#' \code{\link[emmeans]{emmeans}} results for models built with \code{\link[stats]{glm}} using
#' \code{family=binomial} for dichotomous responses. Factor contrasts should be set to sum-to-zero
#' contrasts (i.e., \code{"contr.sum"}). The results should be similar.
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
#' @seealso [Anova.mp()], [glm.mp()], [glmer.mp()], [glmer.mp.con()], [stats::glm()], [stats::glm.control()], [emmeans::emmeans()]
#'
#' @examples
#' library(multpois)
#' library(car)
#' library(nnet)
#' library(emmeans)
#'
#' ## two between-subjects factors (X1,X2) with dichotomous response (Y)
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
#' emmeans(m1, pairwise ~ X1*X2, adjust="holm")
#'
#' m2 = glm.mp(Y ~ X1*X2, data=bs2)
#' Anova.mp(m2, type=3)
#' glm.mp.con(m2, pairwise ~ X1*X2, adjust="holm") # compare
#'
#' ## two between-subjects factors (X1,X2) with polytomous response (Y)
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
#' emmeans::test(
#'   contrast(emmeans(m3, ~ X1*X2 | Y, mode="latent"), method="pairwise", ref=1),
#'   joint=TRUE, by="contrast"
#' )
#'
#' m4 = glm.mp(Y ~ X1*X2, data=bs3)
#' Anova.mp(m4, type=3)
#' glm.mp.con(m4, pairwise ~ X1*X2, adjust="holm") # compare
#'
#' @importFrom stats model.frame
#' @importFrom stats terms
#' @importFrom stats contrasts
#' @importFrom stats 'contrasts<-'
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @importFrom stats poisson
#' @importFrom stats p.adjust
#'
#' @export glm.mp.con
glm.mp.con <- function(
    model,
    formula,
    adjust=c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"),
    ...)
{
  # require the pairwise keyword
  if (formula[[2]] != "pairwise") {
    stop("'pairwise' is required on the left hand side of the ~ .")
  }

  # ensure the model is of class "glm"
  mtype = as.list(class(model))
  if (!any(mtype == "glm")) {
    stop("'model' must be created by glm.mp.")
  }

  # get the data frame used for the model
  df = model.frame(model)

  # df must contain an "alt" factor column or this isn't a model built by glm.mp
  if (!exists("alt", where=df)) {
    stop("'model' must be created by glm.mp.")
  }

  # ensure there are no random factors in the original model formula
  f0 = formula(model)
  t0 = terms(f0)
  iv0 = as.list(attr(t0, "variables"))[c(-1,-2)]
  hasrnd = plyr::laply(iv0, function(term) as.list(term)[[1]] == quote(`|`))
  if (any(hasrnd)) {
    stop("'model' formula cannot have random factors.")
  }

  # get our contrast formula I.V.s
  t = terms(formula)
  IVs = as.list(attr(t, "variables"))[c(-1,-2)]

  # ensure all contrast I.V.s were in the original model formula
  if (!any(IVs %in% iv0)) {
    stop("'formula' terms must be present in 'model'.")
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
    stop("'formula' terms must be factors:", snf)
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

  # get our model's dependent variable
  DV = f0[[2]]

  # now do each of the pairwise comparisons and store them in a table
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
      s = paste0(DV, " ~ alt + ", facname, " + alt:", facname)
      f = as.formula(s) # convert to formula

      # finally, create our model and examine its effects
      m = glm(formula=f, data=d0, family=poisson, ...) # m-P trick
      a = car::Anova(m, type=3)
      a = a[grep("alt:", rownames(a), fixed=TRUE),] # get relevant entry

      # improve our row
      rownames(a)[1] = paste0(lvls[i], " - ", lvls[j]) # update contrast label
      colnames(a)[3] = "p.value" # simplify p-value label
      a = dplyr::mutate(a, .after="Df", N=nrow(d0)/length(levels(df$alt))) # insert N

      # create a new output row entry and add it to our output table
      r = list(Contrast = rownames(a)[1],
               Chisq = round(as.numeric(format(a$`LR Chisq`, scientific=FALSE)), 6),
               Df = a$Df,
               N = a$N,
               p.value=round(a$p.value, 6)
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
