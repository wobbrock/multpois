##
## Data.R
##
## Author: Jacob O. Wobbrock
##

#' @title
#' Between-subjects 2&times;2 design with dichotomous response
#'
#' @description
#' This generic synthetic dataset has a dichotomous response \code{Y} and two factors
#' \code{X1} and \code{X2}. The response has categories \code{\{yes,no\}}.
#' Factor \code{X1} has levels \code{\{a,b\}}, and factor \code{X2} has levels
#' \code{\{c,d\}}. It also has a \code{PId} column for participant identifier.
#' Each participant is on only one row.
#'
#' @name bs2
#' @docType data
#' @format A data frame with 60 observations on the following 4 variables:
#' \describe{
#'      \item{PId}{a subject identifier with levels \code{"1"} ... \code{"40"}}
#'      \item{X1}{a between-subjects factor with levels \code{"a"}, \code{"b"}}
#'      \item{X2}{a between-subjects factor with levels \code{"c"}, \code{"d"}}
#'      \item{Y}{a dichotomous response with categories \code{"yes"}, \code{"no"}}
#' }
#'
#' @seealso See \code{\link{glm.mp}} and \code{\link{glm.mp.con}} for complete examples.
#'
#' @keywords datasets
#' @examples
#' data(bs2, package="multpois")
#'
#' bs2$PId = factor(bs2$PId)
#' bs2$Y = factor(bs2$Y)
#' bs2$X1 = factor(bs2$X1)
#' bs2$X2 = factor(bs2$X2)
#' contrasts(bs2$X1) <- "contr.sum"
#' contrasts(bs2$X2) <- "contr.sum"
#'
#' m = glm.mp(Y ~ X1*X2, data=bs2)
#' Anova.mp(m, type=3)
#' glm.mp.con(m, pairwise ~ X1*X2, adjust="holm")
#'
NULL

#' @title
#' Between-subjects 2&times;2 design with polytomous response
#'
#' @description
#' This generic synthetic dataset has a polytomous response \code{Y} and two factors
#' \code{X1} and \code{X2}. The response has categories \code{\{yes,no,maybe\}}.
#' Factor \code{X1} has levels \code{\{a,b\}}, and factor \code{X2} has levels
#' \code{\{c,d\}}. It also has a \code{PId} column for participant identifier.
#' Each participant is on only one row.
#'
#' @name bs3
#' @docType data
#' @format A data frame with 60 observations on the following 4 variables:
#' \describe{
#'      \item{PId}{a subject identifier with levels \code{"1"} ... \code{"60"}}
#'      \item{X1}{a between-subjects factor with levels \code{"a"}, \code{"b"}}
#'      \item{X2}{a between-subjects factor with levels \code{"c"}, \code{"d"}}
#'      \item{Y}{a polytomous response with categories \code{"yes"}, \code{"no"}, and \code{"maybe"}}
#' }
#'
#' @seealso See \code{\link{glm.mp}} and \code{\link{glm.mp.con}} for complete examples.
#'
#' @keywords datasets
#' @examples
#' data(bs3, package="multpois")
#'
#' bs3$PId = factor(bs3$PId)
#' bs3$Y = factor(bs3$Y)
#' bs3$X1 = factor(bs3$X1)
#' bs3$X2 = factor(bs3$X2)
#' contrasts(bs3$X1) <- "contr.sum"
#' contrasts(bs3$X2) <- "contr.sum"
#'
#' m = glm.mp(Y ~ X1*X2, data=bs3)
#' Anova.mp(m, type=3)
#' glm.mp.con(m, pairwise ~ X1*X2, adjust="holm")
#'
NULL

#' @title
#' Within-subjects 2&times;2 design with dichotomous response
#'
#' @description
#' This generic synthetic dataset has a dichotomous response \code{Y} and two factors
#' \code{X1} and \code{X2}. The response has categories \code{\{yes,no\}}.
#' Factor \code{X1} has levels \code{\{a,b\}}, and factor \code{X2} has levels
#' \code{\{c,d\}}. It also has a \code{PId} column for participant identifier.
#' Participant identifiers are repeated across rows.
#'
#' @name ws2
#' @docType data
#' @format A data frame with 60 observations on the following 4 variables:
#' \describe{
#'      \item{PId}{a subject identifier with levels \code{"1"} ... \code{"10"}}
#'      \item{X1}{a within-subjects factor with levels \code{"a"}, \code{"b"}}
#'      \item{X2}{a within-subjects factor with levels \code{"c"}, \code{"d"}}
#'      \item{Y}{a dichotomous response with categories \code{"yes"}, \code{"no"}}
#' }
#'
#' @seealso See \code{\link{glmer.mp}} and \code{\link{glmer.mp.con}} for complete examples.
#'
#' @keywords datasets
#' @examples
#' data(ws2, package="multpois")
#'
#' ws2$PId = factor(ws2$PId)
#' ws2$Y = factor(ws2$Y)
#' ws2$X1 = factor(ws2$X1)
#' ws2$X2 = factor(ws2$X2)
#' contrasts(ws2$X1) <- "contr.sum"
#' contrasts(ws2$X2) <- "contr.sum"
#'
#' m = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws2)
#' Anova.mp(m, type=3)
#' glmer.mp.con(m, pairwise ~ X1*X2, adjust="holm")
#'
NULL

#' @title
#' Within-subjects 2&times;2 design with polytomous response
#'
#' @description
#' This generic synthetic dataset has a polytomous response \code{Y} and two factors
#' \code{X1} and \code{X2}. The response has categories \code{\{yes,no,maybe\}}.
#' Factor \code{X1} has levels \code{\{a,b\}}, and factor \code{X2} has levels
#' \code{\{c,d\}}. It also has a \code{PId} column for participant identifier.
#' Participant identifiers are repeated across rows.
#'
#' @name ws3
#' @docType data
#' @format A data frame with 60 observations on the following 4 variables:
#' \describe{
#'      \item{PId}{a subject identifier with levels \code{"1"} ... \code{"15"}}
#'      \item{X1}{a within-subjects factor with levels \code{"a"}, \code{"b"}}
#'      \item{X2}{a within-subjects factor with levels \code{"c"}, \code{"d"}}
#'      \item{Y}{a polytomous response with categories \code{"yes"}, \code{"no"}, \code{"maybe"}}
#' }
#'
#' @seealso See \code{\link{glmer.mp}} and \code{\link{glmer.mp.con}} for complete examples.
#'
#' @keywords datasets
#' @examples
#' data(ws3, package="multpois")
#'
#' ws3$PId = factor(ws3$PId)
#' ws3$Y = factor(ws3$Y)
#' ws3$X1 = factor(ws3$X1)
#' ws3$X2 = factor(ws3$X2)
#' contrasts(ws3$X1) <- "contr.sum"
#' contrasts(ws3$X2) <- "contr.sum"
#'
#' m = glmer.mp(Y ~ X1*X2 + (1|PId), data=ws3)
#' Anova.mp(m, type=3)
#' glmer.mp.con(m, pairwise ~ X1*X2, adjust="holm")
#'
NULL

#' @title
#' Mixed factorial 2&times;2 design with polytomous response
#'
#' @description
#' This synthetic dataset represents a survey of 40 respondents about their favorite
#' ice cream flavor. Twenty of the respondents were adults and 20 were children. They
#' were queried four times over the course of a year, once in the middle of each season
#' (fall, winter, spring, summer).
#'
#' This dataset has a polytomous response \code{Pref} and two factors,
#' \code{Age} and \code{Season}. The response has the unordered categories
#' \code{\{vanilla, chocolate, strawberry, other\}}. Factor \code{Age} has levels
#' \code{\{adult, child\}}, and factor \code{Season} has levels \code{\{fall, winter, spring, summer\}}.
#' It also has a \code{PId} column for participant identifier. Each participant identifier is repeated
#' four times, once per season.
#'
#' @name icecream
#' @docType data
#' @format A data frame with 160 observations on the following 4 variables:
#' \describe{
#'      \item{PId}{a subject identifier with levels \code{"1"} ... \code{"40"}}
#'      \item{Age}{a between-subjects factor with levels \code{"adult"}, \code{"child"}}
#'      \item{Season}{a within-subjects factor with levels \code{"fall"}, \code{"winter"}, \code{"spring"}, \code{"summer"}}
#'      \item{Pref}{a polytomous response with categories \code{"vanilla"}, \code{"chocolate"}, \code{"strawberry"}, \code{"other"}}
#' }
#'
#' @seealso See \code{vignette("multpois", package="multpois")} for a complete analysis of this data set.
#'
#' @keywords datasets
#' @examples
#' data(icecream, package="multpois")
#'
#' icecream$PId = factor(icecream$PId)
#' icecream$Pref = factor(icecream$Pref)
#' icecream$Age = factor(icecream$Age)
#' icecream$Season = factor(icecream$Season)
#' contrasts(icecream$Age) <- "contr.sum"
#' contrasts(icecream$Season) <- "contr.sum"
#'
#' m = glmer.mp(Pref ~ Age*Season + (1|PId), data=icecream)
#' Anova.mp(m, type=3)
#' glmer.mp.con(m, pairwise ~ Age*Season, adjust="holm")
#'
NULL
