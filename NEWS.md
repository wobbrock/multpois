# multpois 0.3.2

Patch:

* Added \donttest{} directives around slow-running examples involving the ws3
  and icecream data sets, and the glmer.mp.con() function.



# multpois 0.3.1

Patch:

* Updated data and examples to set the levels of responses with "yes", "no", and
  "maybe" categories so they show in that order. Same for the icecream data to
  sort by "vanilla", "chocolate", and "strawberry", and seasons to be "fall", 
  "winter", "spring", and "summer".



# multpois 0.3.0

Minor changes:

* Fixed glmer.mp and glmer.mp.con to use 'alt' factor as a random slope in mixed-
  effects models, which brought its results in line with further testing of 
  lme4::glmer family=binomial.
* Used factor() in glm.mp.con and glmer.mp.con to reset factor levels of composite 
  factors made for post hoc pairwise comparisons. This prevents warnings from the
  formerly unused factor levels being dropped automatically.
* Minor updates to documentation for functions.
* Minor updates to vignette for presentation and writing clarity.



# multpois 0.2.1

Minor changes:

* Fixed unwanted warnings in glmer.mp.con caused when random factors end up having
  fewer levels in the subsetted table than in the full table. The fix was to update
  any random factors' levels in the subset tables constructed dynamically for pairwise
  comparisons. Any post hoc pairwise comparison calculations remain unchanged.



# multpois 0.2.0

Minor changes:

* Fixed reference style in DESCRIPTION to not include the author inside parentheses.
* Added ellipses argument (...) in package functions to be passed through.
* Added check on ellipses argument (...) to ensure they are formulated correctly.
* Added check on data frame arguments to make sure they are, in fact, data frames.
* Updated some error messages for clarity.
* Added type I ANOVAs to Anova.mp and updated documentation.
* Added additional tests for the above.
* Updated function documentation for the above.
* Updated BugReports in DESCRIPTION to be https://github.com/wobbrock/multpois/issues



# multpois 0.1.0

Major changes: 

* Initial CRAN submission.
* Implemented Anova.mp, glm.mp, glm.mp.con, glmer.mp, glmer.mp.con.
* Added all documentation.
* Wrote and passed all unit tests.
