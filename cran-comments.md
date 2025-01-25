## R CMD check results

0 errors | 0 warnings | 0 notes


* This release (0.3.0) has minor changes:

* Fixed glmer.mp and glmer.mp.con to use 'alt' factor as a random slope in mixed-
  effects models, which brought its results in line with further testing of 
  lme4::glmer family=binomial.
* Used factor() in glm.mp.con and glmer.mp.con to reset factor levels of composite 
  factors made for post hoc pairwise comparisons. This prevents warnings from the
  formerly unused factor levels being dropped automatically.
* Minor updates to documentation for functions.
* Minor updates to vignette for presentation and writing clarity.
