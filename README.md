# mleda

An R package with useful functions for **M**ulti**-L**evel **E**xploratory **D**ata **A**nalysis, specifically for two-level hierarchical data structures.

This package contains two main functions:

* `plot_ml` allows the user to plot the relations between a set of variables and a given outcome. Plots include both main effects and two-way interactions, and can be based on either loess curves using raw data or predictions from a [randomForest](http://cran.r-project.org/web/packages/randomForest/index.html), [cforest](http://cran.r-project.org/web/packages/party/index.html), or [lme4](http://cran.r-project.org/web/packages/lme4/index.html) model object using a modified [partial dependence plot](http://dpmartin42.github.io/posts/partial-dependence-1/).

* `validate_ml` performs either split-half or 5-fold cross-validation (both at the cluster level) to estimate test performance for randomForest, cforest, or lme4 models. In the continuous case, proportion of variation is reported (i.e., 1 - MSE/var(y)). In the classification case, accuracy is reported (i.e., (TP + TN)/(TP + TN + FP + FN)).

With regards to the modified partial dependence plot, the following procedure is used. Instead of repeating the entire training set, a new dataset is created that consists of one observation: the median value for all continuous variables and the most-endorsed level for either categorical or ordinal variables. This new observation is then varied across all the joint values of the predictions of interest. While not a true partial dependence plot, it typically will yield similar results and has the same computation time regardless of the sample size of the training set. 

This approach has been referred to as a "poor-man's" partial dependence plot and can be used for non-multilevel data using the [plotmo](http://cran.r-project.org/web/packages/plotmo/plotmo.pdf) package on CRAN. For those interested in true partial dependence plots, I recommend taking a look at the [edarf](https://github.com/zmjones/edarf/) package developed by [Zachary M. Jones](http://zmjones.com) and [Fridolin Linder](http://polisci.la.psu.edu/people/fjl128) in support of "[Exploratory Data Analysis Using Random Forests](https://github.com/zmjones/rfss/)."

# Installation

This package can be installed from github using [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```{r}
library(devtools)
install_github("dpmartin42/mleda")
```