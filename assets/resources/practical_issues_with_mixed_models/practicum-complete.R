library(tidyverse)
library(ggplot2)
library(lme4)
library(brms)

## First off, a conceptual demonstration that an appropriate ML fit of a mixed effects model can involve a singular fit.
## Consider 10 groups of 10 observations each, with no predictors -- just an intercept -- and *no* by-group random structure.
## Here the correct model is just y ~ 1, which is equivalent to y ~ 1 + (1 | group) with a by-group random intercept variance of 0.
## A non-zero random intercept variance would predict systematic positive within-group correlation among y values, but that would make the 
## fit to the data worse. So, with sufficient data the random intercept variance should be estimated as 0, and this is indeed what happens:
set.seed(1)
dat <- expand_grid(group=factor(paste("G",1:10)),observation=1:10)
dat$y <- rnorm(nrow(dat),0,1)
dat %>%
  group_by(group) %>%
  summarize(mean_y=mean(y)) ## 
summary(lmer(y~1 + (1 | group),dat,REML=F))
## This is a singular fit precisely because the fitting procedure has estimated the correct model parameters!

## For our first real-data case of discussing singular fits, we will use the dataset described on this website:
#  https://kevintshoemaker.github.io/NRES-746/MEM2.html
#  namely this dataset: https://uoftcoders.github.io/rcourse/data/rikz_data.txt
rikz_data <- read_tsv("https://uoftcoders.github.io/rcourse/data/rikz_data.txt")

ggplot(rikz_data,aes(x=Richness)) + geom_histogram(binwidth=1)

ggplot(rikz_data,aes(x=NAP,y=Richness)) + geom_point() + stat_smooth()

# vanilla linear regression suggests plenty of evidence for the Richness ~ NAP relationship, but doesn't take into account the grouping structure of the data
summary(m.lm <- lm(Richness ~ NAP,data=rikz_data))

## Mean level of NAP varies between Beach, hence we want a by-Beach random intercept
rikz_data %>%
  group_by(Beach) %>%
  summarize(mean_NAP=mean(NAP))


## NAP manifestly varies within Beach, hence we want a by-Beach random slope for NAP
ggplot(rikz_data,aes(x=NAP,color=factor(Beach))) + geom_density()

## The mixed linear model with maximal random effects structure (Barr et al., 2013) converges but 
## has a "singular fit" - the point estimate of the covariance matrix is a singular matrix (in particular its correlation parameter is -1.0)
summary(m <- lmer(Richness ~ NAP + (1+ NAP | Beach),data=rikz_data,REML=F))

## Your instinct might be to drop the random slope, or keep the random slope but drop the correlation parameter, 
## or perhaps even to drop the random intercept but keep the random slope.
summary(m.interceptonly <- lmer(Richness ~ NAP + (1 | Beach),data=rikz_data,REML=F))
summary(m.nocorrelation <- lmer(Richness ~ NAP + (1 + NAP || Beach),data=rikz_data,REML=F))
summary(m.nointercept <- lmer(Richness ~ NAP + (0 + NAP | Beach),data=rikz_data,REML=F))

## But any of these choices would non-trivially worsen the fit of the model to the data.
logLik(m)
logLik(m.interceptonly)
logLik(m.nocorrelation)
logLik(m.nointercept)

## If we want to get rid of the "singular fit" complaint, it's straightforward to do so: we can change the
## basis representation of the set of predictors for which there are random effects
## (here, the constant 1 for the random intercept and NAP for the random slope)
## so that all the variability lies in a subspace determined by the smallest possible set of such
## basis vectors, discard the other basis vectors, and then project these predictors onto 
## the reduced basis representation. This involves a spectral decomposition of the estimated
## singular covariance matrix to get its eigenvalues and eigenvectors.
vc <- VarCorr(m)$Beach
eigen_decomp <- eigen(vc)
eigenvalues <- eigen_decomp$values
eigenvectors <- eigen_decomp$vectors
## We drop the eigenvectors that are associated with (near-)zero eigenvalues
nonzero_indices <- which(eigenvalues > 1e-10)  # a small threshold to account for numerical precision issues
zero_indices <- which(eigenvalues <= 1e-10)
informative_basis <- eigenvectors[, nonzero_indices]

## And then we project the predictors onto this informative basis.
rikz_data$X_projected <- model.matrix(m) %*% informative_basis

## Finally, we re-fit the model using the original fixed-effects specification
## but with a random-effects specification involving only random slopes for the 
## reduced, "informative" set of basis vectors (note no random intercept).
m1 <- lmer(Richness ~ NAP + (0 + X_projected|Beach), REML = FALSE,
           data = rikz_data)

## The resulting model is EQUIVALENT to the original model with a singular fit, but 
## doesn't report a singular fit because it isn't -- the new covariance matrix has the same
## rank as the original, but it is full-rank because we have reduced the matrix dimension.
summary(m1)

## In general, one might also worry that singular fit could be a symptom of model misfitting.
## One way of exploring this possibility is fitting the model with multiple different optimizers.
## It turns out that in this case, all optimizers obtain the same fit, strengthening the case that 
## this really is the ML fit.
fits <- allFit(m)

## Finally, we can do Bayesian model fitting if we want instead. The results are generally consistent with the
## results are generally consistent with the singular-fit lme4 model: the fixed effects and 
## estimated variances are similar, and the posterior on the random correlation parameter
## is mostly on highly negative values. Note that the brms fit gives
## CIs on estimates of all the random-effect and noise parameters, whereas lme4 
## gives no information along these lines.
set.seed(1)
m.brm <- brm(Richness ~ NAP + (NAP |Beach), data = rikz_data)
summary(m.brm)



## We didn't get to the below on 5/2 and will return to it on 5/7.
## For discussing flexible hypothesis testing with a fitted brms model, we will use an under-review dataset from a paper of mine.
#  Download the file here: https://osf.io/n54qb?view_only=c49c658e64eb4f9a8d1d153f3c692898
#  and then read it.
haddock_data <- read_csv("~/Downloads/exp2-haddock.csv")
