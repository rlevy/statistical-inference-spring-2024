library(tidyverse)
library(ggplot2)
library(lme4)
library(brms)

## For our first case of discussing singular fits, we will use the dataset described on this website:
#  https://kevintshoemaker.github.io/NRES-746/MEM2.html
#  namely this dataset: https://uoftcoders.github.io/rcourse/data/rikz_data.txt
rikz_data <- read_csv("https://uoftcoders.github.io/rcourse/data/rikz_data.txt")


## For discussing flexible hypothesis testing with a fitted brms model, we will use an under-review dataset from a paper of mine.
#  Download the file here: https://osf.io/n54qb?view_only=c49c658e64eb4f9a8d1d153f3c692898
#  and then read it.
haddock_data <- read_csv("~/Downloads/exp2-haddock.csv")
