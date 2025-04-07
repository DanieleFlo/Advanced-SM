library(lme4) # for the analysis
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages


load("DataRegression2025_unical.RData")
str(RiceFarms)
