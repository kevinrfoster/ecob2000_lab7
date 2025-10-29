Lab 7
================

### Econ B2000, MA Econometrics

### Kevin R Foster, the Colin Powell School at the City College of New York, CUNY

### Fall 2025

We will start the first of a 3-part sequence.

In the first part (Lab 7), we will use basic OLS to estimate some models
of a 0/1 y-variable. In second part (Lab 8) we’ll estimate with logit
and probit models. In third part (Lab 9) we’ll use some additional
machine-learning techniques.

Again we’ll use the couples data,

``` r
library(ggplot2)
library(tidyverse)
library(haven)

setwd("..//ACS_2021_PUMS//") # your directory structure will be different
load("ACS_2021_couples.RData")
```

Let me fix up a couple of the variables with somewhat mysterious coding.

``` r
acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                      "White" = "1",
                                      "Black" = "2",
                                      "American Indian or Alaska Native" = "3",
                                      "Chinese" = "4",
                                      "Japanese" = "5",
                                      "Other Asian or Pacific Islander" = "6",
                                      "Other race" = "7",
                                      "two races" = "8",
                                      "three races" = "9")
                                      
acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")

acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
```

## With 0/1 y-variable

I’ll look at what factors relate to a partner being older and I’ll
choose to consider traditional pairs, where a man and woman are married
and he is placed as householder (in the olden days, would be called
‘head of household’). I’ll create a dummy variable for if the man is
more than 5 years older than the woman. You ~~can~~ should pick a
different number than 5!

``` r
trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )

trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)
```

Note the variable name.

All the math underlying is just concerned with which of the x-variables
make the y-variable more likely to be a higher number. In this case it’s
ok, I’ve set it up for you, but in general you want to confirm which
factor answer is one and which is zero.

For instance,

``` r
table(trad_data$he_more_than_5yrs_than_her,cut(trad_data$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))
```

shows that a one corresponds to ‘he is older by 5 or more years’ and
zero corresponds to ‘not’. But a different person could estimate a model
where the dependent variable is ‘he is *not* older by 5 or more years’
and that would have opposite signs for the estimated coefficients!
Either model could be sensible, as long as you’re clear about which one
the computer is estimating. Be paranoid and check.

You can estimate models something like this (once you figure out what
subset of data you’ll use)

``` r
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)
```

Note that interpreting AGE coefficient is a bit complicated – that’s not
age at marriage but the woman’s current age.

And here I’ve used a couple of the Education dummies, you could also use
a factor with more levels or just the years of education that we
created. You should understand the differences.

``` r
ols_out2 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)
summary(ols_out2)
ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE, data = trad_data)
summary(ols_out3)
```

Try some more complicated factors, too, such as state (but first make a
guess of what states you’d expect to have positive or negative values,
so that you can see what you learn from this). Or maybe `REGION` would
be a bit easier to interpret?

``` r
ols_out4 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + STATEFIP, data = trad_data)
summary(ols_out4)
ols_out5 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + REGION, data = trad_data)
summary(ols_out5)
```

You should be able to demonstrate some complicated hypothesis tests
(such as whether all all of the coefficients on a factor are jointly
zero).

Can you make some graphs to show some of the results from your
regressions?
