Lab 7
================

### Econ B2000, MA Econometrics

### Kevin R Foster, the Colin Powell School at the City College of New York, CUNY

### Fall 2022

This is Part 1 of a 2-part series. Part 1 will estimate OLS and logit
models; Part 2 will estimate fancier machine learning models.

We will use ATUS data, which combines demographic info with details
about how people spent their time each day.

Part of the challenge of this estimation is that many time-use decisions
can be modeled as having 2 segments: 1 does the person spend any time at
all on the activity; 2 how much time (if greater than zero)? In later
econometrics classes you will learn techniques about putting those
together but for now we’ll experiment.

The steps below are a bit more complex, because we’ll be structuring the
data into a format that lots of different models can all use.

For this week, choose a time use variable to model: options are minutes
of time spent caring for household members; caring for non-household
members; on education; on eating and drinking; on government service or
civic obligations; on household chores; on supervising household
services; on personal care (sleeping, washing, etc); on the phone; on
professional services (bank, legal, veterinary, paid childcare); on
consumer purchasing; on religious activity; on socializing or relaxing
(including TV and computer); on sports or exercise (either doing or
watching); on travel; on volunteer activities; or time spent working
(including searching for a job).

Carefully look at summary stats, especially for subgroups if those are
important. Pick a particular subgroup of people to consider, and explain
why they’re interesting. Estimate a variety of models to predict both
whether the person spends time on an activity (both OLS and logit
models) and then how much. Compare logit with OLS for each stage, in
terms of predictive accuracy.

And give me some better output, it’s time to stop dumping all your
output into one file but instead get thoughtful about presenting
results.

Form your lab group.

Decide on how you’re defining your subgroup then find some basic
statistics. Explain what you’re doing with NA.

Run several different types of models to explain time use with some
explanatory variables. For instance if you were looking at time spent on
sports, you might start with
`any_time_sports <- (dat_ATUS$ACT_SPORTS > 0)` then models of type
`any_time_sports ~ AGE + SEX + RACE + HISPAN + MARST + EDUC` . Compare
the confusion matrix for linear model and logit. Look at subgroups to
see if there are particular groups where the models are more confused.
Look at the tradeoff of false positive vs false negative. Are there
explanatory variables (features) that are consistently of little
predictive value? Can you find better ones?

Are these X-variables exogenous? As you add more, think about causality.

We want to set up the data in a way that is common to all of the models.

Some of the estimation procedures are not as tolerant about factors so
we need to set those as dummies. Some are also intolerant of NA values.
I’ll show the code for the basic set of explanatory variables, which you
can modify as you see fit.

The R command `model.matrix()` creates a set of dummy variables out of a
factor. Run this to see a for-instance:

``` r
d_educ <- data.frame(model.matrix(~ dat_ATUS$EDUC))
summary(d_educ)
levels(dat_ATUS$EDUC)
```

That takes the single factor EDUC and creates instead a matrix of dummy
variables corresponding to each level (with one omitted) of the factor –
basically this is what R is doing behind the scenes when you run a
regression with a factor. We’ll do this for all of the factors in the
regression. You should understand what is the omitted level of Education
since that becomes relevant to interpreting the model later.

Might want to recode – the EDUC factor has a very fine classification,
some used by as few as 1 in 10,000!

``` r
require(tidyverse)
dat_ATUS$EDUC_r <- recode_factor(dat_ATUS$EDUC, "\"Less than 1st grade\"" = "ltHS", "\"1st, 2nd, 3rd, or 4th grade\"" = "ltHS", "\"5th or 6th grade\""  = "ltHS",
                                 "\"7th or 8th grade\"" = "ltHS", "\"9th grade\"" = "ltHS", "\"10th grade\"" = "ltHS", "\"11th grade\"" = "ltHS", 
                                 "\"12th grade - no diploma\"" = "ltHS",
                                 "\"High school graduate - GED\"" = "HS", "\"High school graduate - diploma\"" = "HS", "\"Some college but no degree\"" = "some_college",
                                 "\"Associate degree - occupational vocational\"" = "associate", "\"Associate degree - academic program\"" = "associate",
                                 "\"Bachelor's degree (BA, AB, BS, etc.)\"" = "bachelor", "\"Master's degree (MA, MS, MEng, MEd, MSW, etc.)\"" = "master",
                                 "\"Professional school degree (MD, DDS, DVM, etc.)\"" = "prof_or_PhD", "\"Doctoral degree (PhD, EdD, etc.)\"" = "prof_or_PhD",
                                 .default = "D")
# the default shouldn't catch any but it's a useful check
# those fuckin \ characters are a giant ass pain, but just copy-paste from labels(dat_ATUS$EDUC)...

summary(dat_ATUS$EDUC_r)
d_educ_r <- data.frame(model.matrix(~ dat_ATUS$EDUC_r))
```

Set up the other dummy variables as well.

``` r
d_marstat <- data.frame(model.matrix(~ dat_ATUS$MARST))
d_race <- data.frame(model.matrix(~ dat_ATUS$RACE)) # probably want to recode, similar to EDUC
d_hispanic <- data.frame(model.matrix(~ dat_ATUS$HISPAN)) # maybe recode
d_sex <- data.frame(model.matrix(~ dat_ATUS$SEX))
d_region <- data.frame(model.matrix(~ dat_ATUS$REGION))

d_any_time_sports <- data.frame(model.matrix(~ any_time_sports)) # or whatever time use you choose 

# note that, depending on your subgroup, this might snip off some columns so make sure to check summary() of each -- don't want Min = Max = 0!
# and note that if there are NA values then these can get more complicated
# in this case HISPAN has some NA values

dat_for_analysis_sub <- data.frame(
  d_any_time_sports[ !is.na(dat_ATUS$HISPAN) ,2],
  dat_ATUS$AGE[!is.na(dat_ATUS$HISPAN)],
  d_educ_r[!is.na(dat_ATUS$HISPAN),2:7],
  d_marstat[!is.na(dat_ATUS$HISPAN),2:6],
  d_race[!is.na(dat_ATUS$HISPAN),2:20],
  d_hispanic[,2:5],
  d_sex[!is.na(dat_ATUS$HISPAN),2],
  d_region[!is.na(dat_ATUS$HISPAN),2:4]) # need [] since model.matrix includes intercept term


# this is just about me being anal-retentive, see difference in names(dat_for_analysis_sub) before and after running this bit
names(dat_for_analysis_sub)
names(dat_for_analysis_sub) <- sub("dat_ATUS.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "any_time_sports"
names(dat_for_analysis_sub)[2] <- "AGE"
names(dat_for_analysis_sub)[37] <- "SEX"
names(dat_for_analysis_sub)
```

Make sure you understand what is the omitted category in each of these
creations. Some are a bit tricky!

Next create a common data object that is standardized (check what it
does! run ‘summary(sobj$data)’) and split into training and test sets. I
have to use a very small training set to prevent my little laptop from
running out of memory (not for these but for later fancier techniques).
You can try a bigger value like `runif(NN) < 0.5` or something for now.
`summary(restrict_1)` will tell you how many are in the training set vs
test.

``` r
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$any_time_sports)
restrict_1 <- (runif(NN) < 0.1) # use 10% as training data
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(any_time_sports ~ AGE + EDUC_rHS + EDUC_rsome_college + EDUC_rassociate + EDUC_rbachelor + EDUC_rmaster + EDUC_rprof_or_PhD + 
                    MARST.Married...spouse.absent.+ MARST.Widowed. + MARST.Divorced. + MARST.Separated. + MARST.Never.married. +
                    RACE.Black.only. + RACE.American.Indian..Alaskan.Native. + RACE.Asian.only. +
                    HISPAN.Mexican. + HISPAN.Puerto.Rican. + HISPAN.Cuban. + HISPAN.Other.Spanish. +
                    SEX + REGION.Midwest. + REGION.South. + REGION.West.
                      , dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)
```

Then start with some models. I’ll give code for the Linear Probability
Model (ie good ol’ OLS) and logit, to show how to call those with the
standarized object.

``` r
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$any_time_sports)
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$any_time_sports)
```

You can play around to see if the “predvals \> 0.5” cutoff is best.
These give a table about how the models predict.

*Note this might throw some red messages but it still works*
