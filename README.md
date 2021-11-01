Lab 7
================

### Econ B2000, MA Econometrics

### Kevin R Foster, the Colin Powell School at the City College of New York, CUNY

### Fall 2021

For this lab, we will estimate a variety of models to try to predict if
a person got vaxx (same data as last week). Compare logit with OLS in
terms of prediction and set up the variables to be ready to expand into
other models (next week). And give me some better output, it’s time to
stop dumping all your output into one file but instead get thoughtful
about presenting results.

Form your lab group.

First decide on how you’re defining your subgroup (all adults or 12+?
Within certain age? Other?) then find some basic statistics – what
fraction are not vaxxed? (Later go back to look at simple stats for
subgroups to see if there are sharp differences.) Explain what you’re
doing with NA. You did this last week (along with defining `vaxx`) so
check back. You might do the same or choose to improve.

Run several different types of models to explain vaccination rates with
some explanatory variables, `vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE +
RHISPANIC + GENID_DESCRIBE + REGION`. Compare the confusion matrix for
linear model and logit. Look at subgroups to see if there are particular
groups where the models are more confused. Look at the tradeoff of false
positive vs false negative. Are there explanatory variables (features)
that are consistently of little predictive value? Can you find better
ones?

Are these X-variables exogenous? As you add more, think about causality.

We want to set up the data in a way that is common to all of the models.

Some of the estimation procedures are not as tolerant about factors so
we need to set those as dummies. Some are also intolerant of NA values.
I’ll show the code for the basic set of explanatory variables, which you
can modify as you see fit.

The R command `model.matrix()` creates a set of dummy variables out of a
factor. Run this to see a for-instance:

``` r
d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
summary(d_educ)
levels(dat_use1$EEDUC)
```

That takes the single factor EEDUC and creates instead a matrix of dummy
varibles corresponding to each level (with one omitted) of the factor –
basically this is what R is doing behind the scenes when you run a
regression with a factor. We’ll do this for all of the factors in the
regression. You should understand what is the omitted level of Education
since that becomes relevant to interpreting the model later.

Set up the other dummy variables as well.

``` r
d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))

d_vaxx <- data.frame(model.matrix(~ dat_use1$vaxx)) # check number of obs to see that this snips off NA values

# note that, depending on your subgroup, this might snip off some columns so make sure to check summary() of each -- don't want Min = Max = 0!

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],
  dat_use1$TBIRTH_YEAR[!is.na(dat_use1$vaxx)],
  d_educ[!is.na(dat_use1$vaxx),2:7],
  d_marstat[!is.na(dat_use1$vaxx),2:6],
  d_race[!is.na(dat_use1$vaxx),2:4],
  d_hispanic[!is.na(dat_use1$vaxx),2],
  d_gender[!is.na(dat_use1$vaxx),2:5],
  d_region[!is.na(dat_use1$vaxx),2:4]) # need [] since model.matrix includes intercept term


# this is just about me being anal-retentive, see difference in names(dat_for_analysis_sub) before and after running this bit
names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[2] <- "TBIRTH_YEAR"
names(dat_for_analysis_sub)[17] <- "Hispanic"
```

Make sure you understand what is the omitted category in each of these
creations. Some are a bit tricky\!

Next create a common data object that is standardized (check what it
does\! run ‘summary(sobj$data)’) and split into training and test sets.
I have to use a very small training set to prevent my little laptop from
running out of memory (not for these but for later fancier techniques).
You can try a bigger value like `runif(NN) < 0.5` or something for now.
`summary(restrict_1)` will tell you how many are in the training set vs
test.

``` r
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.1) # use 10% as training data
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(vaxx ~ TBIRTH_YEAR + EEDUCsome.hs + EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      REGIONSouth + REGIONMidwest + REGIONWest
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
table(pred = pred_model_lpm1, true = dat_test$vaxx)
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)
```

You can play around to see if the “predvals \> 0.5” cutoff is best.
These give a table about how the models predict.

*Note this might throw some red messages but it still works*

### Note on NA

Here I’ve chosen to set NA responses to *are you vaxxed?* to be missing
so R drops them. However other NA responses such Marital Status or
Gender ID are not omitted but rather set as additional levels in those
factors. Partly that’s to keep the Y-variable binary just yes or no,
whereas the X-variables can have additional levels. I’m interpreting NA
responses to many of these questions as, *it’s complicated*. There are
other cases where NA responses have different interpretations, for
example a question about how Covid impacted childcare would get NA
answer if the person doesn’t have kids needing that care.

You can try different ways of dealing with this: it might be plausible
to count NA answers to vaxx question as ‘no’. Put that into the
estimation and see if there are changes. Or check some of the other NA
answers to see if there are some people who give complicated answers to
many questions or whether these are scattered.

This is an area of ‘researcher degrees of freedom’ where plausible
choices might impact the conclusions of the analysis.
