Lab 7
================

### Econ B2000, MA Econometrics

### Kevin R Foster, the Colin Powell School at the City College of New York, CUNY

### Fall 2023

This is Part 2 of a 3-part series. Part 1 estimated OLS. Part 2 will
estimate logit models; Part 3 will estimate fancier machine learning
models. (Maybe Part 3a and 3b?)

I know that Lab 6 was a bit short so part of this lab is catching up –
please improve your OLS model.

Carefully look at summary stats, especially for subgroups if those are
important. Pick a particular subgroup of people to consider, and explain
why they’re interesting. Compare the output of the models, in terms of
predictive accuracy. Look if there are particular subgroups where the
prediction is better or worse.

Run several different types of models to explain. Compare the
predictions of the linear model and logit. Look at subgroups to see if
there are particular groups where the models are more confused. Look at
the tradeoff of false positive vs false negative. Are there explanatory
variables (features) that are consistently of little predictive value?
Can you find better ones?

Are these X-variables exogenous? As you add more, think about causality.

``` r
# from last time:
ols_out1 <- lm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use)
summary(ols_out1)



pred_vals_ols1 <- predict(ols_out1, dat_use)
pred_model_ols1 <- (pred_vals_ols1 > mean(pred_vals_ols1))
table(pred = pred_model_ols1, true = dat_use$public_work_num)

# logit 
model_logit1 <- glm(public_work_num ~ female + educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = dat_use, family = binomial
                    )
summary(model_logit1)
pred_vals <- predict(model_logit1, dat_use, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_use$public_work_num)
```

You can play around to see if the “predvals \> mean” cutoff is best or
use ” \> 0.5” or some other value. These give a table about how the
models predict.

And make sure to give me some better output, it’s time to stop dumping
all your output into one file but instead get thoughtful about
presenting results.
