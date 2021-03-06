# Notes


# Tasks done in 2021

DONE 07/01/2021 Check that new model code is the same as the old Sedie code
DONE 14/01/2021 Methods: diagnostics 
DONE 15/01/2021 /discussion: diagnose halictidae probs / re run model & add text for discussion
TODO Intro: one para
TODO Discussion: hurdle model and bsts

# If there is time

- Should learn how to simulate data to test if model is a good fit as [here](https://dev.to/martinmodrak/taming-divergences-in-stan-models-5762)
- Read posterior predictive checks as [here](https://mc-stan.org/docs/2_25/stan-users-guide/simulating-from-the-posterior-predictive-distribution.html)

## Steps to run on HPC for Chapter 2

1. Prepare files by running 01-prep/main.r with model parameters (in `params-02-model.r`) and cut-off date (in `00-init/init.r`).

2. Run `01-prep/main.r` to generate data required for running models.

3. Copy folder `2020-08-31-jsa-type-v2-ch2/` using winSCP

4. Use following code
```
    ssh e0045029@atlas6-c01.nus.edu.sg
    cd ~
    cd /hpctmp2/e0045029
    qsub -q openmp -o 2020-08-31-jsa-type-v2-ch2/output-1.txt 2020-08-31-jsa-type-v2-ch2/main.sh
```
*usually use serial for short runs
*can comment out newly-added packages not needed (if not need to install again)

## Roman's comment on validation: 

What I would do is to fit a model to all the time series except the last 25 years (these 25 years as validation dataset). Then sample the posterior and forecast with that model as you have done. Get a MAPE or RMSE estimate of your forecast for each of the forecasts by comparing with the actual data in the first 5, 10, 15, 20 and 25 years of your validation dataset. That would give you a MAPE or RMSE distribution for predicting at different time intervals. Compare that distribution with the RMSE of a naive approach (just assume time series is constant for the last value https://otexts.com/fpp2/simple-methods.html) for the same time intervals.

I would say that for time lengths in which the median MAPE or RMSE of your forecast distribution is lower than that of your naive forecast, it is reasonable to forecast that far in time.

# High pareto k

https://discourse.mc-stan.org/t/a-quick-note-what-i-infer-from-p-loo-and-pareto-k-values/3446

# Hurdle/ "stan structural time series bsts" models

> Hi Eunice,
> 
> The Stan Manual has a section on zero-inflated and hurdle models, which is a great place to > start.
> 
> For structural time series, Google "stan structural time series bsts" and you should find > some links to forum and blog posts explaining the basics of a bayesian structural time > series model. The bats back in R fits them, but only for continuous response data if I > recall correctly.

> The key will be figuring out how to combine them. I couldnt figure it out in 2016, but maybe someone else has. The hard part is figuring out what the Poisson part of the model looks to as the past.

See: https://mc-stan.org/docs/2_20/stan-users-guide/zero-inflated-section.html

# Other stan tutorials

[Basic stan models](http://avehtari.github.io/BDA_R_demos/demos_rstan/rstan_demo.html)
[Intro to stan & regression](https://jrnold.github.io/bayesian_notes/introduction-to-stan-and-linear-regression.html)
[Hierarchical models](https://vasishth.github.io/bayescogsci/book/hierarchical-models-with-stan.html)
[Q&A for cross validation](https://avehtari.github.io/modelselection/CV-FAQ.html)

# Stan diagnostics

## Typical problems faced

- The largest R-hat is XXX (more than 1), indicating chains have not mixed
* Try: running for more iterations

- Bulk Effective Samples Size (ESS) is too low
* Try: running for more iterations

- There were XX divergent transitions after warmup
* Try: increasing adapt delta

[Diagnostics](https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html)

