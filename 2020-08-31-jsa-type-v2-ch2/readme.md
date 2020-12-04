# Notes

## Steps to run on HPC for Chapter 2


1. Prepare files by running 01-prep/main.r with parameter necessary.

2. Copy folder `2020-08-31-jsa-type-v2-ch2/` using winSCP

3. Use following code
```
    ssh e0045029@atlas6-c01.nus.edu.sg
    cd ~
    cd /hpctmp2/e0045029
    qsub -q openmp -o 2020-08-31-jsa-type-v2-ch2/output-1.txt 2020-08-31-jsa-type-v2-ch2/main.sh
```

## Roman's comment on validation: 

What I would do is to fit a model to all the time series except the last 25 years (these 25 years as validation dataset). Then sample the posterior and forecast with that model as you have done. Get a MAPE or RMSE estimate of your forecast for each of the forecasts by comparing with the actual data in the first 5, 10, 15, 20 and 25 years of your validation dataset. That would give you a MAPE or RMSE distribution for predicting at different time intervals. Compare that distribution with the RMSE of a naive approach (just assume time series is constant for the last value https://otexts.com/fpp2/simple-methods.html) for the same time intervals.

I would say that for time lengths in which the median MAPE or RMSE of your forecast distribution is lower than that of your naive forecast, it is reasonable to forecast that far in time.
