## Advanced Data Analysis from Regression to Time Series 

1. [Linear Regression](#linear-regression)

### [Linear Regression](https://www.stat.cmu.edu/~cshalizi/uADA/17/hw/01/hw-01.pdf)

Statisticians often rely on mean squared of errors to assess how well an estimation is, and we can mathematically show that the best prediction by mean-squared error is the expected value of the random variable. Also it worths our attention that the equation of Y = beta * X + e implies a causal model where X is the cause and Y is an effect. In addition, we should be aware of the bias-variance trade-off when analyzing linear regressions. Since constant or linear regression smoothers are somewhat naive, one common approach is the k-Nearest-Neighbor approach where we choose the closest k neighbors from each data point. 

```
Q: What is bias and what is variance?
A: sigma square is the variance and it is the "unpredictable, 'statistical' fluctuation around even the best prediction(Shalizi). Bias (the squared difference of true y and y bar)is the how systematically off our estimations are and we can think of it as the penality of not knowing true predictor. 

Q: What is data smoothing?
A: Data smoothing is the process of removing random noise so that the underlying pattern can stand out. Sometimes the model we build might be overfitting the data - meaning we are just connecting dots rather than fathoming the underlying pattern. 

```




