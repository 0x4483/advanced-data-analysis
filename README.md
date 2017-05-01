## Advanced Data Analysis from Regression to Time Series 

Notice: this note is based on Cosma Shalizi's lecture [36-402 Advanced Data Analysis](https://www.stat.cmu.edu/~cshalizi/uADA/17/) and all credits should go to [Professor Shalizi](https://en.wikipedia.org/wiki/Cosma_Shalizi). 

1. [Linear Regression](#linear-regression)

### [Linear Regression](https://www.stat.cmu.edu/~cshalizi/uADA/17/hw/01/hw-01.pdf)

Statisticians often rely on mean squared of errors to assess how well an estimation is, and we can mathematically show that the best prediction by mean-squared error is the expected value of the random variable. Also it worths our attention that the equation of `Y = b * X + e` implies a causal model where `X` is the cause and `Y` is an effect. In addition, we should be aware of the bias-variance trade-off when analyzing linear regressions. Since constant or linear regression smoothers are somewhat naive, one common approach is the k-Nearest-Neighbor approach where we choose the closest `k` neighbors from each data point. Click here to [view](./img/1.png) how the value of `k` changes how smooth the prediction is to the data points. As the `k` increases, we are closer and closer to a linear smoother and when `k` is `1` we are basically just connecting the dots. However, one might immediately notice that the weakness of kth nearest neighnor algothrim: "each testing point is predicted using information from only a few of the training data points"(Shalizi). Therefore, we might want to consider a regression method that combines the power of a linear regression and the kth nearest neighbor, and that is waht kernel regression does. When modeling using Kernel Regression, the closest neighor is given weight of `1` and the furthest away neighbor is given a value close to `0`. Similar to the parameter `k` in the k nearest neighbor, the kernel regression have tuning parameter `h` or the `bandwidth` for selection. Click here to [view](https://upload.wikimedia.org/wikipedia/commons/thumb/2/2a/Kernel_density.svg/250px-Kernel_density.svg.png) how the value of bandwidth affects the smoothness of the function. Notice that when `h` is very small, it is more likely to "connect the dots" rather than looking for underlying model. 

```
Q: What is bias and what is variance?
A: sigma square is the variance and it is the "unpredictable, 'statistical' fluctuation around even the best prediction(Shalizi). Bias (the squared difference of true y and y bar)is the how systematically off our estimations are and we can think of it as the penality of not knowing true predictor. 

Q: What is data smoothing?
A: Data smoothing is the process of removing random noise so that the underlying pattern can stand out. Sometimes the model we build might be overfitting the data - meaning we are just connecting dots rather than fathoming the underlying pattern. 
```





