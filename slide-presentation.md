Sampling Distribution of the Sample Mean App
========================================================
author: Harry Ramirez
date: 06/17/2019
autosize: true
<h3 style="color: #ffffff;">Learn how it works...</h3>
<div align="center">
<img src="./images/AppScreen1.png" height=270>
</div>


App Description
========================================================

The purpose of our app is to graphically show how the distribution of sample mean becomes approximately normal as more random samples are drawn from the original population. You can experiment with a variety of population distribution shapes and sample sizes.

After using the app you will intuitively become aware of how the distribution of sample mean behaves, and its importance in the field of inferential statistics.

<div align="center">
<img src="./images/SampMeanDist2.png">
</div>


Population Models
========================================================
left: 55%
class: rslide

You may select from a variety of population distributions:

- Normal
- Uniform
- T
- F
- Chi-square
- Exponential

For each population type, you may adjust its parameters and view their
effect over the distribution of sample mean.

***


```r
population <- rchisq(n=10000, df=5)
hist(population,
     xlab = "X Population",
     main = "Population Histogram",
     col = "#317EACDA",
     border = "#BBBBBB")
```

<img src="slide-presentation-figure/popu-1.png" title="plot of chunk popu" alt="plot of chunk popu" style="display: block; margin: auto;" />

Sampling
========================================================

You can set the sample size, the number of repeats to do each time the "Draw Samples" button is used. A "Reset" button let you clear previous re-sampling data and start from zero. There is an option to set a random seed to ensure repeatable results.

<div align="center">
<img src="./images/SampDrawOpt4.png" height=406>
</div>


Results
========================================================

A histogram and summary statistics are generated for the population. Additional ones are updated after each sample draw for the sample and the sample mean distributions.  Visit our App at [https://bit.ly/2KT5Ouk](https://bit.ly/2KT5Ouk).

<div align="center">
<img src="./images/AppScreen5.png" height=410>
</div>
