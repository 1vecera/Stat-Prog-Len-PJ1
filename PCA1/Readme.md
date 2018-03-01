
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **XFGiv05** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: SPL_Analaysis_of_a_Fifa_DataSet_PCA1

Published in: Statistical Programming Languages - Student Project on Analaysis of a FIFA Data set

Description: 'The PCA helps to summarize a quantitative dataset with many variables: see the correlations between the variables ,represent the p-dimensional point cloud of indivuals (here the players) by projecting them on spaces of smaller dimension ,construct new variables called principal components that are uncorrelated and that synthesize information.'

Keywords: PCA,reduction of the dimension, quantitative variables

Author: Julien Kraemer

See also: other Quantlets in this project

Submitted: 14.03.2018

```

![Picture1](factor loadings.png)


### R Code:
```r
# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()
 
x          = read.table("implvola.dat") # load data
x          = x*100                      # scale
n          = nrow(x)                    # number of rows
z          = x[2:n,] - x[1:(n-1),]      # first difference
s          = cov(z)*100000              # covariance of returns
tmp        = eigen(s)                   # spectral decomposition
l          = tmp$values                 # eigenvalues
g          = tmp$vectors                # eigenvectors
g[,2]      = g[,2]*(-1)                 # correction of sign for publication purpose
  
gr1        = cbind(1:8,g[,1])           # first principal component
gr2        = cbind(1:8,g[,2])           # second principal component

plot(gr1,type="l",col="blue3",xlab="Subindex",ylab="Percentage [%]",lwd=2,ylim=c(min(g[,1:2]),max(g[,1:2])))
points(gr1,col="blue3",lwd=2,pch=1)
title("Factor Loadings")
lines(gr2,col="darkgreen",lwd=2)
points(gr2,col="darkgreen",lwd=2)


```
