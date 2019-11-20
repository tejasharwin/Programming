#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
llr <- function(y,x,xlims,n.grid=100,h=((max(x))-(min(x)))/4) {
#######################################################################
##                                                                   ##
##    Local Linear Regression                                        ##
##    -----------------------                                        ##
##                                                                   ##
##  This scipt defines a function that enables the user to           ##    
##  estimate the values of the regression function f(.)              ##
##  in the regression model Yi = f(xi) + ei over a grid of           ##
##  values of x0, where {xi,yi} are pairs of obervations,            ##
##  ei is the corresponding error term and x0 is notation            ##
##  for values in a grid of x. For a specific value x0, the local    ##
##  linear regression estimate of f(x0) is obtained as the fitted    ##
##  value from a weighted least-squares regression of the {yi}       ##
##  upon the {xi}: specifically, it is the value of B0 obtained by   ##
##  minimising the sum of squared errors, of the regression.         ##
##                                                                   ##  
##                                                                   ##
##    The arguments of the llr function are:                         ##  
##                                                                   ##
##    y         A vector consisting of {yi}, the oberservations      ##
##              of the response variable,                            ##
##    x         A vector consisting of {xi}, the oberservations      ##
##              of the explanatory variable                          ##
##    xlims     A vector consisting of 2 values:                     ##
##              minimum of x and maximum of x                        ##
##    n.grid    The number of points required in the grid of x.      ##
##              The default value is 100.                            ##    
##    h         The standard deviation of the normal distribution    ##
##              whose probability density function is used to        ##
##              calculate the weights for each observation           ##
##              corresponding to each grid value of x.               ##
##              The default value is the range of x divided by 4.    ##
##                                                                   ##      
#######################################################################
  #
  # We convert the vectors x and y into a matrix for ease of computation  
  #
  x.mat <- as.matrix(x)
  y.mat <- as.matrix(y)
  #
  # We split x into 100 ( as specified by the default value of n.grid) 
  # equally spaced values and then convert it into a matrix for ease of computation.
  # x0.grid.mat, a matrix of dimensions 100 x 1.
  # It contains the grid of values of x0 in increasing order.
  #
  x0.grid.mat <- as.matrix(seq(xlims[1],xlims[2],length.out=n.grid))
  #
  # A.mat, is a matrix of dimensions n x m
  # n = the number of observations in our sample
  # m = the number of points in the grid of x, ie no.of rows in x0.grid.mat
  # Each column in A displays n weights for the corresponding x0 value.
  #
  A.mat<- as.matrix(sapply(x0.grid.mat, function(x) dnorm((x.mat-x), mean = 0, sd = h)))
  #
  # x.new, is a matrix of dimensions that are the same as A.mat.
  # Each column in x.new displays {xi-x0} for all i = 1,2,...n, for the corresponding x0 value.
  #
  x.new<- as.matrix(sapply(x0.grid.mat, function(x) x.mat-x))
  #
  # B.mat, is a matrix of dimensions that are the same as A.mat
  # It is the product of A.mat and x.new WITHOUT using matrix multiplication. Which means 
  # only the elements of the same position in the matrices are multiplied.
  #
  B.mat <- A.mat*x.new
  #
  # C.mat, is a matrix of dimensions that are the same as A.mat
  # x.new^2 means each element in the matrix x.new is squared.
  #
  C.mat <- A.mat*(x.new^2)
  #
  # colSums(A),  is a vector of length m
  # m = the number of points in the grid of x, ie no.of rows in x0.grid.mat
  # Each columnar element of colSums(A) should be interpreted as: The sum of weights 
  # for all observations, for the corresponding x0 value.
  # colSums(C) and colSums(B) should be interepreted in the same fashion. 
  # 
  # Denominator, is a vector of length m 
  # Each columnar element of Denominator should be interpreted as:
  # the denominator in the formula of the estimate of B0 in the weighted least squares regression, for
  # the corresponding value of x0.
  # 
  Denominator <- (colSums(A.mat)*colSums(C.mat)- (colSums(B.mat))^2)
  #
  # Numerator, is a vector of length m 
  # Each columnar element of Numerator should be interpreted as:
  # the numerator in the formula of the estimate of B0 in the weighted least squares regression, for
  # the corresponding value of x0.
  #
  Numerator <- colSums(A.mat*(t(colSums(C.mat)-t((t(colSums(B.mat)*t(x.new))))))*y)
  #
  # Finally, the required estimates of f(x0) for each value of x0.
  #
  f.hat <- Numerator/Denominator
  x0.grid <-(seq(xlims[1],xlims[2],length.out=n.grid))
  #
  list(x,y,x0.grid,f.hat)
}
