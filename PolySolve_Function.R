#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
polysolve <- function(a, lower, upper, tol = 1e-6) {
#######################################################################
##                                                                   ##
##    The false position algorithm                                   ##
##    ----------------------------                                   ##
##                                                                   ##
##  This scipt defines a function that enables the user to           ##    
##  estimate a root of a continous function h(x) of a single         ##     
##  variable x, using the false position algorithm; where            ##
##  h(x) is a degree p polynomial of the form:                       ##
##                                                                   ##
##  h(x)= a0 + a1x + a2x^2 + . + apx^p                               ##
##                                                                   ##
##  The root of the polynomial is defined to be the value of x       ##       
##  for which the function h(x) equals 0, which means the value      ##
##  of x at which the polynomial curve meets the y-axis.             ##
##                                                                   ##
##                                                                   ##  
##    The arguments of the polysolve function are:                   ##  
##                                                                   ##
##    a         a vector of coefficients such that a[1] represents   ## 
##               a0, a[2] represents a1, and so on                   ##
##    lower     The value of the x for which the user guesses the    ##
##              root to be above of                                  ##
##    upper     The value of the x for which the user guesses the    ##
##              root to be below of                                  ##
##    tol       The "tolerance": the algorithm is judged to          ##
##               have converged when either the absolute value of    ##
##              h(M) or the absolute value of U - L is less than     ## 
##              this. The default value is 10^-6.                    ##
##                                                                   ##
#######################################################################  
  # 
  # define the degree of the polynomial.
  #
  p <- length(a)-1 
  #
  # Start by initialising all the quantities that we need, 
  # as follows:
  #
  # The value of the function h(x) when x takes the value "lower"
  h.lower<- sum(a*lower^(0:p))
  #
  # The value of the function h(x) when x takes the value "upper"
  h.upper<- sum(a*upper^(0:p))
  #
  # Calculate the "middle" and the corresponding value of h(x) when x takes the value "middle"
  #
  M<- lower-(((upper-lower)*h.lower )/ (h.upper - h.lower)) 
  h.M<- sum(a*M^(0:p))
  #
  # Now to construct the algorithm.
  # An if statement that checks whether the user entered appropriate guesses of x 
  # which satisfy the necessary conditions for the algorithm to run.
  # if any of the two conditions: lower < upper or h.lower*h.upper < 0 are not true,
  # then the function returns NA and an appropriate warning message.
  #
  if(!(lower < upper & h.lower*h.upper < 0)) {
  #  
    warning("lower not less than upper and/or h.lower*h.upper is not less than 0")
    return(NA)
  #  
  } else { # An else statement that runs if both conditions in the if statement are satisfied.    
  #
  # Loop begins. The while loop will become FALSE as soon as either 
  # EITHER the absolute value of the difference between upper and lower becomes greater than tol 
  # or the absolute value of h.M becomes greater than tol. 
  #
    while (abs(upper - lower) > tol & abs(h.M) > tol) { 
  #
      if(h.M*h.lower < 0) {                      # This if statement scales down the range of values of x for which a root lies in 
                                                 # by re-assigning the value of upper by M, thereafter the loop calculates the revised value 
        upper <- M                               # of h(x) when x takes the value of upper.
        h.upper <- sum(a*upper^(0:p)) 
  #  
      } else if(h.M*h.upper < 0) {               # The loop continues with an else if function which aims to scale down the range of x values
                                                 # for which a root lies in, by re-assigning the value of lower by M, thereafter the loop
        lower <- M                               # calculates the revised value of h(x) when x takes the value of lower.
        h.lower <- sum(a*lower^(0:p))
  # 
      }
  #
      M <- lower - ((upper-lower)*h.lower)/(h.upper-h.lower)  # revised value of M to calculate h.M
      h.M <- sum(a*M^(0:p))                                   # revised h.M value that is used to evaluate the conditions of the while loop
  #
    }
  }
  return(M) # returns the value of M which causes the while loop to be evaluated as FALSE, ie the estimate of a root.                                 
}

