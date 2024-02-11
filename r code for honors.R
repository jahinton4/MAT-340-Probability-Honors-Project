##inequality bounds

##markov bound for binomial dist
binomial_markov <- function(n, p, alpha){
  bound <- p/alpha
  return (bound)
}

##chebyshev bound for binomial dist
binomial_chebyshev <- function(n, p, alpha){
  bound <- p*(1-p)/(n*(alpha - p)^2)
  return(bound)
}

##chernoff bound for binomial dist
binomial_chernoff <- function(n, p, alpha){
  bound <- ((1-p)/(1-alpha))^((1-alpha)*n)*(p/alpha)^(alpha*n)
  return(bound)
}

##chernoff bound for binomial dist
binomial_bounds <- function(n, p, alpha){
  curve(1 - pbinom(x-1, n, p), from = 0, to = n)
  
  prob <- 1 - pbinom(alpha*n-1, n, p)
  
  bounds <- c(binomial_markov(n, p, alpha), 
              binomial_chebyshev(n, p, alpha),
              binomial_chernoff(n,p,alpha))
  
  abline(h = prob, lty = 2)
  
  abline(h = bounds, col = c("red", "green", "blue"))
  
  output <- (c(prob, bounds))

  names(output) <- (c("Actual", "Markov", "Chebyshev", "Chernoff"))
  
  return (output)
  }


binomial_markov(100, 1/2, 3/4)
binomial_chebyshev(100, 1/2, 3/4)
binomial_chernoff(100, 1/2, 3/4)

binomial_bounds(10, 1/2, 3/4)

##distributions

rbinom(100, 12, 0.5)
qbinom(0.6, 12, 0.4)
pbinom(0,100, 0.5)




