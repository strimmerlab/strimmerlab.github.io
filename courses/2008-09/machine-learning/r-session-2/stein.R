

# demonstration of Stein paradox

p=100

#draw some random mean vector
mu = rnorm(p, sd=2) 

# generate data from multiv. normal with diagonal variance
x = rnorm(p, mean=mu)


# ML estimator
muhat.ml = x

# Stein estimator
mx = mean(x)
lambda = min(1, (p-3) / sum( (x-mx)^2))
lambda
muhat.stein = lambda*mx + (1-lambda)*x

# compare the quadratic errors
sum( (muhat.ml -mu)^2 )
sum( (muhat.stein -mu)^2 )


