my.f2<-.20/(1-.20)
print(my.f2)

# f2 = .25

# Degrees of freedom = u = 2

library(pwr)
pwr.f2.test(u=2, f2=.25, power=.85)

# N=u+v+1
N = 2+49+1
print(N)

# N = 52

# Safe guard Power Analysis

library(MBESS)

ci.R2(R2=.20, p=2, N=52, Random.Predictors=FALSE)

# Lower bound = 0.0246

my.f2 <- 0.0246/(1-0.0246)
print(my.f2)

# f2 = 0.0252

library(pwr)
pwr.f2.test(u=2, f2=0.0252, power=.85)

N = 2 + 433 + 1
print(N)

# Incremental prediction with safeguard

# u = 1

# f2

my.f2 <- .1 / (1 - .20)
print(my.f2)

# f2 = 0.125

library(pwr)
pwr.f2.test(u=1, f2=0.125, power=.85)

N = 1 + 72 + 2
print(N)
# N = 75