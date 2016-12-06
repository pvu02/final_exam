# Incremental prediction

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