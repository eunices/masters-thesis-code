# http://elevanth.org/blog/2017/11/28/build-a-better-markov-chain/
y = sum(rpois(20, 2))
n = 1e4
p = rep(1, n)
for (i in 2:n) {
    r = p[i-1]
    q = exp(log(r) + rnorm(1)/9)
    p[i] = ifelse(runif(1) < q^y * r^(-y) * exp(-20*(q-r)), q, r)
}

# These algorithms take samples from a target distribution by first (1) making a random proposal for 
# new parameter values and then (2) accepting or rejecting the proposal. If both steps are done right,
# then the accepted parameter values will comprise samples from the target distribution.

