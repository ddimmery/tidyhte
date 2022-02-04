set.seed(20051920) # 20051920 is derived from 'test'

n <- 1000
mu <- 1:n
mu_hat <- rnorm(n, mu)

test_that("rroc runs", {
    expect_error(calculate_rroc(mu, mu_hat), NA)
})

test_that("rroc is approximately correct", {
    rroc <- calculate_rroc(mu, mu_hat, n)
    pos <- rroc$value
    neg <- rroc$estimate
    ord <- order(pos)
    pos <- pos[ord]
    neg <- neg[ord]
    dpos <- pos[2:n] - pos[1:(n - 1)]
    avgneg <- -(neg[2:n] + neg[1:(n - 1)]) / 2
    aoc <- sum(avgneg * dpos)
    rsd <- sqrt(2 * aoc)
    expect_true(rsd > 0.975 & rsd < 1.025)
})
