
library(rethinking)
data(Howell1)
d <- Howell1
d <- d[d$age>=18,]

# Example 4.1

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- map(flist, data = d)
precis(m4.1)

# model using stan

dat <- list(N = NROW(d),
            height = d$height)

library(rstan)

m04_1 <- stan(file = 'm04_1.stan',
              data = dat,
              chains = 2,
              iter = 300,
              cores = 2,
              control = list(adapt_delta = .95))

m04_1

# Example 4.2

m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

precis(m4.2)

# model in stan

m04_2 <- stan(file = 'm04_2.stan',
        data = dat,
        chains = 2,
        iter = 300,
        cores = 2,
        control = list(adapt_delta = .95))

m04_2

# see the variance-covariance matrix of m4.1

vcov(m4.1)
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

post <- extract.samples(m4.1, 1e4)
head(post)
precis(post)

plot(d$height, d$weight)

m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- alpha + beta * weight,
    alpha ~ dnorm(178, 20),
    beta ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

precis(m4.3, corr = T)

# stan version example 4.3

dat <- list(N = NROW(d),
            height = d$height,
            weight = d$weight)

m04_3 <- stan(file = 'm04_3.stan',
              data = dat,
              chains = 2,
              iter = 300,
              cores = 2,
              control = list(adapt_delta = .95))

m04_3

# use centering example 4.4

d$weight.c <- d$weight - mean(d$weight)

m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- alpha + beta * weight.c,
    alpha ~ dnorm(178, 20),
    beta ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

precis(m4.4, corr = T)

# in stan example 4.4

dat <- list(N = NROW(d),
            height = d$height,
            weight_c = d$weight.c)

m04_4 <- stan(file = 'm04_4.stan',
              data = dat,
              chains = 2,
              iter = 300,
              cores = 2)

m04_4

# plotting stuff

# using rethinking model

plot(height ~ weight, data = d)
abline(a = coef(m4.3)["alpha"], b = coef(m4.3)["beta"])

# using stan model

post <- as.data.frame(m04_3)
plot(height ~ weight, data = d)
abline(a = mean(post$alpha), b = mean(post$beta))
for(i in 1:20) abline(a = post$alpha[i], b = post$beta[i], col = col.alpha("black", .3))
mu_at_50 <- post$alpha + post$beta * 50

plot(density(mu_at_50))
rethinking::HPDI(mu_at_50, prob = .89)

# sample from posterior
ht <- link(m4.3, data = data.frame(weight = 30:60))

# sample from postterior of stanfit

post <- as.data.frame(m04_3)
mu.link <- function(weight) post$alpha + post$beta * weight
weight.seq <- 30:60
mu <- sapply(weight.seq, mu.link)

# organize and calculate stats

mu <- 
  data.frame(mu) %>% 
  rename_all(function(x) weight.seq) %>%
  reshape2::melt(variable.name = "weight", value.name = "sim.ht") %>%
  mutate(weight = as.integer(as.character(weight))) %>%
  group_by(weight) %>%
  mutate(mu = mean(sim.ht),
         hpdi_l = HPDI(sim.ht)[1],
         hpdi_h = HPDI(sim.ht)[2]) %>%
  ungroup

# figure 4.7, pg. 106

require(ggplot2); library(ggthemes); library(gridExtra)

p <- ggplot(mu) + theme_tufte(base_family = 'sans') + lims(y = c(130, 180))

p1 <- p + 
  geom_point(aes(weight, sim.ht), alpha = .1, shape = 1, color = 'dodgerblue')

p2 <- p + 
  geom_point(data = d, mapping = aes(weight, height), shape = 1, color = 'dodgerblue') +
  geom_ribbon(aes(x=weight, ymin=hpdi_l, ymax=hpdi_h), alpha = .2) +
  geom_line(aes(weight, mu))

grid.arrange(p1, p2, nrow = 1)


# prediction intervals, p. 107

sim.ht <- 
  sapply(weight.seq,
         function(weight)
           rnorm(NROW(post),
                 post$alpha + post$beta * weight,
                 post$sigma))

sim.PI <- apply(sim.ht, 2, PI, prob = 0.89)
sim.PI <- data.frame(weight = weight.seq, PI_l = sim.PI[1,], PI_h = sim.PI[2,])

p2 + geom_ribbon(data = sim.PI,
                 mapping = aes(x=weight, ymin=PI_l, ymax=PI_h), alpha = .05)


# 4.5 polynomial regression, p. 110

d <- 
  Howell1 %>%
  mutate(weight.s = (weight - mean(weight)) / sd(weight))

dat <- list(
  N = NROW(d),
  height = d$height,
  weight_s = d$weight.s
)

m04_5 <- stan(file = 'm04_5.stan',
              data = dat,
              chains = 2,
              iter = 300,
              cores = 2)

m04_5

post <- as.data.frame(m04_5)
weight.seq <- seq(-2.2, 2, length.out = 30)

mu.link <- function(weight) post$alpha + post$beta1 * weight + post$beta2 * (weight^2)
mu <- sapply(weight.seq, mu.link)
mu <- colMeans(mu)

sim.ht <- 
  sapply(weight.seq,
         function(weight)
           rnorm(NROW(post),
                 post$alpha + post$beta1 * weight + post$beta2 * (weight^2),
                 post$sigma))

sim.PI <- apply(sim.ht, 2, PI, prob = 0.89)
sim.PI <- data.frame(weight = weight.seq, PI_l = sim.PI[1,], PI_h = sim.PI[2,])

ggplot() + 
  theme_tufte(base_family = 'sans') + 
  geom_point(data=d, aes(weight.s, height), shape = 1, color = 'dodgerblue') +
  geom_ribbon(data = sim.PI,
              aes(x=weight, ymin=PI_l, ymax=PI_h), alpha = .1) +
  geom_line(aes(weight.seq, mu)) + labs(x = 'Z_weight')


