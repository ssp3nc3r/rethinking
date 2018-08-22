
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

# sample from stanfit
ht <- post$alpha + post$beta %*% t(30:60)
ht <- data.frame(ht) 
colnames(ht) <- 30:60
ht <- reshape2::melt(ht, 
                     variable.name = "Weight", 
                     value.name = "Height_avg") %>%
  mutate(Weight = as.numeric(as.character(Weight)))

# figure 4.7, pg. 106

require(ggplot2); library(ggthemes)

p1 <- ggplot(ht) + 
  geom_point(aes(Weight, Height_avg), alpha = .03, color = "dodgerblue") + 
  theme_tufte(base_family = 'sans') + 
  ylim(140, 180) +
  xlab("weight") + ylab("height")


ht_stats <- ht %>% group_by(Weight) %>% 
  summarise(mu = mean(Height_avg),
            hpdi_l = HPDI(Height_avg)[1],
            hpdi_h = HPDI(Height_avg)[2]) 

p2 <- ggplot(ht_stats) + 
  geom_point(data = d, 
            mapping = aes(weight, height), size = 1, color = "dodgerblue", shape = 1) +
  geom_ribbon(aes(x = Weight, ymin = hpdi_l, ymax = hpdi_h), alpha = .1) +
  geom_line(aes(x = Weight, y = mu)) +
  ylim(140, 180) +
  theme_tufte(base_family = 'sans') 

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)


