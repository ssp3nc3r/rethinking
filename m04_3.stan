// rethinking::m04_3
data {
  int<lower=1> N;
  vector[N] height;
  vector[N] weight;
}
parameters {
  real alpha;
  real beta;
  real<lower=0,upper=50> sigma;
}
model {
  vector[N] mu = alpha + beta * weight;
  target += normal_lpdf(height | mu, sigma);
  target += normal_lpdf(alpha | 178, 20);
  target += normal_lpdf(beta | 0, 10);
}
