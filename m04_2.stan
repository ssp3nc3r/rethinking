// rethinking::m04_1
data {
  int<lower=1> N;
  vector[N] height;
}
parameters {
  real mu;
  real<lower=0,upper=50> sigma;
}
model {
  target += normal_lpdf(height | mu, sigma);
  target += normal_lpdf(mu | 178, 0.1);
}
