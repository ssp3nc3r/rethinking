data {
  int<lower=1> N;
  real div_obs[N];
  real div_sd[N];
  vector[N] R;
  vector[N] A;
}
parameters {
  real<lower=0> sigma;
  vector[N] div_est;
  real bA;
  real bR;
  real a;
  real mu;
}
model {
  //vector[N] mu = a + A * bA + R * bR;
  target += normal_lpdf(div_est | mu, sigma);
  target += normal_lpdf(div_obs | div_est, div_sd);
  
  // priors
  target += normal_lpdf(a|0, 10);
  target += normal_lpdf(bA|0, 10);
  target += normal_lpdf(bR|0, 10);
  target += cauchy_lpdf(sigma|0, 2.5);
}
