data {
  int N;
  vector[N] height;
  vector[N] weight_s;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0,upper=50> sigma;
}
model {
  vector[N] mu = alpha + beta1 * weight_s + beta2 * (weight_s .* weight_s);
  target += normal_lpdf(height | mu, sigma);
  target += normal_lpdf(alpha | 140, 100);
  target += normal_lpdf(beta1 | 0, 10);
  target += normal_lpdf(beta2 | 0, 10);
  target += uniform_lpdf(sigma | 0, 50);
}
