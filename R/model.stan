data {
  int<lower=0> N; // n total observations
  int<lower=0> M; // m individuals
  vector[N] d; // daily vehicle kilometers travelled
  int<lower=1, upper=M> ind[N]; // individuals
}

parameters {
  vector<lower=0>[M] p1;
  vector<lower=0>[M] p2;
  real<lower=0> k_shape;
  real<lower=0> lambda_shape;
  real<lower=0> k_scale;
  real<lower=0> lambda_scale;
}

transformed parameters {
  vector<lower=1>[M] k = 1 + p1;
  vector<lower=0>[M] lambda;
  for (m in 1:M) {
     lambda[m] = p2[m] / tgamma(1 + 1 / k[m]);
  }
}

model {
  for (n in 1:N) {
    d[n] ~ weibull(k[ind[n]], lambda[ind[n]]);
  }
  p1 ~ weibull(k_shape, lambda_shape);
  p2 ~ weibull(k_scale, lambda_scale);

  lambda_shape ~ normal(0, 5);
  k_scale ~ normal(1, 5);
  lambda_scale ~ normal(100, 50);
}

