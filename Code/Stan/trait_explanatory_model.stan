data{
  // Predictors
  int<lower=0> N_subjects;
  int<lower=0> N_preds;
  matrix[N_subjects, N_preds] X;
  // K
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  real<lower=0> delay_later[N,T];
  real<lower=0> amount_later[N,T];
  real<lower=0> delay_sooner[N,T];
  real<lower=0> amount_sooner[N,T];
  int<lower=-1,upper=1> choice[N, T]; // 0 for instant reward, 1 for delayed reward
}
parameters {
  // Regression
  vector[2] beta_k;
  vector[2] beta_a;
  
  // K
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector<lower=0>[2] sigma;
    
  // Subject-level raw parameters (for Matt trick)
  vector[N] k_pr;
  vector[N] a_pr;
}
transformed parameters {
  // Transform subject-level raw parameters 
  vector[N] k;
  vector[N] a;
  vector[N] k_means;
  vector[N] a_means;
  
  k_means = beta_k[1] + X[:,1] * beta_k[2];
  a_means = beta_a[1] + X[:,2] * beta_a[2];
        

  k = exp(k_means + sigma[1] * k_pr);
  a = exp(a_means + sigma[2] * a_pr);
}
model{
  // K
  sigma ~ normal(0, 0.2);
  k_pr  ~ normal(0, 1);
  a_pr  ~ normal(0, 1);
  
  // regression
  beta_k ~ normal(0,1);
  beta_a ~ normal(0,1);
    
  for (i in 1:N) {
    // Define values
    real ev_later;
    real ev_sooner;
      
    for (t in 1:(Tsubj[i])) {
      ev_later   = pow(amount_later[i,t], a[i])  / ( 1 + k[i] * delay_later[i,t] );
      ev_sooner  = pow(amount_sooner[i,t], a[i]) / ( 1 + k[i] * delay_sooner[i,t] );
      choice[i,t] ~ bernoulli_logit( (ev_later - ev_sooner) );
    }
  }
}
generated quantities {
  vector[N] log_lik;
  
  log_lik = rep_vector(0, N);
  
  { // Local section
    for (i in 1:N) {
      // Define values
      real ev_later;
      real ev_sooner;
        
      for (t in 1:(Tsubj[i])) {
        ev_later   = pow(amount_later[i,t], a[i])  / ( 1 + k[i] * delay_later[i,t] );
        ev_sooner  = pow(amount_sooner[i,t], a[i]) / ( 1 + k[i] * delay_sooner[i,t] );
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf(choice[i,t] | (ev_later - ev_sooner) );
      }
    }
  }
}
