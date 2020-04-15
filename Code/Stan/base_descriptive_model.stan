data{
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
  // K
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  vector[2] mu_p;  
  vector<lower=0>[2] sigma;
    
  // Subject-level raw parameters (for Matt trick)
  vector[N] k_pr;
  vector[N] c_pr;
}
transformed parameters {
  // Transform subject-level raw parameters 
  vector[N] k;
  vector[N] c;
  
  for (i in 1:N) {
    c[i] = Phi_approx(mu_p[2] + sigma[2] * c_pr[i]) * 5;
  }
  k = exp(mu_p[1] + sigma[1] * k_pr);
}
model{
  // K
  mu_p[1] ~ normal(0, 20);
  mu_p[2] ~ normal(0, 1);
  sigma   ~ normal(0, 0.2);
  k_pr    ~ normal(0, 1);
  c_pr    ~ normal(0, 1);
    
  for (i in 1:N) {
    // Define values
    real ev_later;
    real ev_sooner;
      
    for (t in 1:(Tsubj[i])) {
      ev_later   = amount_later[i,t]  / ( 1 + k[i] * delay_later[i,t] );
      ev_sooner  = amount_sooner[i,t] / ( 1 + k[i] * delay_sooner[i,t] );
      choice[i,t] ~ bernoulli_logit( c[i] * (ev_later - ev_sooner) );
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
        ev_later   = amount_later[i,t]  / ( 1 + k[i] * delay_later[i,t] );
        ev_sooner  = amount_sooner[i,t] / ( 1 + k[i] * delay_sooner[i,t] );
        log_lik[i] = log_lik[i] + bernoulli_logit_lpmf(choice[i,t] | c[i] * (ev_later - ev_sooner));
      }
    } 
  }
}
