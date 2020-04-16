data{
  // Predictors
  int<lower=0> N;
  int<lower=0> N_test;
  int<lower=0> N_preds;
  matrix[N, N_preds] X_train;
  matrix[N_test, N_preds] X_test;
  // K
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  real<lower=0> delay_later[N,T];
  real<lower=0> amount_later[N,T];
  real<lower=0> delay_sooner[N,T];
  real<lower=0> amount_sooner[N,T];
  int<lower=-1,upper=1> choice[N, T]; // 0 for instant reward, 1 for delayed reward
  // test-set
  int<lower=1, upper=T> Tsubj_test[N_test];
  real<lower=0> delay_later_test[N_test,T];
  real<lower=0> amount_later_test[N_test,T];
  real<lower=0> delay_sooner_test[N_test,T];
  real<lower=0> amount_sooner_test[N_test,T];
  int<lower=-1,upper=1> choice_test[N_test, T];
}
parameters {
  // Regression
  vector[2] beta_k;
  vector[2] beta_a;
  
  // K
  // Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters  
  real mu_p;  
  vector<lower=0>[3] sigma;
    
  // Subject-level raw parameters (for Matt trick)
  vector[N] k_pr;
  vector[N] a_pr;
  vector[N] c_pr;
}
transformed parameters {
  // Transform subject-level raw parameters 
  vector[N] k;
  vector[N] c;
  vector[N] a;
  vector[N] k_means;
  vector[N] a_means;
  
  k_means = beta_k[1] + X_train[:,1] * beta_k[2];
  a_means = beta_a[1] + X_train[:,2] * beta_a[2];
        
  for (i in 1:N) {
    c[i] = Phi_approx(mu_p + sigma[3] * c_pr[i]) * 5;
  }
  k = exp(k_means + sigma[1] * k_pr);
  a = exp(a_means + sigma[2] * a_pr);
}
model{
  // K
  mu_p  ~ normal(0, 1);
  sigma ~ normal(0, 0.2);
  k_pr  ~ normal(0, 1);
  a_pr  ~ normal(0, 1);
  c_pr  ~ normal(0, 1);
  
  // regression
  beta_k[1] ~ normal(0,20);
  beta_a[1] ~ normal(0,20);
  beta_k[2] ~ normal(0,1);
  beta_a[2] ~ normal(0,1);
    
  for (i in 1:N) {
    // Define values
    real ev_later;
    real ev_sooner;
      
    for (t in 1:(Tsubj[i])) {
      ev_later   = pow(amount_later[i,t], a[i])  / ( 1 + k[i] * delay_later[i,t] );
      ev_sooner  = pow(amount_sooner[i,t], a[i]) / ( 1 + k[i] * delay_sooner[i,t] );
      choice[i,t] ~ bernoulli_logit( c[i] * (ev_later - ev_sooner) );
    }
  }
}
generated quantities {
  vector[N] log_lik;
  real log_lik_test[N_test, T];
  vector[N_test] k_test;
  vector[N_test] a_test;
  real mu_c;
  
  log_lik = rep_vector(0, N);
  for (i in 1:N_test) {
    for (t in 1:T) {
      log_lik_test[i,t] = 0;
    }
  }
  
  k_test = exp(beta_k[1] + X_test[:,1] * beta_k[2]);
  a_test = exp(beta_a[1] + X_test[:,2] * beta_a[2]);
  mu_c = Phi_approx(mu_p) * 5;
  
  { // Local section
    for (i in 1:N) {
      // Define values
      real ev_later;
      real ev_sooner;
        
      for (t in 1:(Tsubj[i])) {
        ev_later   = pow(amount_later[i,t], a[i])  / ( 1 + k[i] * delay_later[i,t] );
        ev_sooner  = pow(amount_sooner[i,t], a[i]) / ( 1 + k[i] * delay_sooner[i,t] );
        log_lik[i] += bernoulli_logit_lpmf(choice[i,t] | c[i] * (ev_later - ev_sooner) );
      }
    }
    // for test set
    for (i in 1:N_test) {
      // Define values
      real ev_later;
      real ev_sooner;
        
      for (t in 1:(Tsubj_test[i])) {
        ev_later   = pow(amount_later_test[i,t], a_test[i])  / ( 1 + k_test[i] * delay_later_test[i,t] );
        ev_sooner  = pow(amount_sooner_test[i,t], a_test[i]) / ( 1 + k_test[i] * delay_sooner_test[i,t] );
        log_lik_test[i,t] = bernoulli_logit_lpmf(choice_test[i,t] | mu_c * (ev_later - ev_sooner) );
      }
    }
  }
}
