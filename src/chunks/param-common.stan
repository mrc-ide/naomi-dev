  // prevalence
  real l_rho0;
  real<lower=0> sigma_l_rho;

  real l_rho_ancbias0;
  real<lower=0> sigma_rho_ancbias;
  vector[N_area] l_rho_ancbias_raw;

  // ART coverage
  real l_alpha0;
  real<lower=0> sigma_l_alpha;

  real l_alpha_ancbias0;
  real<lower=0> sigma_alpha_ancbias;
  vector[N_area] l_alpha_ancbias_raw;
