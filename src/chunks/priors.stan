  // priors

  l_rho0 ~ normal(-2, 5);
  sigma_l_rho ~ normal(0, 2.5);

  if(flag_prev)
    l_rho_i ~ normal(l_rho0, sigma_l_rho);

  l_rho_ancbias0 ~ normal(0, 5);
  sigma_rho_ancbias ~ normal(0, 2.5);
  l_rho_ancbias_raw ~ normal(0, 1);

  l_alpha0 ~ normal(0, 5);
  sigma_l_alpha ~ normal(0, 2.5);
  if(flag_artnum)
    logit(alpha_i) ~ normal(l_alpha0, sigma_l_alpha);

  l_alpha_ancbias0 ~ normal(0, 5);
  sigma_alpha_ancbias ~ normal(0, 2.5);
  l_alpha_ancbias_raw ~ normal(0, 1);

  // pi_art prior  
  {
    int cum_nb = 0;
    for(i in 1:N_area){
      segment(pi_art, cum_nb+i, n_nb[i]+1) ~ dirichlet(segment(art_attend_prior_alpha, cum_nb+i, n_nb[i]+1));
      cum_nb = cum_nb + n_nb[i];
    }
  }
