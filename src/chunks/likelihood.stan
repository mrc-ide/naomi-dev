  // survey prevalence likelihood
  if(prev_use_binom)
    target += prev_x .* l_rho_i[idx_prev] - prev_n .* log1p_exp(l_rho_i[idx_prev]); // prev_x ~ binomial_logit(prev_n, l_rho_i)
  else
    l_prev_est ~ normal(l_rho_i[idx_prev], l_prev_se);


  // ANC prevalence data likelihood
  if(flag_ancprev) {
    vector[N_area] l_rho_anc_i = l_rho_i + l_rho_ancbias0 + sigma_rho_ancbias * l_rho_ancbias_raw;
    anc1_prev_x ~ binomial_logit(anc1_prev_n, l_rho_anc_i[anc1_prev_idx]);
  }

  if(flag_artnum) {
    // vector[N_area] l_alpha_i = logit(alpha_i);

    // survey ART coverage likelihood
    if(flag_artcov){
      if(artcov_use_binom)
	target += artcov_x .* l_alpha_i[idx_artcov] - artcov_n .* log1p_exp(l_alpha_i[idx_artcov]); // artcov_x ~ binomial_logit(artcov_n, l_rho_i)
      else
	l_artcov_est ~ normal(l_alpha_i[idx_artcov], l_artcov_se);
    }

    // ANC ART coverage data likelihood
    if(flag_ancartcov) {
      vector[N_area] l_alpha_anc_i = l_alpha_i + l_alpha_ancbias0 + sigma_alpha_ancbias * l_alpha_ancbias_raw;
      anc1_artcov_x ~ binomial_logit(anc1_artcov_n, l_alpha_anc_i[anc1_artcov_idx]);
    }
  }
