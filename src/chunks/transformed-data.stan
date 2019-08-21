
  vector[Nobs_prev] l_prev_est = logit(prev_est);
  vector[Nobs_prev] l_prev_se = prev_se ./ (prev_est .* (1 - prev_est));
  vector[Nobs_artcov] l_artcov_est = logit(artcov_est);
  vector[Nobs_artcov] l_artcov_se = artcov_se ./ (artcov_est .* (1 - artcov_est));

  vector[N_adj] art_attend_prior_alpha;

  for(idx in 1:N_adj){
    int i;
    int j;
    i = adj_i[idx];
    j = adj_j[idx];

    // alpha = 19 if same district and 0.5/[num neighbors] implies Dirichlet prior with
    // mean 0.94 in same region, 80% mass > 0.9
    if(i == j)
      art_attend_prior_alpha[idx] = 19;
    else
      art_attend_prior_alpha[idx] =  1.0/n_nb[i];
  }

