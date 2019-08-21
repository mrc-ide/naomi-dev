functions {
#include /chunks/functions.stan
}
data {
#include /chunks/data.stan
}
transformed data {
#include /chunks/transformed-data.stan
}
parameters {

#include /chunks/param-common.stan
  
  // area-level prevalence and ART coverage
  vector[N_area] l_rho_i;
  vector[N_area] l_alpha_i;

  // ART allocation
  vector[sum(n_nb)] pi_raw;
}
transformed parameters{

  vector[N_area] rho_i = inv_logit(l_rho_i);
  vector[N_area] alpha_i = inv_logit(l_alpha_i);
  vector[N_area] prop_art_i = rho_i .* alpha_i;

  vector[N_adj] pi_art;
  vector[N_adj] q_art;
  vector[N_area] Atilde_j;
  vector[N_area] sd_Atilde_j;

  {
    vector[N_adj] p_art_ij;
    vector[N_adj] A_ij;
    vector[N_adj] var_A_ij;
    int cum_nb = 0;
    for(i in 1:N_area){
      pi_art[(cum_nb+i):(cum_nb+i+n_nb[i])] = (n_nb[i] > 0) ? simplex_constrain(segment(pi_raw, cum_nb+1, n_nb[i])) : rep_vector(1.0, 1);
      cum_nb = cum_nb + n_nb[i];
    }

    p_art_ij = prop_art_i[adj_i] .* pi_art;
    A_ij = pop15pl_i[adj_i] .* p_art_ij;
    var_A_ij = A_ij .* (1.0 - p_art_ij);
        
    cum_nb = 0;
    for(i in 1:N_area){
      vector[n_nb[i]+1] art_i = A_ij[segment(adj_idx_ji, cum_nb+i, n_nb[i]+1)];
      Atilde_j[i] = sum(art_i);
      sd_Atilde_j[i] = sqrt(sum(var_A_ij[segment(adj_idx_ji, cum_nb+i, n_nb[i]+1)]));
      q_art[segment(adj_idx_ji, cum_nb+i, n_nb[i]+1)] = art_i / Atilde_j[i];
      cum_nb = cum_nb + n_nb[i];
    }

  }

}
model {

  // change of variables
  {
    // pi_raw -> pi_art
    int cum_nb = 0;
    for(i in 1:N_area){
      if(n_nb[i] > 0)
	target += simplex_constrain_lj(segment(pi_raw, cum_nb+1, n_nb[i]));
      cum_nb = cum_nb + n_nb[i];
    }
  }

#include /chunks/priors.stan
#include /chunks/likelihood.stan
#include /chunks/likelihood-artnum.stan
}
generated quantities {
#include /chunks/generated-quantities.stan
}

