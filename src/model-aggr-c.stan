functions {
#include /chunks/functions.stan
#include /chunks/functions_q-to-pi.stan
}
data {
#include /chunks/data.stan
#include /chunks/data_q-to-pi-jac.stan
}
transformed data {
#include /chunks/transformed-data.stan
}
parameters {

#include /chunks/param-common.stan
  
  vector<lower=0>[N_area] Atilde_j;  // number receiving ART in area j
  vector<lower=0, upper=1>[N_area] prop_untreated_i; // proportion HIV+ & untreated among (1 - prop_art_i);

  // ART allocation
  vector[sum(n_nb)] q_raw;

}
transformed parameters{

  vector[N_area] rho_i;
  vector[N_area] l_rho_i;
  vector[N_area] alpha_i;
  vector[N_area] l_alpha_i;

  vector[N_area] A_i;
  vector[N_adj] pi_art;
  vector[N_adj] q_art;
  vector<lower = 0, upper = 1>[N_area] prop_art_i;
  vector[N_area] sd_Atilde_j;

  {
    vector[N_area] p_art_i = rho_i .* alpha_i;
    vector[N_adj] p_art_ij;
    vector[N_adj] A_ij;
    vector[N_adj] var_A_ij;
    int cum_nb = 0;

    for(i in 1:N_area){
      q_art[(cum_nb+i):(cum_nb+i+n_nb[i])] = (n_nb[i] > 0) ? simplex_constrain(segment(q_raw, cum_nb+1, n_nb[i])) : rep_vector(1.0, 1);
      cum_nb = cum_nb + n_nb[i];
    }

    // reorder q_art to be sorted {ij}
    q_art = q_art[adj_idx_ji];

    A_ij = q_art .* Atilde_j[adj_j];

    cum_nb = 0;
    for(i in 1:N_area){
      A_i[i] = sum(segment(A_ij, cum_nb+i, n_nb[i]+1));
      cum_nb = cum_nb + n_nb[i];
    }

    prop_art_i = A_i ./ pop15pl_i;
    pi_art = A_ij ./ A_i[adj_i];
	
    p_art_ij = prop_art_i[adj_i] .* pi_art;
    var_A_ij = A_ij .* (1.0 - p_art_ij);
        
    cum_nb = 0;
    for(i in 1:N_area){
      sd_Atilde_j[i] = sqrt(sum(var_A_ij[segment(adj_idx_ji, cum_nb+i, n_nb[i]+1)]));
      cum_nb = cum_nb + n_nb[i];
    }

  }

  rho_i = prop_art_i + (1 - prop_art_i) .* prop_untreated_i; // <lower=prop_art_i, upper=1>
  l_rho_i = logit(rho_i);
  alpha_i = prop_art_i ./ rho_i;
  l_alpha_i = logit(alpha_i);

  
}
model {

  // change of variables

  // target += -log(pop15pl_i);             // A_i -> prop_art_i (commented since pop15pl_i constant)
  target += log1m(prop_art_i) - log(rho_i); // {prop_untreated, prop_art} -> {rho, alpha}
  target += -log(rho_i) - log1m(rho_i);     // rho -> l_rho
  target += -log(alpha_i) - log1m(alpha_i); // alpha -> l_alpha

  {
    // q_raw -> q_art
    int cum_nb = 0;
    for(i in 1:N_area){
      if(n_nb[i] > 0)
	target += simplex_constrain_lj(segment(q_raw, cum_nb+1, n_nb[i]));
      cum_nb = cum_nb + n_nb[i];
    }
  }

  if(flag_logdet)  // {Atilde_j, q_art} -> {Ai, pi_art}
    target += dense_log_determinant(N_adj, d_adj_idx1, d_adj_idx2,
                   q_to_pi_sparse_jac(A_i, Atilde_j, pi_art, q_art,
				      d_adj_i, d_adj_j,
				      d_adj_istar, d_adj_idx_ij,
				      d_adj_ip, d_adj_jp,
				      d_adj_jstar, d_adj_idx_ipjp));
  else if(flag_sparse_logdet)
    target += sparse_log_determinant(N_adj, d_adj_idx1, d_adj_idx2,
                   q_to_pi_sparse_jac(A_i, Atilde_j, pi_art, q_art,
				      d_adj_i, d_adj_j,
				      d_adj_istar, d_adj_idx_ij,
				      d_adj_ip, d_adj_jp,
				      d_adj_jstar, d_adj_idx_ipjp));
#include /chunks/priors.stan
#include /chunks/likelihood.stan
#include /chunks/likelihood-artnum.stan
}
generated quantities {
#include /chunks/generated-quantities.stan
}
