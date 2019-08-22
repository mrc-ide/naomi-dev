#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{

  DATA_MATRIX(X_rho);
  DATA_MATRIX(X_rho_anc);
  DATA_MATRIX(X_alpha);
  DATA_MATRIX(X_alpha_anc);
  DATA_MATRIX(Zu);

  DATA_MATRIX(Q);

  DATA_IVECTOR(idx_prev)
  DATA_VECTOR(l_prev);
  DATA_VECTOR(l_prev_se);

  DATA_IVECTOR(idx_artcov)
  DATA_VECTOR(l_artcov);
  DATA_VECTOR(l_artcov_se);

  DATA_IVECTOR(idx_prev_anc);
  DATA_VECTOR(n_prev_anc);
  DATA_VECTOR(x_prev_anc);

  DATA_IVECTOR(idx_artcov_anc)
  DATA_VECTOR(n_artcov_anc);
  DATA_VECTOR(x_artcov_anc);

  DATA_IVECTOR(idx_artnum)
  DATA_VECTOR(n_artnum);
  DATA_VECTOR(x_artnum);

  
  PARAMETER(log_sigma_rho);
  PARAMETER(logit_phi_rho);
  PARAMETER(log_sigma_alpha);
  PARAMETER(logit_phi_alpha);

  PARAMETER(log_sigma_rho_anc);
  PARAMETER(log_sigma_alpha_anc);


  PARAMETER_VECTOR(beta_rho);
  PARAMETER_VECTOR(beta_alpha);
  PARAMETER_VECTOR(beta_rho_anc);
  PARAMETER_VECTOR(beta_alpha_anc);
  
  PARAMETER_VECTOR(us_rho_raw);
  PARAMETER_VECTOR(ui_rho_raw);
  PARAMETER_VECTOR(us_alpha_raw);
  PARAMETER_VECTOR(ui_alpha_raw);

  PARAMETER_VECTOR(ui_rho_anc_raw);
  PARAMETER_VECTOR(ui_alpha_anc_raw);

  
  // initialize nll
  Type val(0);


  // hyperparameter priors
  val -= dnorm(exp(log_sigma_rho), Type(0.0), Type(2.5), true) + log_sigma_rho;
  val -= dnorm(exp(log_sigma_alpha), Type(0.0), Type(2.5), true) + log_sigma_alpha;
  val -= dnorm(exp(log_sigma_rho_anc), Type(0.0), Type(2.5), true) + log_sigma_rho_anc;
  val -= dnorm(exp(log_sigma_alpha_anc), Type(0.0), Type(2.5), true) + log_sigma_alpha_anc;


  // phi_rho ~ Beta(1/2, 1/2)
  // phi_alpha ~ Beta(1/2, 1/2)
  Type phi_rho(invlogit(logit_phi_rho));
  Type phi_alpha(invlogit(logit_phi_alpha));
  val -= log(phi_rho) +  log(1 - phi_rho);  // change of variables: logit_phi_rho -> phi_rho
  val -= log(phi_alpha) +  log(1 - phi_alpha);  // change of variables: logit_phi_alpha -> phi_alpha
  val -= dbeta(phi_rho, Type(0.5), Type(0.5), true);
  val -= dbeta(phi_alpha, Type(0.5), Type(0.5), true);

  
  // fixed effect priors: N(0.0, 5.0)
  val -= sum(dnorm(beta_rho, 0.0, 5.0, true));
  val -= sum(dnorm(beta_alpha, 0.0, 5.0, true));
  val -= sum(dnorm(beta_rho_anc, 0.0, 5.0, true));
  val -= sum(dnorm(beta_alpha_anc, 0.0, 5.0, true));

  
  // latent field model
  val -= Type(-0.5) * (us_rho_raw * (Q * us_rho_raw)).sum();
  val -= Type(-0.5) * (us_alpha_raw * (Q * us_alpha_raw)).sum();
  val -= dnorm(sum(us_rho_raw), Type(0.0), Type(0.001) * us_rho_raw.size(), true); // soft sum-to-zero constraint on us_rho
  val -= dnorm(sum(us_alpha_raw), Type(0.0), Type(0.001) * us_alpha_raw.size(), true); // soft sum-to-zero constraint on us_alpha

  val -= sum(dnorm(ui_rho_raw, 0.0, 1.0, true));
  val -= sum(dnorm(ui_alpha_raw, 0.0, 1.0, true));
  val -= sum(dnorm(ui_rho_anc_raw, 0.0, 1.0, true));
  val -= sum(dnorm(ui_alpha_anc_raw, 0.0, 1.0, true));


  // likelihood
  vector<Type> u_rho(exp(log_sigma_rho) * (sqrt(phi_rho) * us_rho_raw + sqrt(1 - phi_rho) * ui_rho_raw));
  vector<Type> u_alpha(exp(log_sigma_alpha) * (sqrt(phi_alpha) * us_alpha_raw + sqrt(1 - phi_alpha) * ui_alpha_raw));

  vector<Type> u_rho_anc(exp(log_sigma_rho_anc) * ui_rho_anc_raw);
  vector<Type> u_alpha_anc(exp(log_sigma_alpha_anc) * ui_alpha_anc_raw);

  vector<Type> mu_rho(X_rho * beta_rho + Zu * u_rho);
  vector<Type> mu_alpha(X_alpha * beta_alpha + Zu * u_alpha);

  vector<Type> mu_rho_anc(mu_rho + X_rho_anc * beta_rho_anc + Zu * u_rho_anc);
  vector<Type> mu_alpha_anc(mu_alpha + X_alpha_anc * beta_alpha_anc + Zu * u_alpha_anc);

  vector<Type> mu_prop_art(invlogit(mu_rho) * invlogit(mu_alpha));
  mu_prop_art = logit(mu_prop_art);

  for(int i = 0; i < idx_prev.size(); i++)
    val -= dnorm(l_prev[i], mu_rho[idx_prev[i]], l_prev_se[i], true);

  for(int i = 0; i < idx_artcov.size(); i++)
    val -= dnorm(l_artcov[i], mu_alpha[idx_artcov[i]], l_artcov_se[i], true);

  for(int i = 0; i < idx_prev_anc.size(); i++)
    val -= dbinom_robust(x_prev_anc[i], n_prev_anc[i],  mu_rho_anc[idx_prev_anc[i]], true);

  for(int i = 0; i < idx_artcov_anc.size(); i++)
    val -= dbinom_robust(x_artcov_anc[i], n_artcov_anc[i],  mu_alpha_anc[idx_artcov_anc[i]], true);

  for(int i = 0; i < idx_artnum.size(); i++)
    val -= dbinom_robust(x_artnum[i], n_artnum[i],  mu_prop_art[idx_artnum[i]], true);

  REPORT(mu_rho);
  REPORT(mu_alpha);
  
  ADREPORT(mu_rho);
  ADREPORT(mu_alpha);
  ADREPORT(mu_rho_anc);
  ADREPORT(mu_alpha_anc);
  ADREPORT(mu_prop_art);

  return val;
}
