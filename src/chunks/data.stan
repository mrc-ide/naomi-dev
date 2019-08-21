  int N_area;

  vector[N_area] pop15pl_i;
  vector[N_area] pop15to49_i;
  real prev_ratio;  // ratio of age 15+ prevalence to age 15-49 prevalence


  // ART attendance area i -> location j
  int N_adj;
  int n_nb[N_area];
  int adj_i[N_adj];
  int adj_j[N_adj];
  int adj_idx_ij[N_adj];
  int adj_idx_ji[N_adj];


  // survey prevalence
  int Nobs_prev;               // Number of observations
  int idx_prev[Nobs_prev];     // region index
  vector[Nobs_prev] prev_est;
  vector[Nobs_prev] prev_se;
  vector[Nobs_prev] prev_x;
  vector[Nobs_prev] prev_n;
  int prev_use_binom;

  // survey ART coverage
  int Nobs_artcov;               // Number of observations
  int idx_artcov[Nobs_artcov];      // region index
  vector[Nobs_artcov] artcov_est;
  vector[Nobs_artcov] artcov_se;
  vector[Nobs_artcov] artcov_x;
  vector[Nobs_artcov] artcov_n;
  int artcov_use_binom; 

  // number on ART
  int Nobs_artnum;
  int idx_artnum[Nobs_artnum]; 
  vector[Nobs_artnum] art15pl_i;

  
  // routine ANC testing
  int Nobs_anc1_prev;
  int anc1_prev_idx[Nobs_anc1_prev];
  int anc1_prev_n[Nobs_anc1_prev];
  int anc1_prev_x[Nobs_anc1_prev];

  int Nobs_anc1_artcov;
  int anc1_artcov_idx[Nobs_anc1_artcov];
  int anc1_artcov_n[Nobs_anc1_artcov];
  int anc1_artcov_x[Nobs_anc1_artcov];

  // flags for model components
  int flag_prev;
  int flag_artnum;
  int flag_artcov;
  int flag_ancprev;
  int flag_ancartcov;
