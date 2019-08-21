
    vector simplex_constrain(vector y) {

    vector[rows(y)+1] x;
    int Km1;
    real stick_len;

    Km1 = rows(y);
    stick_len = 1.0;
    for (k in 1:Km1) {
      real z_k;
      z_k = inv_logit(y[k] - log(Km1 - k + 1));
      x[k] = stick_len * z_k;
      stick_len = stick_len - x[k];
    }
    x[Km1+1] = stick_len;

    return x;
  }

  real simplex_constrain_lj(vector y) {

    real lj;
    int Km1;
    real stick_len;

    lj = 0.0;
    Km1 = rows(y);
    stick_len = 1.0;
    for (k in 1:Km1) {
      real adj_y_k;
      adj_y_k = y[k] - log(Km1 - k + 1);
      lj = lj + log(stick_len) - log1p_exp(-adj_y_k) - log1p_exp(adj_y_k);
      stick_len = stick_len * (1.0 - inv_logit(adj_y_k));
    }

    return lj;
  }

