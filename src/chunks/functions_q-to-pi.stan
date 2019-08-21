
   vector q_to_pi_sparse_jac(vector Ai,
			     vector Bj,
			     vector pi_art,
			     vector q_art,
			     int[] i,
			     int[] j,
			     int[] istar,
			     int[] idx_ij,
			     int[] ip,
			     int[] jp,
			     int[] jstar,
			     int[] idx_ipjp) {

     vector[size(i)] val;
     for(ii in 1:size(i))
       val[ii] = (jstar[ii] == 1 ? q_art[idx_ipjp[ii]] : Bj[jp[ii]]) *
	 (istar[ii] == 1 ? -Ai[i[ii]] : (pi_art[idx_ij[ii]] - (j[ii] == jp[ii] ?  1 : 0))) *
	(i[ii] == ip[ii] ? -1 : 1) /
	Ai[i[ii]];

     return val;
  }


  real dense_log_determinant(int dim, int[] i, int[] j, vector x) {

    matrix[dim, dim] M = rep_matrix(0.0, dim, dim);
    for(ii in 1:size(i))
      M[i[ii], j[ii]] = x[ii];
    return log_determinant(M);
  }

  real sparse_log_determinant(int dim, int[] i, int[] j, vector x);
