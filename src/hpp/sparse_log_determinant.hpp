/**
 * Calculate log absolute determinant of sparse matrix in triplet format
 *
 * @param dim number of rows and columns of matrix.
 * @param i row index of non-zero elements.
 * @param j column index of non-zero elements.
 * @param x values of non-zero elements.
 * @param pstream__ 
 * @return log absolute determinant.
 */
template <typename T3__>
typename boost::math::tools::promote_args<T3__>::type
sparse_log_determinant(const int& dim,
		       const std::vector<int>& i,
		       const std::vector<int>& j,
		       const Eigen::Matrix<T3__, Eigen::Dynamic,1>& x,
		       std::ostream* pstream__) {

  typedef Eigen::Triplet<T3__> T;
  std::vector<T> tripletList;
  tripletList.reserve(i.size());
  for(int idx = 0; idx < i.size(); idx++)
    tripletList.push_back(T(i[idx]-1, j[idx]-1, x[idx]));

  Eigen::SparseMatrix<T3__> Js(dim, dim);
  Js.setFromTriplets(tripletList.begin(), tripletList.end());

  const Eigen::SparseLU<Eigen::SparseMatrix<T3__>> lu(Js);
  if (lu.info() != Eigen::Success)
    throw std::runtime_error("Sparse LU decomposition error");
  return lu.logAbsDeterminant();
}
