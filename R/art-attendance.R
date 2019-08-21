#' @param M an adjacency matrix defining ART attendance locations
#' 
#' @import dplyr
create_attend_ij <- function(M){

  adj_ij <- Matrix::Matrix(M, sparse = TRUE) %>%
    as("dgCMatrix") %>%
    Matrix::summary() %>%
    mutate(x = NULL,
           istar = as.integer(i == j),
           jstar = as.integer(i == j)) %>%
    arrange(i, istar, j, jstar) %>%
    mutate(idx_ij = row_number())

  ## create index for contiguous q_ji
  adj_ij <- adj_ij %>%
    inner_join(
      adj_ij %>%
      arrange(j, jstar, i) %>%
      transmute(idx_ij, idx_ji = row_number()),
      by = "idx_ij"
    )

  adj_ij
}

#' @import dplyr
create_attend_d_ij <- function(adj_ij){

  ## d/dB_j' indices
  d_Bjp <- adj_ij %>%
    transmute(i, j, istar, idx_ij, idx1 = idx_ij) %>%
    mutate(ip = i) %>%
    inner_join(
      adj_ij %>%
      transmute(ip = i, jp = j, idx_ipjp = idx_ij),
      by = "ip"
    ) %>%
    inner_join(
      adj_ij %>%
      filter(jstar == 1) %>%
      transmute(jp = j, jstar, idx2 = idx_ij),
      by = "jp"
    )

  ## d/dq_ij' indices
  d_qijp <- adj_ij %>%
    transmute(i, j, istar, idx_ij, idx1 = idx_ij) %>%
    mutate(ip = i) %>%
    inner_join(
      adj_ij %>%
      filter(jstar != 1) %>%
      transmute(ip = i, jp = j, jstar, idx_ipjp = idx_ij, idx2 = idx_ij),
      by = "ip"
    )

  ## d/dq_i'j' indices
  d_qipjp <- adj_ij %>%
    transmute(i, j, istar, idx_ij, idx1 = idx_ij) %>%
    inner_join(
      adj_ij %>%
      filter(jstar == 1) %>%
      transmute(i, jp = j),
      by = "i"
    ) %>%
    inner_join(
      adj_ij %>%
      filter(jstar != 1) %>%
      transmute(ip = i, jp = j, jstar, idx_ipjp = idx_ij, idx2 = idx_ij),
      by = "jp"
    )
  
   d_adj_ij <- bind_rows(
    d_Bjp,
    d_qijp,
    d_qipjp
  )

  d_adj_ij
}
