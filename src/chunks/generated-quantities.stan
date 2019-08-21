  real rho = dot_product(rho_i, pop15to49_i) / sum(pop15to49_i);
  real alpha = dot_product(alpha_i, rho_i .* pop15to49_i) /
    dot_product(rho_i, pop15to49_i);
