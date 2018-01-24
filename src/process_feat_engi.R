library(dplyr)
library(onehot)

get_mean_dist <- function(df, to_drop = NA){
  id <- df$id %>% 
    unique()
  
  mean_dist <- df %>% 
    as_tibble() %>% 
    select(-one_of(to_drop)) %>%
    dist(diag = T, upper = T) %>%
    mean()
  
  df_res <- data.frame(id, mean_dist)
  return(df_res)
}

get_vol <- function(a, b, c, alpha, beta, gamma){
  alpha_r <- pi * (alpha/ 180)
  beta_r <- pi * (beta/ 180)
  gamma_r <- pi * (gamma/ 180)
  return(a * b * c * sqrt(1 + 2*cos(alpha_r)*cos(beta_r)*cos(gamma_r) - cos(alpha_r)**2 - cos(beta_r)**2 - cos(gamma_r)**2))
}

add_elem_properties <- function(df){
  data("periodicTable", envir = environment())
  
  periodicTable <- periodicTable %>% 
    select(symb, mass, Eneg, rcov, rvdw, rion, IP, density, isotopes, C) %>%
    rename(atom = symb)
  
  df_res <- df %>% 
    left_join(periodicTable)
  
  return(df_res)
}

feat_engi <- function(input, coord){
  coord_df <- rbindlist(coord)
  
  spatial_info_by_id_atom <- coord_df %>%
    group_by(id, atom) %>%
    summarise(mean_x = mean(x), sd_x = sd(x), var_x = var(x),
              mean_y = mean(y), sd_y = sd(y), var_y = var(y),
              mean_z = mean(z), sd_z = sd(z), var_z = var(z),
              cov_xy = cov(x,y), cov_yz = cov(y,z), cov_xz = cov(x,z),
              n = n()) %>% 
    gather(key, val, mean_x:n) %>%
    unite(key2, atom, key, sep = "_") %>%
    spread(key2, val)
  
  spatial_info_by_id_atom[is.na(spatial_info_by_id_atom)] <- 0
  
  spatial_info_by_id <- coord_df %>%
    add_elem_properties() %>% 
    group_by(id) %>%
    summarise(mean_mass = mean(mass), mean_eneg = mean(Eneg),
              mean_rcov = mean(rcov), mean_rvdw = mean(rvdw), mean_rion = mean(rion),
              mean_ip = mean(IP), mean_density = mean(density), mean_isotopes = mean(isotopes), mean_c = mean(C))
  
  data <- input %>% 
    left_join(spatial_info_by_id_atom) %>% 
    left_join(spatial_info_by_id) %>% 
    mutate(vol = get_vol(lattice_vector_1_ang, lattice_vector_2_ang, lattice_vector_3_ang, 
                         lattice_angle_alpha_degree, lattice_angle_beta_degree, lattice_angle_gamma_degree),
           mass_vol = mean_mass / vol) %>% 
    unite(spacegroup_nb_atom, spacegroup, number_of_total_atoms, sep = "_") %>% 
    mutate(spacegroup_nb_atom = as.factor(spacegroup_nb_atom))
  # %>% 
  #   mutate(spacegroup_fact = as.factor(spacegroup))
  
  # to_onehot <- onehot(data)
  # data_onehot <- predict(to_onehot, data)
  # data <- data.frame(data_onehot)
  
  # onehot_train <- onehot(data)
  # data <- data.frame(predict(onehot_train, data))
  # dist <- pblapply(coord, FUN = function(x) get_mean_dist(x, to_drop = "atom")) %>% 
  #   rbindlist()
  
  # data <- data %>% 
  #   left_join(dist)

  return(data)
}
