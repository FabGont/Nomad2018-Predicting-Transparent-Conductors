library(data.table)
library(reshape2)
library(stringr)
library(pbapply)
library(PeriodicTable)

read_coord <- function(path){
  path <- paste0(path, '/geometry.xyz')
  id <- as.numeric(str_extract(path, "\\d+"))

  spatial_info <- read.table(path, skip = 6) %>%
    dplyr::rename(x = V2, y = V3, z = V4, atom = V5) %>% 
    select(-V1) %>% 
    mutate(id = id)
  
  return(data.frame(spatial_info))
}

process_coord <- function(path_dir) {
  files <- list.dirs(path_dir, recursive = F)
  
  l <- pblapply(files, read_coord)
  
  return(l)
}