################################################################################
# Wide to long for many measures with standardized variable names
#
# This requires the variable names to be fairly predictable, and has been
# difficult to generalize.  Good exercise nonetheless.
################################################################################

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)


############################################################
#  Create example data (by doing the split-gather in reverse).
############################################################
numobs     <- 50
numreps    <- 10
my_example <- data.frame(study_id = 1:numobs, 
                         tx_group = rep(c("Tx","Ctrl"), numobs/2),
                         age = runif(numobs, 1, 10) %>% floor) 

create_visit_dat <- function(x) {
  rtn <- data.frame(v1 = rep("a", numobs),
                    v2 = rep("b", numobs),
                    v3 = rep("c", numobs),
                    v4 = rep("d", numobs),
                    v5 = rep("e", numobs)) 
  colnames(rtn) <- paste0(c("visit_date_", "first_measure_", 
                            "second_measure_", "third_measure_",
                            "fourth_measure_"), x)
  return(rtn)
}

my_example <- purrr::map(1:numreps, ~ create_visit_dat(.x)) %>%
  purrr::reduce(cbind) %>% 
  cbind(my_example, .) %>% tbl_df

str(my_example, list.len = 10)
# Here is our very wide dataset
my_example





############################################################
# Programatically gather
############################################################

# The split and gather function does the bulk of the work.
split_gather <- function(.data, prefix) {
  rtn <- 
    .data %>% 
      select(study_id, tx_group, starts_with(prefix)) %>% 
      gather(key = "visit_num", value = prefix, -study_id, -tx_group)  %>%
      mutate(visit_num = visit_num %>%
                         str_sub(start = str_locate(., "[0-9]+"), 
                                 end   = nchar(.)) %>%
                         as.numeric)

  colnames(rtn)[4] <- prefix
  return(rtn)
}


# Split off non-time-varying data (often baseline data)
my_base <- my_example %>% select(study_id:age)

# Split off longitudinal data, apply wide-to-long and combine (map and reduce)
my_long <- 
  my_example %>% 
    dplyr::select(-c(study_id:age)) %>%
    names %>% 
    stringr::str_sub(start = 1, 
                     end   = stringr::str_locate(., "_[0-9]") %>% .[,1]-1) %>%
    #str_replace_all("_diameter", "") %>% #---- Used this to standardize names
    unique %>%
    purrr::map(~ split_gather(.data = my_example, prefix = .x)) %>%
    purrr::reduce(dplyr::full_join) 

# Combine longitudinal data with the non-time-varying data for final long
# dataset
my_long <- dplyr::full_join(my_base, my_long)

my_long


# Then do any final variable cleaning required...



