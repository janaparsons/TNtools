##this code creates an internal dataset with CBSA codes and names for use in TN_tracts

x <- dplyr::distinct(totalcensus::dict_cbsa[,1:2])
usethis::use_data(x, internal = TRUE)
