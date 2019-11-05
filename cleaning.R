#
#
#
# Data Cleaning and Wrangling
# ------------------------------------------------------
setwd("~/Documents/School/VT/DataCompFall19")

library(tidyverse)


# General Hospital Data
general <- read_csv("data/Hospital_Revised_FlatFiles/Hospital\ General\ Information.csv")
# Complications/Deaths Data
deaths.hospital <- read_csv("data/Hospital_Revised_FlatFiles/Complications and Deaths - Hospital.csv")
HAI.hospitals <- read_csv("data/Hospital_Revised_FlatFiles/Healthcare Associated Infections - Hospital.csv")


check_measure_sizes <- function(general_size, dataset) {
  unique.values <- length(unique(dataset$`Measure ID`))
  if (nrow(dataset) / unique.values != general_size) {
    stop('The number of entries in the dataset does not match the general size')
  }
  return( TRUE )
}


num.of.hospitals <- nrow(general)
# Verify that this matches the size of general
# This should be done for each dataset to join in
# --- unique measures for deaths/complications
check_measure_sizes(num.of.hospitals, deaths.hospital)
# --- for HAI data
check_measure_sizes(num.of.hospitals, HAI.hospitals)








