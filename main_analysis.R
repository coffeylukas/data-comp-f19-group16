#
#
# Data Comp 2019
# --------------------------
setwd("~/Documents/School/VT/DataCompFall19")

library(tidyverse)

# General Hospital Data
general <- read_csv("data/Hospital_Revised_FlatFiles/Hospital\ General\ Information.csv")

# By State death and complications
Complications_and_Deaths_State <- read_csv("data/Hospital_Revised_FlatFiles/Complications and Deaths - State.csv")