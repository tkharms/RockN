### Rock-derived nitrogen in Alaska ###
## Tamara Harms 2/2024
## Munge rock type, %N/Niso, stream N data

library(here)
library(tidyverse)
library(googledrive)
library(readxl)

### Import data from Drive ###
## Downloading all files in data subdirectory
Nurl <- "https://drive.google.com/drive/folders/1SCasDShyzXuFZdvJMC_7pBCqaCgLjxF0"

dat1 <- drive_get(as_id(Nurl))

# download all excel files
N_glist <- drive_ls(dat1, type = "xlsx")

setwd(here("data"))
walk(N_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

# download all csv files
C_glist <- drive_ls(dat1, type = "csv")
walk(C_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

setwd(here())

### Rock type data
## Detailed characterizations from visual assessment of specimens and field notes from state geologist
chars <- read_excel(here("data", "Rock Classification schema.xlsx"))

# replace "N/A" with NA
chars[chars == "N/A"] <- NA
  
# remove the N data- these will be merged in a later step to ensure completeness
chars <- chars %>% select(-c(dN15, N_mgkg))

### Rock N data 
nit <- read_excel(here("data", "rockN_comp.xlsx"))

### Look-up table linking state geo layer classes to broader classifications 
cats <- read_excel(here("data", "stategeo_class_lookup.xlsx"))

### Specimen attributes from state database
state <- read_excel(here("data", "Harms_UAF_sampled_220526.xlsx"))

### Read in stream chem from drive (this not in drive yet)