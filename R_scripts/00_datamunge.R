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

# Correct spelling
chars <- chars %>% mutate(type_b = ifelse(type_b == "gniess", "gneiss", type_b))

# Reduce deformation categories
chars <- chars %>% mutate(deform = ifelse(deformation =="min", "low", 
                                      ifelse(deformation == "moderate/high", "moderate-high", deformation)))

### Rock N data 
nit <- read_excel(here("data", "rockN_comp.xlsx"))

### Look-up table linking state geo layer classes to broader classifications 
cats <- read_excel(here("data", "stategeo_class_lookup.xlsx"))

# replace "N/A" with NA
cats[cats == "N/A"] <- NA

### Specimen attributes from state database
state <- read_excel(here("data", "Harms_UAF_sampled_220526.xlsx"))

### Read in stream chem from drive ###
stream <- read.csv(here("data", "chemspace.csv"))

### Join N to rock characterization ###
names(chars)[names(chars) == 'sample_id'] <- 'assigned_sample_label'

Nchars <- full_join(nit, chars, by = "assigned_sample_label")

write.csv(Nchars, here("data", "rockNmeta.csv"), row.names = FALSE)

# Samples without characterization of specimens
nometa <- Nchars %>% filter(is.na(type)) %>%
                     filter(!grepl("ref/", ID))

write.csv(nometa, here("data", "samples_missing_rockchars.csv"), row.names = FALSE)
  
p <- drive_get(as_id(Nurl))
  
drive_upload(here("data", "samples_missing_rockchars.csv"), path=as_id(p),type = 'csv', name ='samples_missing_rockchars')
  