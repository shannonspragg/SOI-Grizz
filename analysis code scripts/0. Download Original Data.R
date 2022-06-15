
# Download Data -----------------------------------------------------------
### Here we download all of our "original" data

# Load Packages -------------------------------------------------------
library(googledrive)


# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# AG Census
folder_url <- "https://drive.google.com/drive/u/0/folders/1gAEffqQmJ9eDCyMRxwlJz-N1kuFAh2db" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# CAN PAs:
folder_url <- "https://drive.google.com/drive/u/0/folders/1hvJDRs63oBfQQVFAXvkwNf3ktE-2m7VG" # Canada PAs gdb
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/CPCAD-BDCAPC_Dec2020.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# CCS REGIONS:
folder_url <- "https://drive.google.com/drive/u/0/folders/17gAdIv6M2-Nd5-2-tnYWyp5cTukVVfoJ" # ccs regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Census Metro:
folder_url <- "https://drive.google.com/drive/u/0/folders/1SarVo_1LEWmyhJ1kQ_MWxTt4rcwCX5oW" # metro regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# ECOPROVINCE:
folder_url <- "https://drive.google.com/drive/u/0/folders/1PX0LYabRczon1XN_7XEa65Dud2KT7Il4" # ECOPROV regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Grizz Pop Units:
folder_url <- "https://drive.google.com/drive/u/0/folders/1k0i78cWFQiJCtN7-BBoKHslOIZ-jcivV" # GRIZZ POPS 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# GHM HUMAN MOD:
folder_url <- "https://drive.google.com/drive/u/0/folders/10AdYmzFuYJRSovR3-t1fKHgy52gsFBKk" # GHM 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# GPW POP DENS:
folder_url <- "https://drive.google.com/drive/u/0/folders/15jeet_73C0LzpAEuukdaBBVVwe7gYlPZ" #  POP DENS
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# RASTERS:
folder_url <- "https://drive.google.com/drive/u/0/folders/1DtQ6xJBhEHH12iTtbszteatPcId11w0B" # RASTERS
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# WARP:
folder_url <- "https://drive.google.com/drive/u/0/folders/1POAqlR6vyXwAq-sSP8Wi3q87gDFFJ0Qf" # WARP
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

