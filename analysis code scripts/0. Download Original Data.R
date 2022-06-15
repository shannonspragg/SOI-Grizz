
# Download Data -----------------------------------------------------------
### Here we dowload all of our "original" data

# Load Packages -------------------------------------------------------

library(googledrive)


### NEED TO UPDATE THESE -- REORGANIZE ORIGINAL INTO FOLDERS, ADD IN EACH FOLDER AS A GOOGLE DRIVE DOWNLOAD

# Load our Data with GoogleDrive: -----------------------------------------
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# AG Census
folder_url <- "https://drive.google.com/drive/u/0/folders/11eFcCnbaZEK8sh2BpzTQbCrVh8D1AkpH" # ag data
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/AG_CENSUS/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# CAN PAs:
folder_url <- "https://drive.google.com/drive/folders/1pvMatdKYeUdGAxME5y3zO-jfit1T8aC3" # Canada PAs gdb
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/CPCAD-BDCAPC_Dec2020.gdb/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# CCS REGIONS:
folder_url <- "https://drive.google.com/drive/u/0/folders/1QzJukP_1LPPmIMf71HslGSjFBqLFfoi7" # ccs regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/CCS_REGIONS/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# Census Metro:
folder_url <- "https://drive.google.com/drive/u/0/folders/1btdenHTXQyMaBXpqcS9MjM02FG8bQHdQ" # metro regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/CENSUS_METRO_AREAS/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# ECOPROVINCE:
folder_url <- "https://drive.google.com/drive/u/0/folders/1gxijJ00ogtk7tvOex7OIiL_ntXh8QNNx" # ECOPROV regions
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/ERC_ECOPROVINCE/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))


# Grizz Pop Units:
folder_url <- "https://drive.google.com/drive/u/0/folders/168DOx8cRU_0qZU50TbNVFv8brOqtWbtV" # GRIZZ POPS 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/GBPU_BC/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# GHM HUMAN MOD:
folder_url <- "https://drive.google.com/drive/u/0/folders/1GuG_CKY_rRpWu4wJEI2g22S9ZlNRemVe" # GHM 
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/GHM_HUMAN_MOD/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# GPW POP DENS:
folder_url <- "https://drive.google.com/drive/u/0/folders/1sXkXK1La49EUxC-_3KF0Jo4f3F9r0NYj" #  POP DENS
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/GPW_POP_DENS/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# RASTERS:
folder_url <- "https://drive.google.com/drive/u/0/folders/1ztty8pRVJXE6dik3p3rOourddt6EhTXQ" # RASTERS
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/RASTERS/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

# WARP:
folder_url <- "https://drive.google.com/drive/u/0/folders/1t-Fu-Ua_RzBmydG_EXovsYeRKF5LMSTh" # WARP
folder <- drive_get(as_id(folder_url))
gdrive_files <- drive_ls(folder)
#have to treat the gdb as a folder and download it into a gdb directory in order to deal with the fact that gdb is multiple, linked files
lapply(gdrive_files$id, function(x) drive_download(as_id(x),
                                                   path = paste0(here::here("Data/original/WARP_DATA/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

