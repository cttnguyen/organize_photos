library(dplyr)
library(purrr)

source("dropbox_fns.R")
source("utils.R")

setwd("D:/Pictures/Wedding")

is_crystal <- (Sys.info()[["sysname"]] == "Windows")

# Google Doc --------------------------------------------------------------

df <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/18TgkvTtewZ_zpM8zozI6houUswISIeDjgU6HLyOws78/edit#gid=0",
  range = "Sheet1!A:I"
)

if(isTRUE(is_crystal)){
  df <- mutate(
    df,
    #Use Crystal computer local instead of Daniel's
    filename = stringr::str_extract(filename, paste0("(?<=", Category, "\\/).*")),
    #Don't want numbers
    Category =  stringr::str_remove_all(Category, "^[0-9]{2} ") |> 
      forcats::fct_inorder()
  )
}

# Local Setup -------------------------------------------------------------

# Create directory for each album collection if not already exists
df |> 
  select(-Category, -filename) |> 
  names() |> 
  walk(
    \(dirname){
      if(!dir.exists(dirname)){
        dir.create(dirname)
      }
    }
  )

# Check what files are already available
if(isTRUE(is_crystal)){
  local <- read_local("Originals")
} else {
  local <- unique(df$Category) |> 
    map_dfr(~ read_local(.x, full.names = TRUE))
}
# Add photo category
local <-  df |> 
  select(Category, filename) |> 
  right_join(local, by = "filename")

# cross check file availablity across googlesheet list and local
check_file_match(google = df, local = local)

# Download from Dropbox and Delete---------------------------------------------------
if(isTRUE(is_crystal)){
  # rdrop2::drop_auth(new_user = TRUE)
  rdrop2::drop_auth()
  
  # for all directories in luckett wedding + tea ceremony - samantha o_neal, 
  # get all sub-directory paths 
  # then get all file paths from sub-directories
  photo_paths <- get_album_paths() |> 
    map_dfr(get_photo_paths) |> 
    mutate(Category = forcats::fct(Category, levels = levels(df$Category)))
  
  # for all file paths, attempt to download to local Originals/ directory
  photo_paths |> 
    anti_join(local, by = c("Category", "filename")) |> 
    pwalk(
      .f = download_photo, .progress = "download"
    )
  
  # cross check file availability across googlesheet list, dropbox, and local
  check_file_match(google = df, dropbox = photo_paths, local = local) |> 
    count(Category, google, dropbox, local)
  
  # delete_from_dropbox(photo_paths)
}


# Sync Google Sheet to Local Drives --------------------------------------------------------------

#long df of selected photos only
df_long <- df |> 
  tidyr::pivot_longer(
    cols = c(-Category, -filename)
  ) |> 
  filter(value == TRUE)


df_long |> 
  group_by(name) |> #For each album/drive
  group_walk(
    \(x, ...){
      add_files( #add all files in filename col to folder
        fnames = x$filename,
        source = if_else(isTRUE(is_crystal), "Originals", NA_character_),
        target = x$name[1]
      )
      # remove_files( #remove all files NOT in filename col from folder
      #   fnames = x$filename,
      #   dirname = x$name[1]
      # )
    } ,
    .keep = TRUE
  )

# cross check file availability with googlesheet
df_long |> 
  group_by(name) |> 
  group_map(
    ~ check_file_match(
      google = .x,
      local = read_local(.x$name[1]) |> 
        left_join(select(.x, Category, filename), by = "filename")
    ) |> 
      mutate(target = .x$name[1]),
    .keep = TRUE
  ) |> 
  bind_rows() 

