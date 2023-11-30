get_album_paths <- function(path = "/Luckett Wedding + Tea Ceremony - Samantha O_Neal"){
  rdrop2::drop_dir() |> 
    filter(path_display == path) |> 
    pull(path_display) |>  #or path_lower?
    rdrop2::drop_dir() |> 
    pull(path_display) #or path_lower?
}

get_photo_paths <- function(path){
  rdrop2::drop_dir(path, recursive = TRUE) |> 
    pull(path_display) |> 
    tibble::as_tibble_col("dropname") |> 
    mutate(filename = stringr::str_remove_all(dropname, "^\\/[A-z \\+\\-]*\\/")) |> 
    filter(stringr::str_detect(filename, "\\/")) |>
    tidyr::separate(filename, c("Category", "filename"), "\\/")
}

download_photo <- function(dropname, Category, filename){
  local_path <- paste0("Originals/", filename)
  if(file.exists(local_path)){
    warning(sprintf("%s from %s already exists. File not downloaded.", local_path, Category))
  } else {
    rdrop2::drop_download(
      path = dropname,
      local_path = local_path,
      overwrite = FALSE,
      progress = FALSE,
      verbose = FALSE
    )
  }
}

delete_from_dropbox_h <- function(dropname, Category, filename){
  local_path <- paste0("Originals/", filename)
  if(file.exists(local_path)){
    rdrop2::drop_delete(dropname)
  } else{
    warning(
      sprintf("%s from %s does not exist locally. File not deleted from dropbox.", local_path, Category)
    )
  }
}

delete_from_dropbox <- function(photo_paths){
  perform_deletion <- readline(sprintf("Would you like to delete %s files from dropbox? (Y/N) ", nrow(photo_paths)))
  if(perform_deletion %in% c("Y", "y")){
    pwalk(
      .l = photo_paths, .f = delete_from_dropbox_h, .progress = "delete"
    )
  } else if(perform_deletion %in% c("N", "n")){
    message("No files deleted")
  } else {
    message("Invalid selection, cancelling action.")
  }
}