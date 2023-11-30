read_local <- function(path = "Originals/", ...){
  list.files(path, ...) |> 
    tibble::as_tibble_col("filename")
}
check_file_match <- function(..., names = c("Category", "filename")){
  
  df_list <- list(...)
  df_names <- names(df_list)
  
  checkmate::assert_list(
    x = df_list,
    types = "data.frame",
    names = "unique"
  )
  walk(
    df_list,
    ~ checkmate::assert_subset(names, names(.x))
  )
  
  
  imap(
    df_list,
    ~ mutate(.x, !!sym(.y) := 1) |> 
      select(all_of(c(names, .y)))
  ) |> 
    reduce(
      \(x, y) full_join(x, y, by = names)
    ) |> 
    mutate(across(
      all_of(df_names),
      ~ tidyr::replace_na(.x, 0)
    )) |> 
    filter(if_any(
      all_of(df_names),
      ~ .x == 0
    ))
  
}

add_files <- function(fnames, source_dir = NA_character_, target_dir){
  
  checkmate::assert_directory_exists(target_dir)
  if(!is.na(source_dir)) checkmate::assert_directory_exists(source_dir)
  
  fnames <- discard(fnames, ~ file.exists(file.path(target_dir, .x)))
  # target_path <- file.path(target_dir, fnames)
  if(is_empty(fnames)){
    message(sprintf("All files already exist in %s.", target_dir))
  } else{
    if(is.na(source_dir)){ #Daniel
      source_path <- fnames
    } else { #Crystal
      source_path <- file.path(source_dir, fnames)
    }
    file.copy(
      from = source_path,
      to = target_dir,
      overwrite = FALSE,
      copy.date = TRUE
    )
    message(sprintf("%s files copied to %s", length(fnames), target_dir))
  }
  
}

remove_files <- function(fnames, dirname){
  
  checkmate::assert_directory_exists(dirname)
  
  dir_fnames <- list.files(dirname) #all files in the directory
  remove_fnames <- discard(dir_fnames, ~ .x %in% fnames) #all directory files NOT in fnames
  
  if(is_empty(remove_fnames)){
    message(sprintf("No files named fnames in %s", dirname))
  } else {
    file.remove(remove_fnames)
    message(sprintf("%s files removed from %s", length(remove_fnames), dirname))    
  }
  
  
  
}













