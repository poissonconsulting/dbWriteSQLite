#' Export all spatial datasets in a database as geopackages.
#'
#' @param conn A [SQLiteConnection-class] to a database.
#' @param dir A string of the path to the directory to save the geopackages in.
#' @param overwrite A flag specifying whether to overwrite existing geopackages.
#' @return An invisible named vector of the file names and new file names saved.
#' @export
#' @details If more than one spatial column is present in a table,
#'  a separate geopackage will be exported for each, and the other spatial columns will be dropped.
rws_export_gpkg <- function(conn, dir = ".", overwrite = FALSE) {
  chk_sqlite_conn(conn)
  chk_string(dir)
  chk_flag(overwrite)
  
  meta <- suppressWarnings(try(rws_read_meta(conn = conn), silent = TRUE))
  if(inherits(meta, "try-error")) err("Database must have a valid metadata table.")
  
  init <- suppressWarnings(try(rws_read_init(conn = conn), silent = TRUE))
  if(inherits(meta, "try-error")) err("Database must have a valid init table.")
  
  meta$Geometry <- sapply(meta$MetaMeta, vld_crs,
                          USE.NAMES = FALSE)
  if(!any(meta$Geometry)) err("No geometries detected.")
  
  exports <- meta[meta$Geometry == TRUE, ]
  
  if(nrow(init) > 0) {
    init <- init[!is.na(init$SFInit), ]
    init$GeomInit <- TRUE
    exports <- merge(exports, init, by.x = c("TableMeta", "ColumnMeta"),
                     by.y = c("TableInit", "SFInit"), all.x = TRUE)
    exports$GeomInit[is.na(exports$GeomInit)] <- FALSE
  } else {
    exports$GeomInit <- FALSE
  }
  
  exports <- exports[, c("TableMeta", "ColumnMeta", "GeomInit")]
  
  tbl_names <- rws_list_tables(conn = conn)
  
  if(!file.exists(dir)) dir.create(dir, recursive = TRUE)
  ui_line(glue::glue("Saving files to {ui_value(dir)}"))
  
  exported <- vector()
  
  for(i in 1:length(exports$ColumnMeta)) {
    col_name <- exports$ColumnMeta[i]
    tbl_name <- exports$TableMeta[i]
    
    table <- rws_read_table(tbl_name, conn = conn)
    table <- as.data.frame(table)
    
    for(col in names(table)){
      if(inherits(table[,col], "difftime")) table[,col] <- as.numeric(table[,col])
    }
    
    col_name <- names(table)[toupper(names(table)) == col_name]
    tbl_geom_names <- exports$ColumnMeta[exports$TableMeta == tbl_name]
    tbl_geom_names <- names(table)[toupper(names(table)) %in% tbl_geom_names]
    tbl_name <- tbl_names[toupper(tbl_names) == tbl_name]
    tbl_name <- ifelse(exports$GeomInit[i], tbl_name, paste0(tbl_name, "_", col_name))
    
    unwanted_cols <- tbl_geom_names[!tbl_geom_names == col_name]
    
    if(!length(unwanted_cols) == 0){
      table <- table[ , -which(names(table) %in% unwanted_cols)]
    }
    
    table <- activate_sfc(table, col_name)
    
    path <- paste0(file.path(dir, tbl_name), ".gpkg")
    
    if(file.exists(path) & !overwrite) err("File ", basename(path), " already exisits. Set 'overwrite' = TRUE to overwrite.")
    
    sf::st_write(table, path, delete_dsn = overwrite, quiet = TRUE)
    ui_line(glue::glue("Exported table {ui_value(tbl_name)} with spatial column {ui_value(col_name)} as {ui_value(basename(path))}"))
    exported[i] <- path
  }
  return(invisible(exported))
}


activate_sfc <- function (x, sfc_name) 
{
  chk::chk_string(sfc_name)
  if (!sfc_name %in% sfc_names(x)) 
    err("sfc_name must be an sfc column.")
  if (identical(sfc_name, active_sfc_name(x))) 
    return(x)
  if (is.sf(x)) {
    x <- sf::st_set_geometry(x, sfc_name)
  }
  else {
    x <- sf::st_sf(x, sf_column_name = sfc_name)
  }
  x
}

sfc_names <- function(x) {
  if (!is.data.frame(x)) return(character(0))
  
  sfc_names <- colnames(x)
  sfc_names <- sfc_names[vapply(x, is.sfc, TRUE)]
  sfc_names
}

active_sfc_name <- function(x) {
  if (!is.sf(x)) return(character(0))
  if (is.null(attr(x, "sf_column"))) return(character(0))
  
  attr(x, "sf_column")
}
