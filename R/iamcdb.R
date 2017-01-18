# iamcdb.R define S3 methods to describes and manipulate the iamc db template file


#' @export
iamc.template <- function(filename, ...) {
  if(!file.exists(filename)){
    stop("iamc template error: cannot find the template")
  }
  structure(list(filename = filename, ...), class = "iamc.template")
}

#' @export
print.iamc.template <- function(x, ...) {
    cat("<iamc template: ", x$filename, ">\n", sep = "")
}

#' @export
load_var <- function(x, ...) {
    UseMethod("load_var", x)
}

#' @export
# load the variable definition from the template
load_var.iamc.template <- function(x, sheetvar = "variable definitions", colvar = NA, colunit = NA,
    firstrow = 2, convert2usd2010 = F) {

    # open template workbook
    wb <- openxlsx::loadWorkbook(file = x$filename)

    # find sheetvar (allow for mistyping)
    sheets = names(wb)
    sheetidx = agrep(sheetvar, sheets)

    # find useful columns
    header = openxlsx::read.xlsx(x$filename, sheetidx, rows = c(1, 1), colNames = F)
    if (is.na(colvar))
        colvar = agrep("Variable", header)
    if (length(colvar) == 0)
        stop("loadvar error: variable column in template has not been found, please provide colvar")
    if (length(colvar) > 1)
        stop("loadvar error: cannot detect the 'variable' column as header names are ambiguous, please provide colvar")
    if (is.na(colunit))
        colunit = agrep("Units", header)
    if (length(colunit) != 1)
        stop("loadvar error: unit column in template has not been found or header names are ambiguous, please provide colunit")

    # Load vars
    variables = openxlsx::read.xlsx(x$filename, sheet = sheetidx, startRow = firstrow, cols = colvar,
        colNames = F)

    units = openxlsx::read.xlsx(x$filename, sheetidx, startRow = firstrow, cols = colunit, colNames = F)
    xls.var = data.frame(var = as.character(variables[, 1]), unit = as.character(units[, 1]))

    # coeff
    xls.var$coeff=1
    if (convert2usd2010)
      xls.var[grep("US\\$",xls.var$unit),]$coeff = 1.10774

    return(xls.var)

}

#' @export
load_gdx <- function(x, ...) {
  UseMethod("load_gdx", x)
}

#' @export
load_gdx.iamc.template <- function(x, gdxfiles, nrep.keep, year.keep = NULL, scen_func = NULL) {

  if(!length(gdxfiles)>0){stop("load_gdx error: please provide at least one GDX filename")}

  if(is.null(scen_func)){
    scen_func <- function(.gdxfile){
      name = basename(.gdxfile)
      name = stringr::str_sub(name,stringr::str_length("db_")+1,-5)
      return(name)
    }
  }

  gdxlist = lapply(gdxfiles, gdxtools::gdx)

  # NREP is case-sensitive. It should have the same case as in the registration template
  #nrep.keep =c('USA', 'OLDEURO', 'NEWEURO', 'KOSAU', 'CAJAZ', 'TE', 'MENA', 'SSA',
  #             'SASIA', 'CHINA', 'EASIA', 'LACA', 'INDIA', 'World')
  if(is.null(year.keep)){
    year.keep = get_years(x)
    print(paste("Found template years:",paste(year.keep,collapse = ",")))
  }

  # load all gdx report variables
  varname <- function(v1,v2,v3){
    v1=ifelse(v1=='','',paste0(v1,'|'))
    v2=ifelse(v2=='','',paste0(v2,'|'))
    return(paste0(v1,v2,v3))
  }
  gdx.rep = data.table::rbindlist(lapply(gdxlist,gdxtools::extract,'db',addgdx=T))
  gdx.rep$var = tolower(varname(gdx.rep$V1,gdx.rep$V2,gdx.rep$V3))
  gdx.rep$value = as.numeric(gdx.rep$value)
  gdx.rep$year =as.numeric(gdx.rep$yr)
  gdx.rep$V1 = NULL
  gdx.rep$V2 = NULL
  gdx.rep$V3 = NULL
  gdx.rep$yr = NULL
  gdx.rep = subset(gdx.rep, tolower(gdx.rep$nrep) %in% tolower(nrep.keep))
  gdx.rep = subset(gdx.rep, gdx.rep$year %in% year.keep)

  # Identify scenario
  gdx.rep$scenario = scen_func(gdx.rep$gdx)

  # Ensure correct sensitivity case of region
  for(nk in nrep.keep){
    if(length(gdx.rep[tolower(gdx.rep$nrep)==tolower(nk),]$nrep)>0){
      gdx.rep[tolower(gdx.rep$nrep)==tolower(nk),]$nrep = nk
    } else {
      warning(paste("load_gdx error : ",nk,"does not exists in GDX."))
    }
  }

  return(gdx.rep)
}


#' @export
save_xls <- function(x, ...) {
    UseMethod("save_xls", x)
}

#' @export
#' @import data.table
save_xls.iamc.template <- function(x, .gdx.data, .iamc.vars, model_name, sheetdata = "data", addtimestamp = T, keepNA = F) {

    # ensure data.table
    .gdx.data = data.table(.gdx.data)
    .iamc.vars = data.table(.iamc.vars)

    # merge
    .iamc.vars$ivar = tolower(.iamc.vars$var)
    .gdx.data$ivar = tolower(.gdx.data$var)
    .gdx.data$var = NULL
    .data = merge(.iamc.vars,.gdx.data,by=c("ivar"),all.x=T)
    .iamc.vars$ivar = NULL
    .data$ivar = NULL

    # Update value with coeff
    .data$value = .data$value * .data$coeff
    .data$coeff = NULL

    # add model name
    .data$model = model_name

    # keep only useful column
    .data = .data[, list(model, scenario, nrep, var, unit, year, value)]

    # manage no data
    nodata = .data[is.na(year)]
    .data = .data[!is.na(year)]
    missing = expand.grid(year = unique(.data$year), nrep = unique(.data$nrep), scenario = unique(.data$scenario))
    nodata = nodata[, list(year = missing$year, scenario = missing$scenario, nrep = missing$nrep,
        value = NA), by = list(model, var, unit)]
    .data = rbind(.data, nodata)

    # spread the years
    tabdata = data.table::dcast(.data, model + scenario + nrep + var + unit ~ year, fun.aggregate = sum,
        value.var = "value")

    # Manage NA values
    if (!keepNA) {
        nbyears = length(unique(.data$year))
        tabdata = as.data.table(tabdata[rowSums(is.na(tabdata)) != nbyears,])
    }
    tabdata[is.na(tabdata)] <- "N/A"

    # find parts that not split scenario [to limit size file]
    maxrowfile = 15000
    tsize = tabdata[, .(nrow = nrow(.SD)), by = "scenario"]
    tsize[, `:=`(idx, as.integer(cumsum(nrow)/maxrowfile) + 1)]
    idxpart = merge(tabdata[, .(scenario)], tsize[, .(scenario, idx)], by = "scenario")$idx

    # open template workbook
    wb <- openxlsx::loadWorkbook(file = x$filename)

    # read header
    sheets = names(wb)
    sheetidx = agrep(sheetdata, sheets)
    header = openxlsx::read.xlsx(x$filename, sheetidx, rows = c(1, 1), colNames = F)

    # specify header
    names(tabdata) <- c("Model", "Scenario", "Region", "Variable", "Unit", paste(unique(.data$year)))

    for (i in 1:max(idxpart)) {

        # create workbook
        wb <- openxlsx::createWorkbook(creator = "gdx2iamcdb")

        # add data worksheet
        openxlsx::addWorksheet(wb, sheetdata)

        # write data
        openxlsx::writeData(wb, 1, tabdata[idxpart == i, ], startCol = 1, startRow = 1, rowNames = F,
            colNames = T, keepNA = T)

        # save into new excel file
        idname = paste0("_part", i)
        if (addtimestamp)
            idname = paste0(idname, format(Sys.time(), "_%y-%m-%d_%H-%M-%OS"))
        newname = paste0(tools::file_path_sans_ext(x$filename), idname, ".", tools::file_ext(x$filename))

        # Save Workbook
        openxlsx::saveWorkbook(wb, newname, overwrite = TRUE)

        print(newname)

    }

    return(list(iamc.missing = unique(.data[is.na(value)])))

}

#' @export
get_years <- function(x, ...) {
  UseMethod("get_years", x)
}

# Load the variable definition from the template
#' @export
get_years.iamc.template <- function(x, sheetdata = "data") {

  # open template workbook
  wb <- openxlsx::loadWorkbook(file = x$filename)

  # read header
  sheets = names(wb)
  sheetidx = agrep(sheetdata, sheets)
  header = openxlsx::read.xlsx(x$filename, sheetidx, rows = c(1, 1), colNames = F)

  # find years
  header = suppressWarnings(as.numeric(header))
  header = header[!is.na(header)]
  years = header[header > 1000]

  return(years)
}

