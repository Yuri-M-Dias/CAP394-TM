# Reads the telemetry data and returns a proper data.table.
# Not exactly Tidy, yet.

library(data.table)
library(lubridate)
library(fst)
library(parallel)
library(tictoc)
library(tidyverse)

ROOT_FOLDER = './'
DATA_DIR = 'Data/'

TELEMETRY_FST_FILE = 'sat_2018-03.fst'
TELEMETRY_DESC_FILE = 'telemetryDescription.csv'
TELEMETRY_DATA_FILE = '01-01-2018--08-08-2018-SAT2.csv'

CSV_SEPARATOR = ','
DECIMAL_SEPARATOR = '.'
STD_ENCODING = 'UTF-8'

#load("./tmcs.RData")
#TM_CLASSES = tmcs # From the file above!

read.header = function(file.path,
                       file.separator,
                       num.lines = 1) {
  headerNames = scan(
    file = file.path,
    nlines = num.lines,
    # Defaults to string
    what = character(),
    sep = file.separator
  )
  return(headerNames)
}

read.formated.telemetries = function(file.path,
                                     nrows = Inf,
                                     skip = 3,
                                     file.separator = CSV_SEPARATOR) {
  # First row as header names
  headerNames = read.header(file.path, file.separator)
  # Ignores last row, missing data
  input.path = file.path
  telemetries = fread(
    input = input.path,
    header = FALSE,
    verbose = TRUE, # For debug
    skip = skip, # First three are: header, info and more info
    encoding = STD_ENCODING,
    dec = DECIMAL_SEPARATOR,
    nrows = nrows,
    nThread = detectCores(),
    stringsAsFactors = TRUE,
    check.names = TRUE,
    col.names = headerNames,
    #colClasses = TM_CLASSES, # Necessary for some reason
    sep = file.separator
  )
  #telemetries = telemetries[,.SD[-1:-2]] # Drops first two rows (desc and type)
  #message(str(telemetries))
  # Converts to numeric
  tmNames = names(TM_CLASSES)
  # Have to convert to char first, else it coerces the factor index itself
  telemetries[
    ,
    (tmNames) := lapply(.SD, rlang::as_function(~ as.numeric(as.character(.)))),
    .SDcols = tmNames
  ]
  # Converts date column to proper date-time
  telemetries[,`Data-Hora` := dmy_hms(`Data-Hora`)] %>%
    setnames("Data-Hora", "date")
  # Prints with fractional seconds
  op = options(digits.secs = 3)

  return(telemetries)
}

read.telemetries.description = function(file.path,
                                        file.separator = CSV_SEPARATOR) {
  telemetries <- fread(
    input =  file.path,
    header = TRUE,
    #verbose = TRUE, # For debug
    encoding = STD_ENCODING,
    dec = DECIMAL_SEPARATOR,
    nThread = detectCores(),
    stringsAsFactors = FALSE,
    check.names = TRUE,
    sep = file.separator
  )
  return(telemetries)
}

save.telemetry.fst = function(data,
                              file.path,
                              compressionLevel = 100) {
  return(
    write.fst(
      data, path = file.path, compress = compressionLevel
    )
  )
}

read.telemetry.fst = function(file.path,
                              as.data.table = TRUE,
                              columns = NULL,
                              from = 1,
                              to = NULL) {
  return(
    read.fst(
      file.path,
      as.data.table = as.data.table,
      columns = columns,
      from = from,
      to = to
    )
  )
}

build.file.path = function(root.dir = ROOT_FOLDER,
                           data.dir = DATA_DIR,
                           file.name) {
  data.dir = paste0(root.dir, data.dir)
  file.path = paste0(data.dir, file.name)
  return(file.path)
}

quick.read.tm = function(root.dir) {
  telemetry.fst.path = build.file.path(
    root.dir = root.dir,
    file.name = TELEMETRY_FST_FILE
  )
  return(
    read.telemetry.fst(telemetry.fst.path)
  )
}

quick.read.tm.desc = function(root.dir) {
  telemetry.desc.path = build.file.path(
    root.dir = root.dir,
    file.name = TELEMETRY_DESC_FILE
  )
  return(
    read.telemetries.description(telemetry.desc.path)
  )
}

get.file.size = function(bigfile.path)
{
    # 1L would be the date field, slighly too slow....
    nrow(fread(bigfile.path, select = 2L))
}

telemetry.big.file.reader = function(bigfile.path)
{
    size = get.file.size(bigfile.path)
    i = 3 # Due to the headers
    step = 200000
    while (i < size) {
        tmp.tms = read.formated.telemetries(
            file.path = bigfile.path,
            nrows = step,
            skip = i
        )
        inmsize = format(object.size(tmp.tms), units = "auto")
        message(sprintf("Read from line %i, for a size of: %s", i, inmsize))
        bigfile.new.name = paste0(
          './BigOutput/',
          date(tmp.tms[1]$date),
          '--',
          date(tmp.tms[.N]$date),
          '-SAT2.fst'
        )
        tic()
        save.telemetry.fst(tmp.tms, file.path = bigfile.new.name)
        toc()
        message(sprintf("Saved file: %s", bigfile.new.name))
        i = i + step
    }
}

concatenate.fsts = function(targetDir)
{
    tic()
    fsts = list.files(path = targetDir, pattern = '*fst$')
    endResult = NULL
    for (fileName in fsts) {
        target = paste0(targetDir, fileName)
        tmp = read.telemetry.fst(target)
        message(sprintf("Read %s", fileName))
        if (is.null(endResult)) {endResult = tmp; next;}
        else endResult = merge(endResult, tmp, all = TRUE)
        gc()
    }
    bigfile.new.name = paste0(
      './BigOutput/',
      date(endResult[1]$date),
      '--',
      date(endResult[.N]$date),
      '-SAT2.fst'
    )
    save.telemetry.fst(endResult, file.path = bigfile.new.name)
    message(sprintf("Wrote %s", bigfile.new.name))
    toc()

}

# This is a test disguised as a function
check.telemetry.saving.results = function() {
  sat2 = read.formated.telemetries(
    file.path = TELEMETRY_DATA_FILE,
    file.separator = CSV_SEPARATOR
  )
  save.telemetry.fst(data = sat2, file.path = TELEMETRY_FST_PATH)
  sat22 = read.telemetry.fst(TELEMETRY_FST_PATH)
  if (all.equal(sat2, sat22)) {
    print("Nice result!")
  } else {
    stop("Not so good result!")
  }
  return(TRUE)
}
