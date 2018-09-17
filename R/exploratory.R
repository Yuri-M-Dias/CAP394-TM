library("dplyr")
# Constants
sat_DATA_FILE <- "./Data/sat2_2018-03.csv"
CSV_SEPARATOR <- ","

# Reads data
headerNames <- scan(
  file = sat_DATA_FILE,
  nlines = 1,
  what = character(),
  sep = CSV_SEPARATOR
)
headerDescription <- scan(
  file = sat_DATA_FILE,
  skip = 1,
  nlines = 1,
  what = character(),
  sep = CSV_SEPARATOR
)
headerUnits <- scan(
  file = sat_DATA_FILE,
  skip = 2,
  nlines = 1,
  what = character(),
  sep = CSV_SEPARATOR
)
sat2 <- read.csv(
  file = sat_DATA_FILE,
  skip = 3,
  header = FALSE,
  sep = CSV_SEPARATOR
)
names(sat2) <- paste0(headerNames)

#Data cleaning

sat2$`Data-Hora` <-
  as.POSIXct(strptime(sat2$`Data-Hora`, "%d-%b-%Y %H:%M:%OS"))
# Prints with fractional seconds
op <- options(digits.secs = 3)

sat2 <- dplyr::rename(sat2, date = 'Data-Hora')

onePassage <- sat2[0:100,]

attach(onePassage)
par(mfrow = c(1, 2))

plot(
  x = 0,
  y = 0,
  xlim = c(min(date), max(date)),
  ylim = c(min(TM001), max(TM001)),
  type = 'n'
)
points(date,
       TM001,
       lwd = 1,
       pch = 6,
       col = 1)
points(date,
       TM001,
       type = 'l',
       lwd = 1,
       col = 4)

plot(
  x = 0,
  y = 0,
  xlim = c(min(date), max(date)),
  ylim = c(min(TM004), max(TM004)),
  type = 'n'
)
points(date,
       TM004,
       lwd = 1,
       pch = 6,
       col = 1)
points(date,
       TM004,
       type = 'l',
       lwd = 1,
       col = 4)

#detach(sat2)
