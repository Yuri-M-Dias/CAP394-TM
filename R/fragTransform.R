library(data.table)
library(lubridate)
library(tictoc)
library(stringr)
library(tidyverse)

root.dir = "./"
source(paste0(root.dir, "R/reading_telemetry.R"))

telemetries = quick.read.tm(root.dir)

#telemetries.num = telemetries[,
  #lapply(telemetries, is.numeric) == TRUE, with = FALSE
#]

tic()

telemetries.num.mt = telemetries[,-c('date')] %>%
  # Date is currently too big for this code!
  #mutate_if(is.POSIXct, as.numeric) %>%
  mutate_if(is.factor, as.integer) %>%
  mutate_all(abs) %>%
  # This is deep wizardry, I'm sorry.
  mutate_all(~ if_else(
    . %% 1 > 0,
    . - . %% 1 + as.double(str_match(as.character(.), "\\d*$")[1]),
    as.double(.))
  ) %>%
  mutate_all(as.double) %>%
  mutate_all(~ if_else(. < 1 , . + 1, .)) %>%
  as.data.table

cardinalities = telemetries.num.mt[,lapply(.SD, max)]
names(telemetries.num.mt) = as.vector(sapply(cardinalities, as.character))

fwrite(
  telemetries.num.mt,
  file = "./Data/tms-frag.csv",
  sep = " "
)

toc()
