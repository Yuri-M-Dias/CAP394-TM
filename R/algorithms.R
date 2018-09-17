library(data.table)
library(lubridate)
library(tictoc)
library(tidyverse)

root.dir = "./"
source(paste0(root.dir, "R/reading_telemetry.R"))

telemetry.fst.path = build.file.path(
  root.dir = root.dir,
  file.name = '2018-01-01--2018-08-08-SAT2-TH.fst'
)
telemetries <- read.telemetry.fst(telemetry.fst.path, columns = c('date'))

get.sequences.counter = function(telemetries) {
  counter = 1
  counter.next = counter + 1
  passage.num = 1
  passage.counter = 1
  telemetries.len = dim(telemetries)[1]
  passages.num = vector()
  passages.counters = vector()
  while (counter <= telemetries.len) {
    frame.current = telemetries[counter]
    frame.next = telemetries[counter.next]
    if (is.na(frame.next$date)) {
      frame.next = frame.current
    }
    difference = difftime(frame.next$date, frame.current$date, units = 'sec')
    passage.counter = passage.counter + 1
    passages.counters[counter] = passage.counter
    passages.num[counter] =  passage.num
    # 10s difference?!
    if (difference > 10) {
      passage.counter = 1
      passage.num = passage.num + 1
    }
    counter = counter.next
    counter.next = counter + 1
    if (counter %% 10000 == 0) {
      message(sprintf("c-%i_s-%i", counter, passage.num))
    }
  }
  result = list()
  result$num = passages.num
  result$sequences = passages.counters
  result
}

tic()
results = get.sequences.counter(telemetries)
sequences = results$sequences
sequences.nums = results$num
toc()
summary(sequences)
summary(sequences.nums)
tic()
if (length(sequences) == dim(telemetries)[1]) {
  message(sprintf("Can merge! Merging!"))
  # Reads it all
  telemetries <- read.telemetry.fst(telemetry.fst.path)
  telemetries[
    , `:=`(sequenceCounter = sequences, passageNum = sequences.nums)
  ]
  save.telemetry.fst(data = telemetries, file.path = telemetry.fst.path)
}
toc()
