library(data.table)
all_speeches <- rbindlist(readRDS(file.path(getwd(), "Data", "speeches_by_lp.rds")))
fwrite(all_speeches, file.path(getwd(), "Data", "all_speeches.csv"))

