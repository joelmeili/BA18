## ---- prepare_data.R
# @title: Source code for data preparation
# @author: joel.meili

# - load packages
library(dplyr)

# - prepare data
path <- "~/Dropbox/BA18/data/"
files <- c("DOW", "SP500", "10YUS_BOND_FUTURES", "JPY_USD", "GOLD")

data <- lapply(files, FUN = function(x){
  read.csv(paste0(path, x, ".csv"), header = T, stringsAsFactors = F)})

for(i in 1:length(data)){
  data[[i]][, "Date"] <- as.Date(data[[i]][, "Date"], format = "%m/%d/%Y")
  data[[i]] <- data[[i]][order(data[[i]][, "Date"]),]
  rownames(data[[i]]) <- NULL
  data[[i]][, files[i]] <- c(0, diff(log(data[[i]][, "Last.Price"])))
  data[[i]] <- data[[i]][-1,]
  data[[i]] <- data[[i]][, c("Date", files[i])]
}

assets <- data[[1]]

for(i in 2:length(files)){assets <- assets %>% inner_join(data[[i]], by = "Date")}

save(assets, file = paste0(path, "log.return.assets.Rdata"))
