################################################################################
#                                                                              #
# Fibox Respiration                                                         #
#                                                                              #
# Written by: Mario Muscarella                                                 #
#                                                                              #
# Last update: 28 Aug 2019                                                   #
#                                                                              #
################################################################################
#                                                                              #
# Notes:                  #
#                                                                              #
# Issues:                                                                      #
#                                                                              #
# Recent Changes:                                                              #
#                                                                              #
# Future Changes (To-Do List):                                                 #
#         1.                                                                   #
#                                                                              #
################################################################################

setwd("~/GitHub/fibox-respiration/")

# Required packages
list.of.packages <- c("chron")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = T))

# General functions
CV <- function(x, ...){(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100}

# Function to import raw data
read.fibox <- function(file = "", skip = "", fileEncoding="latin1"){
  data.in <- read.delim(file, header=T, skip=57, sep=";",strip.white=T,
                        stringsAsFactors=FALSE, fileEncoding="latin1")
  colnames(data.in) <- c("date", "time", "logtime", "oxygen", "phase", 
                         "amp", "temp", "error", "other")
  data.in$date <- chron(dates. = data.in$date, format = c("d/m/y"))
  data.in$time <- chron(times. = data.in$time, format = c("h:m:s"))
  return(data.in)
}

# Temp Space
data.in <- read.fibox(file = file)
  
summary.fibox <- function(data.in = ""){
  CV.O2 <- CV(data.in$oxygen)
  if(CV.O2 < 5){
    mean.O2 <- mean(data.in$oxygen)
    mean.temp <- mean(data.in$temp)
  } else {
    mod1 <- lm(data.in$oxygen ~ data.in$logtime)
    cooksd <- cooks.distance(mod1)
    CV.O2 <- CV(data.in$oxygen[which(cooksd < 0.1)])
    if(CV.02 < 5){
      mean.O2 <- mean(data.in$oxygen[which(cooksd < 0.1)])
      mean.temp <- mean(data.in$temp[which(cooksd < 0.1)])
    } else {
      stop("Error in data")
    }
  }
  read.date <- median(data.in$date)
  read.time <- median(data.in$time)
  read.temp <- mean.temp
  read.O2 <- mean.O2
  output <- data.frame("date" = read.date, "time" = read.time, 
                       "O2" = read.O2, "temp" = read.temp)
  return(output)
}
  
# Temp Space
data.in <- read.fibox(file = "./data/T3.txt")
summary.fibox(data.in)

dir <- "./data/"
files <- list.files(dir)
test.run <- as.data.frame(matrix(NA, nrow = length(files), ncol = 4))
colnames(test.run) <- c("date", "time", "O2", "temp")
str(test.run$date) <- chron(format = c("d/m/y"))
str(test.run$time) <- chron(format = c("h:m:s"))

data.in$date <- chron(dates. = data.in$date, format = c("d/m/y"))
data.in$time <- chron(times. = data.in$time, format = c("h:m:s"))

for(i in 1:length(files)){
  input.file <- paste(dir, files[i], sep = "", collapse = "")
  temp <- read.fibox(file = input.file)
  test.run[i, ] <- summary.fibox(temp)
}

# Cote-Nord
# Lakes
dir.1 <- "./data/Cote-Nord 2013/Lakes/"
lakes <- list.files(dir.1)

