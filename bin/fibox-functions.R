################################################################################
#                                                                              #
# Fibox Respiration                                                            #
#                                                                              #
# Written by: Mario Muscarella                                                 #
#                                                                              #
# Last update: 10 Dec 2019                                                      #
#                                                                              #
################################################################################
#                                                                              #
# Notes:                                                                       #
#                                                                              #
# Issues:                                                                      #
#                                                                              #
# Recent Changes:                                                              #
#                                                                              #
# Future Changes (To-Do List):                                                 #
#         1.                                                                   #
#                                                                              #
################################################################################

# Required packages
list.of.packages <- c("chron")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = T))

# General functions
CV <- function(x, ...){(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))*100}
se <- function(x, ...){(sd(x, na.rm = TRUE)/sqrt(length(na.omit(x))))}

# Function to import raw data
read.fibox <- function(file = "", skip = "", fileEncoding="latin1"){
  temp <- readLines(file,n=1)
  if(grepl(";", temp)){ sep = ";" } else {sep = "\t"}
  data.in <- read.delim(file, header=T, skip=57, sep=sep,strip.white=T,
                        stringsAsFactors=FALSE, fileEncoding="latin1")[, 1:8]
  colnames(data.in) <- c("date", "time", "logtime", "oxygen", "phase", 
                         "amp", "temp", "error")
  temp.date <- suppressWarnings(chron(dates. = data.in$date[1], format = c("d/m/y")))
  if(!is.na(temp.date)){
    data.in$date <- chron(dates. = data.in$date, format = c("d/m/y"))
  } else {
    data.in$date <- chron(dates. = data.in$date, format = c("m/d/y"))
  }
  if(lengths(regmatches(data.in$time[1], gregexpr(":", data.in$time[1]))) == 2){
    data.in$time <- chron(times. = data.in$time, format = c("h:m:s"))
  } else {
    time.temp <- gsub(":([0-9][0-9][0-9])$", ".\\1", data.in$time)
    data.in$time <- chron(times. = time.temp, format = c("h:m:s"))
  }
  bads <- which(as.numeric(data.in$oxygen) <= 0)
  if(length(bads > 0)){
    data.in <- data.in[-c(bads), ]
  }
  return(data.in)
}
  
summary.fibox <- function(data.in = ""){
  CV.O2 <- CV(data.in$oxygen)
  if(CV.O2 < 5){
    mean.O2 <- mean(data.in$oxygen)
    mean.temp <- mean(data.in$temp)
  } else {
    mod1 <- lm(data.in$oxygen ~ data.in$logtime)
    cooksd <- cooks.distance(mod1)
    CV.O2 <- CV(data.in$oxygen[which(cooksd < 0.2)])
    if(CV.O2 < 5){
      mean.O2 <- mean(data.in$oxygen[which(cooksd < 0.2)])
      mean.temp <- mean(data.in$temp[which(cooksd < 0.2)])
    } else {
      mean.O2 <- median(data.in$oxygen[which(cooksd < 0.2)])
      mean.temp <- median(data.in$temp[which(cooksd < 0.2)])
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

fibox.resp <- function(files = "", dir = "", set = ""){
  temp.resp <- as.data.frame(matrix(NA, nrow = length(files), ncol = 5))
  colnames(temp.resp) <- c("date", "time", "O2", "temp", "file")
  for(i in 1:length(files)){
    input.file <- paste(dir, set, files[i], sep = "/", collapse = "")
    temp <- read.fibox(file = input.file)
    
    temp.resp[i, c(1:4)] <- summary.fibox(temp)
    temp.resp[i, 5] <- files[i]
  }
  temp.resp$date <- as.character(format(as.Date(dates(temp.resp$date), 
                                                format = c("%Y/%m/%d")), "%Y/%m/%d"))
  time.length <- max(as.Date(temp.resp$date)) - min(as.Date(temp.resp$date))
  if(time.length > 8){
    temp.date <- suppressWarnings(as.character(format(as.Date(temp.resp$date, 
                                               format = c("%Y/%d/%m")), "%Y/%m/%d")))
  } else (temp.date <- temp.resp$date)
  if(sum(is.na(temp.date)) > 0){
    temp.date[-c(which(is.na(temp.date)))] <- as.character(format(as.Date(temp.resp$date[-c(which(is.na(temp.date)))], 
                                                                          format = c("%Y/%d/%m")), "%Y/%m/%d"))
    temp.date[c(which(is.na(temp.date)))] <- as.character(format(as.Date(temp.resp$date[c(which(is.na(temp.date)))], 
                                                       format = c("%Y/%m/%d")), "%Y/%m/%d"))
  }
  time.length.2 <- max(as.Date(temp.date, format = c("%Y/%m/%d"))) - 
                   min(as.Date(temp.date, format = c("%Y/%m/%d")))
  if(time.length.2 > time.length){
    temp.date <- as.character(format(as.Date(temp.resp$date, 
                                             format = c("%Y/%m/%d")), "%Y/%m/%d"))
  }
  if(time.length > 8){
    while(time.length > 8){
      elapsed <- as.Date(temp.date) - as.Date(temp.date[1])
      odd.date <- which(elapsed %in% c(setdiff(elapsed, c(0:4))))[1]
      temp.date[odd.date] <- as.character(format(as.Date(temp.date[odd.date],format = c("%Y/%d/%m")), "%Y/%m/%d"))
      elapsed.2 <- as.Date(temp.date) - as.Date(temp.date[1])
      if(elapsed.2[odd.date] %in% c(setdiff(elapsed.2, c(0:4)))){
        temp.date[odd.date - 1] <- as.character(format(as.Date(temp.date[odd.date - 1],format = c("%Y/%d/%m")), "%Y/%m/%d"))
        temp.date[odd.date] <- as.character(format(as.Date(temp.date[odd.date],format = c("%Y/%m/%d")), "%Y/%m/%d"))
      }
      time.length <- max(as.Date(temp.date), na.rm = T) - min(as.Date(temp.date), na.rm = T)
    }
  }
  temp.resp$date <- temp.date
  temp.resp$time <- times(temp.resp$time)
  date_time <- chron(dates. = temp.resp$date,
                     times. = temp.resp$time,
                     format = c("y/m/d","h:m:s"))
  temp.resp <- temp.resp[order(date_time), ]
  date_time.2 <- date_time[order(date_time)]
  temp.resp$elapsed <- NA
  for(i in 1:dim(temp.resp)[1]){
    temp.resp$elapsed[i] <- (date_time.2[i] - date_time.2[1]) * 24
  }
  output <- temp.resp[, c(1,2,6,3,4,5)]
  # if(all.equal(order(output$elapsed), order(output$file))){
  # } else {
  #   stop("File Order Incorrect; Check Times")
  #}
  return(output)
}

plot_resp <- function(resp.a = resp.a, resp.b = resp.b,
                      group = "", samp = ""){
  # Rep A
  timevals.a <- seq(0, ceiling(max(resp.a[[1]]$elapse)), 1)
  
  ## Linear Model
  mod.a.l <- resp.a[[2]]
  pred.a.l <- predict(mod.a.l, list(elapsed=timevals.a))
  
  ## Trimmed Model
  mod.a.t <- resp.a[[3]]
  pred.a.t <- predict(mod.a.t, list(elapsed=timevals.a))
  
  ## Exponential Model
  mod.a.e <- resp.a[[4]]
  pred.a.e <- exp(predict(mod.a.e, list(elapsed=timevals.a)))
  
  # Rep B
  timevals.b <- seq(0, ceiling(max(resp.b[[1]]$elapse)), 1)
  
  ## Linear Model
  mod.b.l <- resp.b[[2]]
  pred.b.l <- predict(mod.b.l, list(elapsed=timevals.b))
  
  ## Trimmed Model
  mod.b.t <- resp.b[[3]]
  pred.b.t <- predict(mod.b.t, list(elapsed=timevals.b))
  
  ## Exponential Model
  mod.b.e <- resp.b[[4]]
  pred.b.e <- exp(predict(mod.b.e, list(elapsed=timevals.b)))
  
  x.range <- c(0, round(max(c(resp.a[[1]]$elapsed, resp.b[[1]]$elapsed), 
                            na.rm = T) * 1.1, 0))
  y.range <- c(floor(min(c(resp.a[[1]]$O2, resp.b[[1]]$O2), na.rm = T) * 0.98),
               round(max(c(resp.a[[1]]$O2, resp.b[[1]]$O2), na.rm = T) * 1.12, 0))
  par(mar = c(5, 6, 3, 1))
  plot(1, 1, type = "n", 
       pch = 22, bg = "gray", 
       xlim = x.range, ylim = y.range,
       xlab = "", ylab = "", las = 1, axes = F,
       main = paste(group, ": ", samp, sep = ""))
  axis(side = 1, labels = T)
  axis(side = 2, labels = T, las = 1)
  mtext("Incubation Time (hrs.)", side = 1, line = 2.5, cex = 1.25)
  mtext("Dissolved Oxygen", side = 2, line = 4.25, cex = 1.25)
  mtext(expression(paste("(mg O"[2], " L"^"-1", ")")), 
        side = 2, line = 2.75, cex = 1)
  box(lwd = 1.5)
  
  pts.a <- rep("gray", dim(resp.a[[1]])[1])
  pts.a[which(is.na(resp.a[[1]]$O2.t))] <- "yellow"
  points(O2 ~ elapsed, data = resp.a[[1]], pch = 22, bg = pts.a)
  lines(timevals.a, pred.a.t, lwd = 2, lty = 1, col = "red")
  lines(timevals.a, pred.a.e, lwd = 2, lty = 2, col = "red")
  
  pts.b <- rep("gray", dim(resp.b[[1]])[1])
  pts.b[which(is.na(resp.b[[1]]$O2.t))] <- "yellow"
  points(O2 ~ elapsed, data = resp.b[[1]], pch = 23, bg = pts.b)
  lines(timevals.b, pred.b.t, lwd = 2, lty = 1, col = "blue")
  lines(timevals.b, pred.b.e, lwd = 2, lty = 2, col = "blue")
  
  
  legend("topleft", c(paste("A: slope = ", round(coef(mod.a.l)[2], 3), ", ", 
                             "r2 = ", round(summary(mod.a.l)$r.squared, 3), 
                             sep = ""), 
                       paste("B: slope = ", round(coef(mod.b.l)[2], 3), ", ", 
                             "r2 = ", round(summary(mod.b.l)$r.squared, 3), 
                             sep = "")), 
         pch = c(22, 23), pt.bg = "gray", bty = "n", title = "Linear", cex = 0.7)
  legend("topright", c(paste("A: slope = ", round(coef(mod.a.e)[2], 3), ", ", 
                             "r2 = ", round(summary(mod.a.e)$r.squared, 3), 
                             sep = ""), 
                       paste("B: slope = ", round(coef(mod.b.e)[2], 3), ", ", 
                             "r2 = ", round(summary(mod.b.e)$r.squared, 3), 
                             sep = "")), 
         pch = c(22, 23), pt.bg = "gray", bty = "n", title = "Exponential", cex = 0.7)
}

plot_resp_na <- function(group = "", samp = ""){
  par(mar = c(5, 6, 3, 1))
  plot(1, 1, type = "n", 
       pch = 22, bg = "gray", 
       xlim = c(0, 40), ylim = c(4, 9),
       xlab = "", ylab = "", las = 1, axes = F,
       main = paste(group, ": ", samp, sep = ""))
  axis(side = 1, labels = T)
  axis(side = 2, labels = T, las = 1)
  mtext("Incubation Time (hrs.)", side = 1, line = 2.5, cex = 1.25)
  mtext("Dissolved Oxygen", side = 2, line = 4.25, cex = 1.25)
  mtext(expression(paste("(mg O"[2], " L"^"-1", ")")), 
        side = 2, line = 2.75, cex = 1)
  box(lwd = 1.5)
  legend("center", "No Data", bty = "n", 
         pt.lwd = 1.5, cex = 1.25, pch = 22, pt.bg = "gray")
}

fit.models <- function(dir = "", samps = "", series = "", rep = "", ignore = ""){
  resp.dir <- paste(dir, samps, series, sep = "/", collapse = "")
  rep.ignore <- ignore[ignore$Series == series & ignore$Rep == rep, ]
  files <- list.files(paste(resp.dir, rep, sep = "/", collapse = ""), pattern = ".txt")
  if(length(files) > 0){
    resp.temp <- fibox.resp(files = files, dir = resp.dir, set = rep)
    resp.temp$O2[which(resp.temp$O2 > 15)] <- NA
    resp.temp$O2.t <- resp.temp$O2
    if(dim(rep.ignore)[1] > 0){
      for(j in 1:dim(rep.ignore)[1]){
        resp.temp$O2.t[grepl(rep.ignore$Time[j], resp.temp$file)] <- NA
      }
    }
    # Full Linaer Model
    mod.lm <- lm(O2 ~ elapsed, data = resp.temp)
    # Trimmed Linear Model
    mod.tr <- lm(O2.t ~ elapsed, data = resp.temp)
    # Exponential Model
    mod.ex <- lm(log(O2.t) ~ elapsed, data = resp.temp)
    
  } else {
    resp.temp <- data.frame(date = NA, time = NA, elapsed = NA, O2 = NA, 
                            temp = NA, file = NA, O2.t = NA)
    mod.lm <- NA; mod.tr <- NA; mod.ex <- NA
  }
  output <- list("Data" = resp.temp, 
                 "Raw_Linear_Model" = mod.lm, 
                 "Trimmed_Linear_Model" = mod.tr, 
                 "Exponential_Model" = mod.ex)
  return(output)
}

process.fibox <- function(dir = "", output.fig = "", label = "", ignore = ""){
  samples <- list.files(dir)
  samps.ignore <- read.delim(ignore, header = T)
  
  data.resp <- as.data.frame(matrix(NA, nrow = length(samples), ncol = 22))
  colnames(data.resp) <- c("Sample", "Year", "Date", "Region", "BR_O2_T0", "TR_O2_T0",
                           "BR_A", "BR_B", "BR_A_R2", "BR_B_R2",
                           "Ex_BR_A", "Ex_BR_B", "Ex_BR_A_R2", "Ex_BR_B_R2",
                           "TR_A", "TR_B", "TR_A_R2", "TR_B_R2",
                           "Ex_TR_A", "Ex_TR_B", "Ex_TR_A_R2", "Ex_TR_B_R2")
  
  pdf(file = output.fig, width = 10, height = 4)
  layout(matrix(1:2, ncol = 2))
  par(oma = c(0.5, 0.5, 0.5, 0.5))
  
  for(i in 1:length(samples)){
    print(paste("Sample ", i , " of ", length(samples), ": ", samples[i], sep = ""))
    data.resp$Sample[i] <- samples[i]
    data.resp$Region[i] <- label
    temp.ignore <- samps.ignore[samps.ignore$Site %in% samples[i], ]
    
    # BR
    print("Processing Bacterial Respiration")
    resp.BR.A <- fit.models(dir = dir, samps = samples[i], 
                            series = "BR", rep = "A", ignore = temp.ignore)
    
    resp.BR.B <- fit.models(dir = dir, samps = samples[i], 
                            series = "BR", rep = "B", ignore = temp.ignore)
    if(dim(resp.BR.A[[1]])[1] >= 2 & dim(resp.BR.B[[1]])[1] >= 2){
      plot_resp(resp.a = resp.BR.A, resp.b = resp.BR.B, group = "BR", samp = samples[i])
      } else {
      plot_resp_na(group = "BR", samp = samples[i])
    }
    
    print("Processing Total Respiration")
    resp.TR.A <- fit.models(dir = dir, samps = samples[i], 
                            series = "TR", rep = "A", ignore = temp.ignore)
    
    resp.TR.B <- fit.models(dir = dir, samps = samples[i], 
                            series = "TR", rep = "B", ignore = temp.ignore)
    if(dim(resp.TR.A[[1]])[1] >= 2 & dim(resp.TR.B[[1]])[1] >= 2){
      plot_resp(resp.a = resp.TR.A, resp.b = resp.TR.B, group = "TR", samp = samples[i])
    } else {
      plot_resp_na(group = "TR", samp = samples[i])
    }

    data.resp$Date[i] <- as.character(format(min(as.Date(c(resp.BR.A[[1]]$date,
                                                           resp.BR.B[[1]]$date, 
                                                           resp.TR.A[[1]]$date, 
                                                           resp.TR.B[[1]]$date)), 
                                                 na.rm = T),  "%Y/%m/%d"))
    data.resp$Year[i] <- as.character(format(min(as.Date(c(resp.BR.A[[1]]$date, 
                                                           resp.BR.B[[1]]$date,
                                                           resp.TR.A[[1]]$date, 
                                                           resp.TR.B[[1]]$date)),
                                                 na.rm = T), "%Y"))
    
    BR.T0 <- c(na.omit(resp.BR.A[[1]]$O2)[1], na.omit(resp.BR.B[[1]]$O2)[1])
    TR.T0 <- c(na.omit(resp.TR.A[[1]]$O2)[1], na.omit(resp.TR.B[[1]]$O2)[1])
    data.resp$BR_O2_T0[i] <- ifelse(length(BR.T0 > 0), mean(BR.T0), "NA")
    data.resp$TR_O2_T0[i] <- ifelse(length(TR.T0 > 0), mean(TR.T0), "NA")
    
    if(all(!is.na(resp.BR.A))){
      data.resp$BR_A[i] <- coef(resp.BR.A[[3]])[2]
      data.resp$BR_A_R2[i] <- round(summary(resp.BR.A[[3]])$r.squared, 4)
      data.resp$Ex_BR_A[i] <- coef(resp.BR.A[[4]])[2]
      data.resp$Ex_BR_A_R2[i] <- round(summary(resp.BR.A[[4]])$r.squared, 4)
    }
    if(all(!is.na(resp.BR.B))){
      data.resp$BR_B[i] <- coef(resp.BR.B[[3]])[2]
      data.resp$BR_B_R2[i] <- round(summary(resp.BR.B[[3]])$r.squared, 4)
      data.resp$Ex_BR_B[i] <- coef(resp.BR.B[[4]])[2]
      data.resp$Ex_BR_B_R2[i] <- round(summary(resp.BR.B[[4]])$r.squared, 4)
    }
    if(all(!is.na(resp.TR.A))){
      data.resp$TR_A[i] <- coef(resp.TR.A[[3]])[2]
      data.resp$TR_A_R2[i] <- round(summary(resp.TR.A[[3]])$r.squared, 4)
      data.resp$Ex_TR_A[i] <- coef(resp.TR.A[[4]])[2]
      data.resp$Ex_TR_A_R2[i] <- round(summary(resp.TR.A[[4]])$r.squared, 4)
    }
    if(all(!is.na(resp.TR.B))){
      data.resp$TR_B[i] <- coef(resp.TR.B[[3]])[2]
      data.resp$TR_B_R2[i] <- round(summary(resp.TR.B[[3]])$r.squared, 4)
      data.resp$Ex_TR_B[i] <- coef(resp.TR.B[[4]])[2]
      data.resp$Ex_TR_B_R2[i] <- round(summary(resp.TR.B[[4]])$r.squared, 4)
    }
  }
  dev.off()
  graphics.off()
  return(data.resp)
}


