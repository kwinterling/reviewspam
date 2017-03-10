

require(data.table)
require(ggplot2)
require(feather)
require(readr)
require(cpm)
require(bursts)

# df <- read.csv('ratings_Movies_and_TV.csv')

colorLevels <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple', 'pink')

detect_narrow <- function (burst_frame) {
  
}

run_all <- function () {
  # Run everything
  file_url <- 'http://snap.stanford.edu/data/amazon/productGraph/categoryFiles/ratings_Electronics.csv'
  download.file(file_url, 'ratings_Electronics.csv')
  dt <- as.data.table(read.csv('ratings_Electronics.csv'))
  colnames(dt) <- c('user', 'item', 'rating', 'timestamp')
  write_csv(dt, 'ratings.csv')
  extract_main()
  mainf()
}

extract_main <- function () {
  # Run to initialize csvs of products with more than 100 reviews
  df <- as.data.table(read.csv('ratings.csv'))
  pids <- unique(df$item)
  extract_products(df, pids, 1, 3000, 'fl1.csv')
  extract_products(df, pids, 3000, 6000, 'fl2.csv')
  extract_products(df, pids, 6000, 9000, 'fl3.csv')
  extract_products(df, pids, 9000, 12000, 'fl4.csv')
  extract_products(df, pids, 12000, 15000, 'fl5.csv')
  extract_products(df, pids, 15000, 18000, 'fl6.csv')
  extract_products(df, pids, 18000, 20000, 'fl7.csv')
}

mainf <- function () {
  # Produce plots and results csv files
  csvs <- c('fl1.csv', 'fl2.csv', 'fl3.csv', 'fl4.csv', 'fl5.csv', 'fl6.csv', 'fl7.csv')
  # simulateRatings(200, 500, 25, 2)
  
  for (i in 1:length(csvs)) {
    burstDetect(3, csvs[i])
  }
}

posRatings <- function (num) {
  if (num < 0.5) {
    rating <- 5
  } else if ((num > 0.50) && (num <= 0.70)) {
    rating <- 4
  } else if ((num >= 0.70) && (num < 0.80)) {
    rating <- 3
  } else if ((num >= 0.80) && (num < 0.90)) {
    rating <- 1
  } else {
    rating <- 2
  }
  rating
}

attackRatings <- function (num) {
  if (num < 0.8) {
    rating <- 1
  } else if ((num > 0.8) && (num <= 0.9)) {
    rating <- 2
  } else if ((num >= 0.9) && (num < 0.93)) {
    rating <- 3
  } else if ((num >= 0.93) && (num < 0.95)) {
    rating <- 4
  } else {
    rating <- 5
  }
  rating
}

simulateRatings <- function (n, m, seedval, attacks) {
  # Create simulated data and make plots
    set.seed(seedval)
  
  cdir <- getwd()
  ifelse(!dir.exists(file.path(cdir, 'simcsvs')), dir.create(file.path(cdir, 'simcsvs')), FALSE)
  ifelse(!dir.exists(file.path(cdir, 'simplots')), dir.create(file.path(cdir, 'simplots')), FALSE)
  ifelse(!dir.exists(file.path(cdir, 'simavgplots')), dir.create(file.path(cdir, 'simavgplots')), FALSE)
  
  for (ii in 1:m) {
    pid <- ii
    timestamp <- floor(cumsum(rexp(n, rate = 0.000005)))
    rating <- sapply(runif(n, 0, 1), posRatings)
    
    if (attacks > 0) {
      for (j in 1:attacks) {
        attackinit <- sample(1:n, 1)
        attackstart <- timestamp[attackinit]
        nreviews <- sample(50:200, 1)
        
        attacktimes <- attackstart + cumsum(rexp(nreviews, 0.00005))
        attratings <- sapply(runif(nreviews, 0, 1), attackRatings)
        
        timestamp <- c(timestamp, attacktimes)
        rating <- c(rating, attratings)
      }
    }
    item <- numeric(length(rating)) + pid
    dt <- data.table(item = item, rating = rating, timestamp = timestamp)
    
    dtt <- dt
    dtt <- dtt[order(dtt$timestamp),]
    dtt$timestamp <- fixtimes(dtt$timestamp, 360)
    dtt <- dtt[order(dtt$timestamp),]
    ratingThreshold <- 3
    
    dttt <- dtt[rating < ratingThreshold]
    
    ratingBursts <- kleinberg(dttt$timestamp)
    if (nrow(ratingBursts) > 1) {
      write_csv(ratingBursts, paste('simcsvs/', ratingThreshold, pid, 'results.csv', sep='_'))
      dtt$numreviews <- cumsum(numeric(nrow(dtt)) + 1)
      dttt$numreviews <- cumsum(numeric(nrow(dttt)) + 1)
      dttt$date <- as.POSIXct(dttt$timestamp, origin = "1970-01-01")
      dtt$date <- as.POSIXct(dtt$timestamp, origin = "1970-01-01")
      dtt$avgrating <- (1/(1:length(dtt$rating)))*cumsum(dtt$rating)
      
      pl <- ggplot(dttt, aes(date, numreviews)) +
        geom_line() +
        labs(title = paste('Product ID:', pid, sep = ' '), x = 'Date', y = 'Total Negative Reviews')
      avgpl <- ggplot(dtt, aes(date, avgrating)) +
        geom_line() +
        geom_smooth() +
        labs(title = paste('Product ID:', pid, sep = ' '), x = 'Date', y = 'Average Rating')
      
      for (j in 2:nrow(ratingBursts)) {
        row <- ratingBursts[j,]
        xmi <- as.POSIXct(row$start, origin = "1970-01-01")
        xma <- as.POSIXct(row$end, origin = "1970-01-01")
        ymi <- (dtt[date == xmi])$numreviews
        yma <- (dtt[date == xma])$numreviews
        lev <- row$level
        rct <- data.table(xmin = xmi, xmax = xma, ymin = -Inf, ymax = Inf)
        pl <- pl + geom_rect(data = rct, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                             color = colorLevels[lev],
                             alpha = 0.5,
                             inherit.aes = FALSE)
        avgpl <- avgpl + geom_rect(data = rct, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                   color = colorLevels[lev],
                                   alpha = 0.5,
                                   inherit.aes = FALSE)
      }
      pl
      ggsave(paste('simplots/', ii, 'plot.pdf', sep = '_'), plot = pl)
      avgpl
      ggsave(paste('simavgplots/', ii, 'plot.pdf', sep='_'), plot = avgpl)
    }
  }
}

burstDetect <- function (ratingThreshold, dsubcsv) {
  # Create plots from data
  cdir <- getwd()
  ifelse(!dir.exists(file.path(cdir, 'csvs')), dir.create(file.path(cdir, 'csvs')), FALSE)
  ifelse(!dir.exists(file.path(cdir, 'plots')), dir.create(file.path(cdir, 'plots')), FALSE)
  ifelse(!dir.exists(file.path(cdir, 'avgplots')), dir.create(file.path(cdir, 'avgplots')), FALSE)
  ifelse(!dir.exists(file.path(cdir, 'hstplots')), dir.create(file.path(cdir, 'hstplots')), FALSE)
  
  dt <- as.data.table(read.csv('ratings.csv'))
  pids <- unique(dt$item)
  dsubset <- as.data.table(read.csv(dsubcsv))
  inds <- dsubset$item
  
  for (i in inds) {
    dtt <- dt[item == pids[i]]
    # dtt <- dtt[rating < ratingThreshold]
    dtt <- dtt[order(dtt$timestamp),]
    dtt$timestamp <- fixtimes(dtt$timestamp, 360)
    dtt <- dtt[order(dtt$timestamp),]
    
    dttt <- dtt[rating < ratingThreshold]
    
    if (length(dttt$timestamp) > 2) {
    ratingBursts <- kleinberg(dttt$timestamp)
    if (nrow(ratingBursts) > 1) {
      write_csv(ratingBursts, paste('csvs/', ratingThreshold, pids[i], 'results.csv', sep='_'))
      dtt$numreviews <- cumsum(numeric(nrow(dtt)) + 1)
      dttt$numreviews <- cumsum(numeric(nrow(dttt)) + 1)
      dttt$date <- as.POSIXct(dttt$timestamp, origin = "1970-01-01")
      dtt$date <- as.POSIXct(dtt$timestamp, origin = "1970-01-01")
      dtt$avgrating <- (1/(1:length(dtt$rating)))*cumsum(dtt$rating)
      
      pl <- ggplot(dttt, aes(date, numreviews)) +
        geom_line() +
        labs(title = paste('Product ID:', pids[i], sep = ' '), x = 'Date', y = 'Total Negative Reviews')
      avgpl <- ggplot(dtt, aes(date, avgrating)) +
        geom_line() +
        geom_smooth() +
        labs(title = paste('Product ID:', pids[i], sep = ' '), x = 'Date', y = 'Average Rating')
      hpl <- ggplot(dtt, aes(rating)) + stat_count(geom = "bar")
      
      for (j in 2:nrow(ratingBursts)) {
        row <- ratingBursts[j,]
        xmi <- as.POSIXct(row$start, origin = "1970-01-01")
        xma <- as.POSIXct(row$end, origin = "1970-01-01")
        ymi <- (dtt[date == xmi])$numreviews
        yma <- (dtt[date == xma])$numreviews
        lev <- row$level
        rct <- data.table(xmin = xmi, xmax = xma, ymin = -Inf, ymax = Inf)
        pl <- pl + geom_rect(data = rct, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                             color = colorLevels[lev],
                             alpha = 0.5,
                             inherit.aes = FALSE)
        avgpl <- avgpl + geom_rect(data = rct, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                   color = colorLevels[lev],
                                   alpha = 0.5,
                                   inherit.aes = FALSE)
      }
      pl
      ggsave(paste('plots/', pids[i], 'plot.pdf', sep = '_'), plot = pl)
      ggsave(paste('hstplots/', pids[i], 'plot.pdf', sep = '_'), plot = hpl)
      avgpl
      ggsave(paste('avgplots/', pids[i], 'plot.pdf', sep='_'), plot = avgpl)
    }
    
    }
  }
}

fixtimes <- function (times, seconds) {
  # Add seconds if two consecutive times are the same in a series
  n <- length(times) - 1
  ftimes <- times
  
  for (i in 1:n) {
    if (times[i] == times[i + 1]) {
      ftimes[i + 1] <- ftimes[i] + seconds
    }
  }
  ftimes
}

mainfunc <- function (df) {
  pid <- '0310263662'
  dff <- subset(df, item == pid)
  dff <- subset(dff, rating == 1)
  dff
}

getoffsets <- function (times) {
  offsets <- numeric()
  n <- length(times) - 1
  for (i in 1:n) {
    offsets[i] <- times[i + 1] - times[i]
  }
  offsets <- c(offsets, 0)
  offsets
}

extract_products <- function (df, pids, a, b, fn) {
  # Extract products with more than 100 reviews in index range [a, b] and
  # write to fn
  pds <- character()
  counts <- numeric()
  
  for (i in a:b) {
    dff <- df[item == pids[i]]
    nr <- nrow(dff)
    if (nr > 100) {
      pds <- c(pds, i)
      counts <- c(counts, nr)
    }
  }
  dt <- data.table(item = pds, reviews = counts)
  write_csv(dt, fn)
  print(fn)
}



plotnumreviews <- function (product_id) {
  dff <- subset(df, item == product_id)
  tss <- as.POSIXct(dff$timestamp, origin="1970-01-01")
  dff$time <- tss
  dff <- dff[order(dff$timestamp),]
  dff$numreviews <- 1:length(dff$rating)
  qplot(time, numreviews, data = dff)
}

plotavg <- function (product_id) {
  dff <- subset(df, item == product_id)
  tss <- as.POSIXct(dff$timestamp, origin="1970-01-01")
  dff$time <- tss
  dff <- dff[order(dff$timestamp),]
  numreviews <- 1:length(dff$rating)
  dff$avgrating <- (1/numreviews)*cumsum(dff$rating)
  qplot(time, avgrating, data = dff)
}

getones <- function (k, x) {
  if (x == k)
    1
  else
    0
}

plkstars <- function (k, product_id) {
  dff <- subset(df, item == product_id)
  tss <- as.POSIXct(dff$timestamp, origin="1970-01-01")
  dff$time <- tss
  dff <- dff[order(dff$timestamp),]
  dff$kstars <- cumsum(lapply(dff$rating, function (x) {getones(k, x)}))
  qplot(time, kstars, data = dff)
}


ddetectChangePoint <- function (tseries, tsfield) {
  changetimes <- numeric()
  changepoints <- numeric()
  times <- tseries$time
  values <- tseries[tsfield]
  
  cpm <- makeChangePointModel(cpmType = "Kolmogorov-Smirnov", ARL0 = 500)
  
  i <- 0
  
  while (i < length(values)) {
    i <- i + 1
    
    cpm <- processObservation(cpm, values[i])
    
    if (changeDetected(cpm) == TRUE) {
      print(sprintf("Change detected at observation %d", i))
      
      changetimes <- c(changetimes, i)
      
      Ds <- getStatistics(cpm)
      tau <- which.max(Ds)
      
      if (length(changepoints) > 0) {
        tau <- tau + changepoints[length(changepoints)]
      }
      changepoints <- c(changepoints, tau)
      
      cpm <- cpmReset(cpm)
      
      # If a changepoint was detected, continue the while loop at the changepoint
      i <- tau
    }
  }
  cpointtimes <- data.frame(changetimes, changepoints)
  cpointtimes
}


#   `plotdata('0310263662')
