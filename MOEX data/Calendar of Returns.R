lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs

moex.calendar <- function(x, s = NULL, e = NULL, transpose = F, data = T){
 
  if (data){ p <- NULL # Loop for data extraction 
    
    getData <- function(A, s, e) { 
      if (is.null(s) && is.null(e))
        return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
      if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
      if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
      return(get_candles(A, from = s, till = e, interval = 'daily')) 
    }
    for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])

     message(
      sprintf(
       "%s is downloaded; %s from %s", 
       A, which(x == A), length(x)
      )
     )
                 
     D <- D[!duplicated(D),] # Remove duplicates
     
     p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
     
     p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
     
     colnames(p) <- x } else { p <- x } 
     
  r <- diff(log(as.timeSeries(p)))[-1,] # Calculate logs
  
  calendar <- NULL # to store data frames
  
  for (j in 1:ncol(r)){ d <- r[,j] # Assign each column
  
    d <- data.frame(as.Date(rownames(d)), d) # Move Dates to Data Frame
    
    rownames(d) <- seq(nrow(d)) # Substitute dates with ascending numbers
    
    D <- NULL # Loop to make monthly data
    
    for (n in 2:ncol(d)){ # Convert daily data to monthly
      
      v <- round(tapply(d[,n], format(as.Date(d[,1]), "%Y-%m"), sum), 4) * 100
      
      v <- data.frame(rownames(v), v) # Join with new data set
      
      rownames(v) <- seq(nrow(v)) # Generate sequence for index column
      
      colnames(v)[1] <- 'Date' # Name column as Date
      
      # Define empty variable if it is still empty or Put in the dataset there
      if (is.null(D)){ D = v } else { D <- merge(x = D, y = v, by = "Date") } }
    
    D <- as.data.frame(D) # Convert to data frame format
    
    colnames(D) <- colnames(d) # Give column names
    
    colnames(D)[1] <- 'Date' # Date column
    
    Y <- as.data.frame(substr(D[,1], 1, 4)) # Year value
    M <- as.data.frame(substr(D[,1], 6, 7)) # Month value
    
    D <- D[,-1] # Reduce 
    
    D <- data.frame(Y, M, D) # Data Frame with Year, month and return
    
    colnames(D) <- c("Year", "Month", colnames(D)[2]) # Assign column names
    
    C <- NULL # Data Frame for Joined Year columns
    
    for (m in 1:length(unique(D[,1]))){ l <- as.data.frame(sort(unique(D[,2]))) 
    
      l2 <- D[D[,1] == unique(D[,1])[m],] # First unique year
      
      colnames(l) <- "Month" # Give column name to Data Frame of months
      
      l3 <- merge(l2, l, by = "Month", all = T) # Merge months Data Frames
      
      l3 <- l3[,c("Year", "Month", colnames(D)[3])] # Column names to DF
      
      if (isTRUE(any(is.na(l3)))){ # Give name to year observation with NA
        
        l3[is.na(l3[,1]),][,1] <- D[D[,1] == unique(D[,1])[m],][1,1] }
      
      l3 <- l3[,-1] # Delete Year Column 
      
      # Assign Year number name as column name of returns
      colnames(l3)[2] <- D[D[,1] == unique(D[,1])[m],][1,1]
      
      m1 <- data.frame(l3[,1], month.name) # Data Frame with months
      
      colnames(m1) <- c("Month", "Months") # Column names for numbers and names
      
      l4 <- merge(m1, l3, by = "Month") # Join Months and returns by numbers
      
      l4 <- l4[,-1] # Reduce column with number of months instead of names
      
      m2 <- l4[,1] # Assign month column to new variable
      
      l4 <- as.data.frame(l4[,-1]) # Reduce excessive month column
      
      rownames(l4) <- m2 # Months as row names and year as column name
      colnames(l4) <- D[D[,1] == unique(D[,1])[m],][1,1] 
      
      if (is.null(C)){ C <- l4 } else { C <- cbind(C, l4) } } # Join
      
    # Median and Mean for each month
    C$Median <- round(apply(C, 1, median, na.rm = T), 2)
    C$Mean <- round(apply(C[,1:(ncol(C) - 1)], 1, mean, na.rm = T), 2)
    
    g <- list(c(0, 1), c(0, 1, 2), list(sum, median, mean)) # List with values
    
    for (n in 1:length(g[[3]])){ # Calculate Sum, Median and Mean for each year
      
      C[nrow(C) + 1,] = round(apply(C[1:(nrow(C) - g[[2]][n]),], 2, g[[3]][[n]],
                                    na.rm = T), 2) }
    
    for (n in 1:length(g[[3]])){ for (m in 1:length(g[[1]])){ # Cut unnecessary
      
        C[nrow(C) - g[[2]][n], ncol(C) - g[[1]][m]] <- "" } } # Values
    
    rownames(C)[(nrow(C) - 2):nrow(C)] <- c("Sum", "Median", "Mean") # Names
    
    if (isTRUE(transpose)){ C <- t(C) } # Transpose if needed
    
    calendar <- c(calendar, list(colnames(r[,j]), C)) } # Add to list
    
  calendar # Display 
}
moex.calendar("IMOEX")
