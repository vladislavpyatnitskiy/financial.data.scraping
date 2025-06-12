# Use Data Frame from function enabling to get data from Smartlab website
smartlab.screener <- function(x,
                              levebitda = NULL, hevebitda = NULL, 
                              ldebtebitda = NULL, hdebtebitda = NULL, 
                              lpe = NULL, hpe = NULL, 
                              lpbv = NULL, hpbv = NULL, 
                              lps = NULL, hps = NULL, 
                              lpfcf = NULL, hpfcf = NULL, 
                              lmc = NULL, hmc = NULL){ 
  
  # Named list: each entry is a list(low, high)
  filters <- list(
    `EV/EBITDA` = list(levebitda, hevebitda), # EV / EBITDA
    `DEBT/EBITDA` = list(ldebtebitda, hdebtebitda), # DEBT / EBITDA
    `P/E` = list(lpe, hpe), # Price to Earnings
    `P/BV` = list(lpbv, hpbv), # Price to Book Value
    `P/S` = list(lps, hps), # Price to Sales
    `P/FCF` = list(lpfcf, hpfcf), # Price to Free Cash Flow
    `MARKET/CAP` = list(lmc, hmc) # Market Capitalisation
  )
  
  for (n in names(filters)) { B <- filters[[n]]
    
    # Apply lower bound if not NULL
    if (!is.null(B[[1]])) { x <- x[!is.na(x[[n]]) & x[[n]] >= B[[1]], ] }
    
    # Apply upper bound if not NULL
    if (!is.null(B[[2]])) { x <- x[!is.na(x[[n]]) & x[[n]] <= B[[2]], ] }
  }
  
  na.omit(x)
}
smartlab.screener(sm_data4, hevebitda = 5, hdebtebitda = 2, hpe = 10, lpe = 0,
                  hpfcf = 10, lpfcf = 0, hpbv = 1, hps = 2, lps = 0, lpbv = 0)
