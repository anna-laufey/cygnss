
CYGNSSsampling <- function(y, type, n) {
  
  # y - object to sample (? x 4 [ws, lon, lat, time])
  # type - string: 'random', or 'every-few', or 'averaging'
  # n - if n > 1: size of subsample, else if 0 < n <= 1: the percentage 
  #               of size of population to subsample
  ## RETURNS: an nx4 matrix, a subsample of y.
  
  rows = dim(y)[1]
  cols = dim(y)[2]
  
  #assert that the input y is valid
  if (cols != 4) { 
    print('ERROR: Make sure that the input y has 4 columns: ws, lon, lat, time')
    return(NULL)
  }
  
  #assert that the input n is valid
  if (n < 0) { 
    print('ERROR: Invalid number of samples.')
    return(NULL)
  } else if (n > rows) {
    print('ERROR: n is larger than the size of the population.')
    return(NULL)
  }
  
  #get the number of samples that are needed (Nsamples)
  if (n <= 1) { #get the number of samples that are needed (Nsamples)
    Nsamples = floor(n*rows)
  } else {
    Nsamples = n
  }
  
  
  if (type == 'random') { # simple random sampling, without replacement.
    
    ind = sample(1:rows, Nsamples, replace=FALSE)
    sampledResult = y[ind,]
    return(sampledResult)
    
  } else if (type == 'every-few') { # only take every few* observations, *depending on Nsamples
    
    by = floor(rows/Nsamples)
    ind = seq(1,rows,by)
    ind = ind[1:Nsamples]
    sampledResult = y[ind,]
    return(sampledResult)
    
  } else if (type == 'averaging') { # average every few* observations together, *depending on Nsamples
    
    by = floor(rows/Nsamples)
    
    ind = seq(1,rows,by)
    ind = ind[1:Nsamples]
    
    sampledResult = matrix(nrow = Nsamples, ncol = 4)
    
    if (by > 1) {
      k = 1
      for (i in ind) {
        sampledResult[k,] = colMeans(y[i:(i+by-1),])
        k = k + 1
      }
    } else {
      sampledResult = y[ind,]
    }
    
    return(sampledResult)
    
  } else { # assert that input type is valid
    print('ERROR: Invalid type.')
    return(NULL)
  }
  
  
}

# test cases ----

# A = matrix(c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8), nrow = 8, ncol = 4)
# A
# CYGNSSsampling(A, 'random', 5)
# CYGNSSsampling(A, 'random', 0.5)
# CYGNSSsampling(A, 'every-few', 4)
# CYGNSSsampling(A, 'every-few', 0.5)
# CYGNSSsampling(A, 'averaging', 3)
# CYGNSSsampling(A, 'averaging', 4)
# CYGNSSsampling(A, 'averaging', 5)
# CYGNSSsampling(A, 'averaging', 0.5)

