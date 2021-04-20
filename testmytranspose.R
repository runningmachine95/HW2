# function 'mytranspose'
mytranspose <- function(x) {
  # for matrices
  if (class(x) == 'matrix'){
    if ((nrow(x) == 0) || (ncol(x) == 0)) {
      y = matrix(nrow=0, ncol=0)
    }
    else {
      y = matrix(1, nrow=ncol(x), ncol = nrow(x))
      for(i in 1:nrow(x)) {
        for(j in 1:ncol(x)) {
          y[j,i] <- x[i,j]
        }
      }
    }
  }
  # for vectors
  else if((class(x) == 'numeric') || (class(x) == 'character')){
    y = matrix(1, nrow=1, ncol = length(x))
    for (i in 1:length(x)){
      y[i] = x[i]
    }
  }
  # for booleans
  else if (class(x) == 'logical'){
    y=c()
    for (i in 1:length(x)){
      y=c(y,x[i])
    }
    y=matrix(y, nrow=1, ncol = length(x))
  }
  # for dataframes
  else if(class(x) == 'data.frame'){
    y = matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] = as.matrix(x)[i,j]
      }
    }
    rownames(y) = colnames(x)
  # for missing values
  }
  else if ((class(x)) == 'NULL'){
    y=c()
  }
  else if (is.na(x)){
    y = NA
  }
  return(y)
}



# Test
assertEquals = function(exp, act) {
  return(all.equal(exp, act))
}

myvar1 <-  matrix(1:10, nrow=5, ncol=2)
assertEquals(t(myvar1), mytranspose(myvar1))

myvar1 <-  matrix(NA, nrow=0, ncol=0)
assertEquals(t(myvar1), mytranspose(myvar1))

myvar1 <-  matrix(c(1,2), nrow=1, ncol=2)
assertEquals(t(myvar1), mytranspose(myvar1))

myvar1 <-  matrix(c(1,2), nrow=2, ncol=1)
assertEquals(t(myvar1), mytranspose(myvar1))



myvar2 <- c(1,2,NA,3)
assertEquals(t(myvar2), mytranspose(myvar2))

myvar2 <- c(NA)
assertEquals(t(myvar2), mytranspose(myvar2))

myvar2 <- c()
assertEquals(c(), mytranspose(myvar2))


d <- c(1,2,3,4)
assertEquals(t(d), mytranspose(d))

e <- c("red", "white", "red", NA)
assertEquals(t(e), mytranspose(e))

f <- c(TRUE,TRUE,TRUE,FALSE)
assertEquals(t(f), mytranspose(f))

mydata3 <- data.frame(d,e,f)
assertEquals(t(mydata3), mytranspose(mydata3))