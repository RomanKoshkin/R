# create a matrix of 100 rows and 3 columns. The colums will be filled with Gaussian data (mean = 0,2 and 5 respectively)
m <- matrix(rnorm(100,0,1),rnorm(100,2,1),rnorm(100,5,1), nrow=100, ncol=3) 
apply(m, 2, mean) # find means of all the 3 columns