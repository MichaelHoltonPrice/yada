# Create an x-vector and parameter vector for testing calculations
x <- seq(0,20,by=.1)

# The ordering of the parameter vector, theta, is
#
# (sh1,sc1,sh2,sc2,...)
#
# where, e.g., sh1 is the shape parameter of the first mixture and sc1 is the
# scale parameter of the first mixture.
sh1 <- 0.5
sc1 <- 5
sh2 <- 1.5
sc2 <- 10
sh3 <- 4.8
sc3 <- 12
th <- c(sh1,sc1,sh2,sc2,sh3,sc3)

# Test calc_weib_mix_density_matrix
expect_error(
  pdfMat <- calc_weib_mix_density_matrix(x,th),
  NA
)

expect_equal(
  dim(pdfMat),
  c(length(x),3)
)

expect_equal(
  pdfMat[,1],
  (sh1/sc1) * (x/sc1)^(sh1-1) * exp(-(x/sc1)^sh1)
)

expect_equal(
  pdfMat[,2],
  (sh2/sc2) * (x/sc2)^(sh2-1) * exp(-(x/sc2)^sh2)
)

expect_equal(
  pdfMat[,3],
  (sh3/sc3) * (x/sc3)^(sh3-1) * exp(-(x/sc3)^sh3)
)

# Test calc_weib_mix_density
z <- c(0.25,0.40,0.35)
expect_error(
  pdfVect <- calc_weib_mix_density(x,z,th),
  NA
)

expect_equal(
  z[1]*pdfMat[,1] + z[2]*pdfMat[,2] + z[3]*pdfMat[,3],
  pdfVect
)

# Test calc_trapez_weights
expect_equal(
  calc_trapez_weights(c(-1.5, 2, 3, 4, 7)),
  c(1.75, 2.25, 1, 2, 1.5)
)