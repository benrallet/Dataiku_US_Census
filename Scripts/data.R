# Functions to pre-process the data

# Replace the quantitaive variables reprensenting the country of birth
# by a binary variable equal to 1 with the country is the USA, 0 otherwise
data.modificationOfCountryOfBirth <- function(X) {
  for (col in c("PEFNTVTY", "PEMNTVTY", "PENATVTY")) {
    X[[col]] <- apply(as.data.frame(X[[col]]), 1, function(x) {if (x == " United-States") x <- 1 else x <- 0})
  }
  X
}

# Transform quantitative variables with 2 levels 
# by a binary quantitativevariable
data.modificationBinaryVariable <- function(X) {
  X[["ASEX"]] <- apply(as.data.frame(X[["ASEX"]]), 1, function(x) {if (x == " Female") x <- 1 else x <- 0})
  X[["YEAR"]] <- apply(as.data.frame(X[["YEAR"]]), 1, function(x) {if (x == 95) x <- 1 else x <- 0})
  X
}