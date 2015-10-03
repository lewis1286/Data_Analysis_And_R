
df = read.csv('/path/to/file.ext')

#linear regression:
lm.fit <- lm(y ~ x1 + x2 + ... + xn , data = dataframe)
summary(lm.fit)


bs(...) b spline basis
predict(...) function for predictions

# na.vector.and.median.fill
# this function takes in a datafram <dframe> copies all its vectors to 
# boolean vectors telling if the element is na or not, (named <vector>.is.na),
# replaces all na values with medians for existing vectors.
# Inputs:  dataframe
# Outputs: dataframe with extra vectors of na booleans, and all na's changed to medians
na.vector.and.median.fill <- function(dframe) {
  # skipping first variable (identity), adds binary vectors for each 
  for (i in c(2:dim(dframe)[2])) {
    feature.name <- names(dframe[i])
    dummy.name <- paste0("is.na.",feature.name)
    is.na.feature <- is.na(dframe[,feature.name])
    dframe[,dummy.name] <- as.integer(is.na.feature)
    dframe[is.na.feature,feature.name] <- median(dframe[,feature.name], na.rm = TRUE)
  }
  return(dframe)
}
t_feat <- na.vector.and.median.fill(t_feat)
dim(t_feat)
