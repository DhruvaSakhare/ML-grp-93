binarize2 <- function(X,attributeNames)
{
  # Construct binary version of the matrix X (with N observations and M attributes).
  # Input data matrix (X) should contain only numeric attributes, input attributeNames is optional
  #
  # Output contains a list with X (binarized data) and attributeNames (names of binarized attributes)
  # Binarized data containts (2*M) columns: 
  # The first M columns correspond to X values larger than median value (of each corresponding column).
  # Last M columns correspond to X values smaller than median value (of each corresponding column).
  #
  # Ex: the first attribute of X is binarized into 2 attributes: attribute >= median and
  # attribute < median. These are now the first and (M+1)th attributes of the output
  #
  # Author: Yashar Sabaz, s143093@student.dtu.dk
  # Updated November 2017 by Quoc Tien Au
  
  N = dim(X)[1]; 
  M = dim(X)[2];
  
  'Attributes non-binary: Forcing representation to be binary.'
  med = apply(X, 2, median)
  X <- t(apply(X,1,'>', med))
  
  binarized <- list();
  binarized$X <- X;
  
  'Get attributes < median'
  smaller_than_median_attributes = !(binarized$X);
  binarized$X = cbind(binarized$X, smaller_than_median_attributes)*1;
  
  
  'Write names for "new" attributes, if it has been input'
  if(missing(attributeNames)==FALSE){
  
    att_names_bin = paste(attributeNames, '50%-100% percentile');
    att_names_bin2 = paste(attributeNames, '0%-50% percentile');
    binarized$attributeNames = c(att_names_bin, att_names_bin2);
    
  }
  
    
  
  binarized
  
  
}
