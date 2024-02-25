max_idx <- function(Y){
# function to extract the column index of the max value for each row of Y
#
# Author: Laura Frølich, lff@imm.dtu.dk
       apply(Y, 1, which.max)
}
