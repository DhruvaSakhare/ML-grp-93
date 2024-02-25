discreternd <- function(p, n=1){
# DISCRETERND Random numbers from a discrete distribution
#
# Usage:
#   p = [.5 .25 .25];
#   i = discreternd(p) 
#      Generates a random number in {1,2,3} with 50 pct. probability of
#      being 1 and 25 pct. probability of being 2 or 3.
#
#
# Input:
#   p           Vector of (relative) probabilities. If p sums to one, each 
#               element in p is the probability of generating the 
#               corresponding integer. If p is not normalized it denotes 
#               the relative probability.
#
# Output:
#   i           Vector of random integers
#               between 1 and n.
#
# Author: Laura Frølich, lff@imm.dtu.dk

  i = suppressWarnings(sample(x=1:n, prob=p, replace=TRUE))
}
