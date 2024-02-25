gmmposterior <- function(object, ...){
  # Calls either gmmposterior.Mclust or gmmposterior.mixEM depending on the class of the object.
  #
  # Author: Laura Frølich, lff@imm.dtu.dk
UseMethod("gmmposterior")
}
