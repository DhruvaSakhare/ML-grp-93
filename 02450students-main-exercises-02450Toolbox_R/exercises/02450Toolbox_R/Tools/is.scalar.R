is.scalar <- function(x){
if(!is.vector(x) || length(x)!=1 || !is.numeric(x)){
  FALSE
}else{
  TRUE}
}
