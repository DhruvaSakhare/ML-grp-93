## If you want to source() a bunch of files, something like
     ## the following may be useful:
      sourceDir <- function(path, trace = TRUE, exceptions=c(), ...) {
         for (nm in list.files(path, pattern = "\\.[RrSsQq]$", include.dirs=TRUE, recursive=TRUE)) {
           if(!(nm %in% exceptions)){
            if(trace) cat(nm,":")           
            source(file.path(path, nm), ...)
            if(trace) cat("\n")
          }
         }
       }

