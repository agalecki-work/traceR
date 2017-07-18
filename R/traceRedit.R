check_fanno <- function(fun, flbl = deparse(substitute(fun))) {
# Check whether body of a function is null
 
 info <- character()
 if (mode(fun) != "function")       info <- c(info, message ("Object ", flbl, "is not a function"))
 if (is.null(body(fun)))            info <- c(info, message ("Function: ", flbl,  " body is null. No changes made."))
 if (!is.null(attr(fun, "locked"))) info <- c(info,  message("Function: ", flbl,  " already annotated. No changes made."))
 if (length(info > 0))    message ("Object/function ", flbl, "not suitable for annotations")
 return(info)
}

annotate_fun <- function(fun, flbl = ".", idx = 0, anno = "traceReditor"){
   funinfo <- check_fanno(fun, flbl = flbl) 
   if (length(funinfo) > 0)  return(invisible(fun))  # Function unchanged
   callx <- traceReditor(fun, flbl = flbl, idx = idx)
   funR <- fun
   body(funR) <-  callx
   attr(funR, "locked") <- TRUE
   attr(funR, "oldFun") <- fun
   funR 
}

traceReditFUN_ns <- function(cx = NULL, ns, pos = -1, envir = as.environment(pos), verbose = FALSE){
## cx  is character vector containing object names (only *functions* in namespace ns will be annotated)
    
   if (verbose)  message("traceReditFUN_ns: STARTS")
   if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || substring(nm, 1L, 8L) != "package:") 
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
   } else ns <- asNamespace(ns)
  
  if (is.null(cx))   cx  <-  ls(ns)  # By default all functions in ns
 
  idx <-  1:length(cx)


  for (i in seq_along(cx)){ 
     if (verbose) message("---", cx[i], "----")
     subx <- substitute(x, list(x = cx[i]))
     if (is.name(subx))  subx <- deparse(subx)
 
    if (!is.character(subx) || length(subx) != 1L) 
        stop("'fixInNamespace' requires a name")
        
    # if (verbose) message("Before get call")
    gsubx <- get(subx, envir = ns, inherits = FALSE)
    
    if (isFunctionClass(gsubx)){
    
    x <- traceReditor(gsubx, flbl = cx[i], idx = idx[i])
    if (verbose) message("Note: ", subx, " Before assignInNamespace")
    assignInNamespace(subx, x, ns)
    } else if (verbose) message("Note: ",  subx, " is of ", class(gsubx)[1], "class and it was NOT annotated")
}
invisible(cx)
}

isFunctionClass <- function(f) class(f)[1] == "function"

traceRedit <- function(cx = NULL, ns, pos = -1, envir = as.environment(pos), verbose = FALSE){

  traceReditFUN_ns(cx, ns, pos = -1, envir = as.environment(pos), verbose = verbose)

}





  

