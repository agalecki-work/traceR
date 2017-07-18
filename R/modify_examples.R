traceReditor <- function (fun, lbl = ".", idx = 0, verbose = FALSE) {    
# function annotates fun with .traceR statements
 if (verbose) message("-traceReditor: starts")
 curle_brackect_symbol <- as.symbol("{")
 b_f <- body(fun)
 L <- if (is.symbol(b_f)  || as.character(b_f[1]) != curle_brackect_symbol) {
        c(curle_brackect_symbol, b_f)
      } else  as.list(b_f)       
T <- lapply(1:length(L), function(el){
        ix <- (idx *100 + el)/100
        lblx <- as.character(as.expression(L[[el]]))
        if (el == 1) {   
        substitute(.traceR(ix, lbl, first = TRUE, auto = TRUE), list(ix = ix, lbl = lblx))
        } else {
        substitute(.traceR(ix, lbl, auto = TRUE), list(ix = ix, lbl = lblx ))
        }})
  
out <- vector("list", 2 * length(L)-2 )
for (i in seq_along(L)) {
  if (i > 1) out[2*i-2] <- L[i]
  if (i < length(L)) out[2*i-1]  <- T[i] 
}

## Create new body
out0 <- list(
     quote(`{`),
     substitute(.functionLabel <- tx, list(tx = lbl)),
     quote(.traceR <- attr(options()$traceR, "fun")),
     quote(.traceR <- if (is.null(.traceR)) function(...){} else .traceR)
)

lx <- c(out0, out)
return(as.call(lx))
}
