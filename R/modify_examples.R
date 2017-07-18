traceReditor <- function (fun, flbl = deparse(substitute(fun)), idx = 0) {    
# function annotates fun with .traceR statements
# Returns an object of class "call"
 
 b_f <- body(fun)
 if (is.null(b_f) || length(b_f) == 1) return(body(fun)) 
 curle_brackect_symbol <- as.symbol("{")
 L <- if (is.symbol(b_f)  || as.character(b_f[1]) != curle_brackect_symbol) {
        c(curle_brackect_symbol, b_f)
      } else  as.list(b_f)       
 T <- lapply(1:length(L), function(el){
        ix <- (idx *100 + el)/100
        lblx <- as.character(as.expression(L[[el]]))
        if (el == 1) {   
        substitute(.traceR(ix, flbl, first = TRUE, auto = TRUE), list(ix = ix, flbl = lblx))
        } else {
        substitute(.traceR(ix, flbl, auto = TRUE), list(ix = ix, flbl = lblx ))
        }})
  
out <- vector("list", 2 * length(L)-2 )
for (i in seq_along(L)) {
  if (i > 1) out[2*i-2] <- L[i]
  if (i < length(L)) out[2*i-1]  <- T[i] 
}

## Create new body
out0 <- list(
     quote(`{`),
     substitute(.functionLabel <- tx, list(tx = flbl)),
     quote(.traceR <- attr(options()$traceR, "fun")),
     quote(.traceR <- if (is.null(.traceR)) function(...){} else .traceR)
)

lx <- c(out0, out)
return(as.call(lx))
}
