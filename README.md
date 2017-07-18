# traceR
Annotating R functions to trace their execution

## Installation

* Development version from Github:
```
rm(list = ls())
library("devtools")
install_github("agalecki/traceR")
library (traceR)
```

## Create annotated body of a function

```
(fx <- function(x) x+2)
traceR:::traceReditor(fx)
```


## Annotation of a single function

```
(fx <- function(x) x^2)
annotate_fun(fx)                                     # fx annotated
annotate_fun(fx, flbl = "our_label")
annotate_fun(round)                                  # Body is null. Annotation not made. Error message.
annotate_fun(testthat:::as.expectation.expectation)                                 )
```

## Example: stringr package

* Check the body of a selected function 
```
sessionInfo()
ls(asNamespace("stringr"))      # namespace for stringr package
stringr:::word                  # Body of the function
```

* Annotating functions in stringr package
```
library(traceR)
library(stringr)
traceRedit(ns = "stringr")
detach(package:stringr)
stringr:::word                # Annotated function
```

* Create traceR report

```
library(stringr)
sentences <- c("Jane saw a cat", "Jane sat down")
traceR.on()          #  options()$traceR
word(sentences, 1)
head(.traceRmap)
dim(.traceRmap)
traceR.report()      # html map (dynamic tree) created
Objects()            # List of environments: e_1, ... 
ls(e_1)              # Names of objects in environment
e_1$start            # Object in environment 
e_1$string            
traceR.off()

```
