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




# Examples of modifying body of a function

```
(fx <- function(x) x+2)
traceR:::traceReditor(fx)
```



## Annotation of a single function

```
(fx <- function(x) x^2)
traceReditf(fx, lbl = "our_label", idx = 3)                # fx annotated
traceReditf(round)                                  # Body is null. Annotation not made. Error message.
traceReditf(testthat:::as.expectation.expectation)                                 )
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
