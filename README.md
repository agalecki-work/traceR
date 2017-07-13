# traceR
Tracing execution of R functions

## Installation

* Development version from Github:
```
library("devtools"); install_github("agalecki/traceR")
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
traceR.on()
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
