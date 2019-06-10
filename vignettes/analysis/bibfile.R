library(RefManageR)
?GetDOIs()
?ReadBib
bib <- ReadBib(file="vignettes/CopyOfEntropyTypingRefs.bib")
bib <- GetDOIs(bib)
WriteBib(bib,file="entropy.bib")
