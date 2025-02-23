# hardRain

R package for identification of rain in audio recordings

*Installation*  
`install.packages("devtools")`  
`devtools::install_github("https://github.com/Cdevenish/hardRain", dependencies = TRUE, build_vignettes = TRUE)`

*Vignettes*
See vignette
`browseVignettes("hardRain")`

*Examples*
```{r} 

library(hardRain)

citation("hardRain")

example("classifyRain")

# Get filenames of training data (known rain recordings in wav files). Only five files are used
# here for purposes of this example

train.fn <- list.files(system.file("extdata/rain", package = "hardRain"), "\\.wav$", full.names = T)

# Calculate the threshold using default settings - for two frequency bands
trBR <- getThreshold(train.fn)
trBR

# Get the test filenames (10 wav files with rain / non-rain)
test.fn <- list.files(system.file("extdata/test", package = "hardRain"), "\\.wav$", full.names = T)

# Classify the test files using the thresholds obtained above
resBR <- classifyRain(test.fn, thresh.vals = trBR)
head(resBR)

# How many files identified as rain/non-rain for each threshold?
tapply(resBR$value, list(resBR$threshold), table)


```




