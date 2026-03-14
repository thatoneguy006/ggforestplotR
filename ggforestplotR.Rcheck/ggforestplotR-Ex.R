pkgname <- "ggforestplotR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ggforestplotR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_forest_table")
### * add_forest_table

flush(stderr()); flush(stdout())

### Name: add_forest_table
### Title: Add a Side Table to a Forest Plot
### Aliases: add_forest_table

### ** Examples

coefs <- data.frame(
  term = c("Age", "BMI", "Treatment"),
  estimate = c(0.3, -0.2, 0.4),
  conf.low = c(0.1, -0.4, 0.2),
  conf.high = c(0.5, 0.0, 0.6),
  sample_size = c(120, 115, 98)
)

p <- ggforestplot(coefs, n = "sample_size")
add_forest_table(p, position = "left", show_n = TRUE, estimate_label = "Beta")

ggforestplot(coefs, n = "sample_size") +
  add_forest_table(position = "right", show_n = TRUE, estimate_label = "Beta")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
