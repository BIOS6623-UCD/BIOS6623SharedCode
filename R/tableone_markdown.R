################################################################################
# Generating a table 1 using the tidyverse and Peter DeWitt's qwraps2 package
#
# This approach is a bit more work than many 'table one' packages and functions
# but is highly customizable and fits into a tidy workflow.  It also allows
# printing to markdown or latex.
#
# This example was largely taken from Peter's blog:
# http://www.peteredewitt.com/2016/summary-table/
#
# NOTE: There is a bug in the summary_table() function that results in the wrong
# row title if you only have one summary stat in a given row group. (issue #42
# https://github.com/dewittpe/qwraps2/issues/42).  To get around it in the
# second example below, I've added an 'n missing` row for each row group.
################################################################################


# qwraps2 is on CRAN, so install from there.
if (!require(qwraps2)) install.packages("qwraps2")

library(dplyr)
library(tibble)
library(stringr)
library(qwraps2)
options(qwraps2_markup = 'markdown') # default is latex

# Define the statistics and structure we want.
#   - the outer list defines the row groups (first column)
#   - the inner lists define the statistics and row names we want for each row
#     group.
#   - qwraps2 has helper functions for combining and formatting statistics
#     commonly reported together (N & percent, mean & SD, median and IQR). Or as
#     shown in the Weight - Range item, you can create your own.  
#   - Note that the actual values here are function (defined by the tilde ~),
#     the actual calculation occurs in the next step.
mtcar_summaries <-
  list("Miles Per Gallon" =
       list("min:"         = ~ min(mpg),
            "mean (sd)"    = ~ qwraps2::mean_sd(mpg, denote_sd = "paren"),
            "median (IQR)" = ~ qwraps2::median_iqr(mpg),
            "max:"         = ~ max(mpg)),
       "Cylinders" = 
         list("mean"             = ~ mean(cyl),
              "mean (formatted)" = ~ qwraps2::frmt(mean(cyl)),
              "4 cyl, n (%)"     = ~ qwraps2::n_perc0(cyl == 4),
              "6 cyl, n (%)"     = ~ qwraps2::n_perc0(cyl == 6),
              "8 cyl, n (%)"     = ~ qwraps2::n_perc0(cyl == 8)),
       "Weight" =
         list("Range" = ~ paste(range(wt), collapse = ", "))
       )

# Generate (and calculate) table
#  prints a markdown table
mtcars %>% summary_table(mtcar_summaries)

# Or a latex table
options(qwraps2_markup = 'latex')
mtcars %>% summary_table(mtcar_summaries)


############################################################
# Now say we want to create a table with p-values for some 
# two group comparison
#
# Hypothesis tests and P-values are not a feature of this 
# table function, so we will add them to the row group names 
# (variable titles) ourselves.  
#
# Definitely a hack, but it works
############################################################
options(qwraps2_markup = 'markdown')

# label our group values (AT=Automatic transmission, MT=Manual)
my_mtcars <- mtcars %>% mutate(am = factor(am, levels = 0:1, 
                                           labels = c("AT", "MT")))

# Lets restrict our summary stats to those that match the hypothesis test
# for demonstration purposes, we'll use a t-test, Fisher's exact, and Wilcoxon
# rank sum test respectively.
# Our comparison will be between cars with manual and automatic transmissions.
mtcar_summaries <-
  list("Miles Per Gallon" =
         list("mean (sd)"    = ~ qwraps2::mean_sd(mpg, denote_sd = "paren"),
              "n missing"    = ~ paste0(frmt(sum(is.na(mpg))))),
       "Cylinders" =
         list("4 cyl, n (%)"     = ~ qwraps2::n_perc0(cyl == 4),
              "6 cyl, n (%)"     = ~ qwraps2::n_perc0(cyl == 6),
              "8 cyl, n (%)"     = ~ qwraps2::n_perc0(cyl == 8),
              "n missing"    = ~ paste0(frmt(sum(is.na(cyl))))),
       "Weight" =
         list("median (IQR)" = ~ qwraps2::median_iqr(wt),
              "n missing"    = ~ paste0(frmt(sum(is.na(wt)))))
       )

# 2-group table without p-values
tab1 <- my_mtcars %>% group_by(am) %>% summary_table(mtcar_summaries)
tab1

# Calculate p-values and format into a nice markdown string
#   - titles here must match the ones above
tab1_pvals <- 
  c("Miles Per Gallon" = t.test(mpg ~ am, data = my_mtcars)$p.value,
    "Cylinders"        = fisher.test(my_mtcars$cyl, my_mtcars$am)$p.value,
    "Weight"           = wilcox.test(wt ~ am, data = my_mtcars, exact = FALSE)$p.value
    ) %>% tbl_df %>% rownames_to_column %>%
  mutate(fancy_name = paste0(rowname, " (", frmtp(value), ")")) %>%
  mutate(rowname = str_replace_all(rowname, c("\\(" = "\\\\(", "\\)" ="\\\\)")))

# Replace original row group title with our new one w/ p-values
#   note, the titles are buried in the attributes of our table object
attr(attr(tab1, "rgroups"), "names") <- 
  attr(attr(tab1, "rgroups"), "names") %>% 
  str_replace_all(tab1_pvals$rowname, tab1_pvals$fancy_name)

# DONE! Let's see the results
tab1



