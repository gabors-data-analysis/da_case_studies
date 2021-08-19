###
# SIMPLE SCRIPT TO TRY NICE DATA SUMMARY TABLES
#
# I use a toy example based on datasummary from modelsummary package
#
# I use multiple descriptives:
#   1) Simplest case


rm(list=ls())
library(modelsummary)
library(tidyverse)

# Call data
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
penguins <- read.csv(url)


##
# 1) datasummary_skim
#  1a) default: select only numerics
datasummary_skim(penguins)
#  same as:
datasummary_skim(penguins,"numeric")
#  1b) only chategoricals (characters) -> only nobs and frequencies for each
datasummary_skim(penguins,"categorical")
#  1c) can mix with select command
datasummary_skim( penguins %>% select( body_mass_g, bill_length_mm ) )


##
# 2) datasummary:
#   Can select functions and passing Arguments to handle missing values
#   2a) simply get mean and sd
datasummary( body_mass_g + bill_length_mm
                ~ mean * Arguments(na.rm = TRUE) 
                  + sd * Arguments(na.rm = TRUE) , data = penguins )

#   2b) Get central tendency measures:
# need to add mode as a function
mode_ds <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
datasummary( body_mass_g + bill_length_mm
             ~ mean * Arguments(na.rm = TRUE) 
             + median * Arguments(na.rm = TRUE) 
             + mode_ds, data = penguins )

#   2c) Get Dispersion measures:
range_ds <- function( x ){ max( x , na.rm = TRUE ) - min( x , na.rm = TRUE ) }
iqr_ds <- function( x ){ IQR( x, na.rm = TRUE )[ 2 ] - IQR( x, na.rm = TRUE )[ 1 ]}
datasummary( body_mass_g + bill_length_mm
             ~ sd * Arguments(na.rm = TRUE) 
             + range_ds 
             + IQR * Arguments(na.rm = TRUE) 
             , data = penguins )

##
# 3) Subgroup analysis
#
# 3a) Means of ONE VARIABLE by ONE variable's groups
datasummary(flipper_length_mm + body_mass_g ~ mean * sex,
            data = penguins)


# 3b) Means and SD of ONE VARIABLE by ONE variable's groups
datasummary(body_mass_g ~ sex * (mean + sd),
            data = penguins)

# 3c) Means and SD of MULTIPLE VARIABLES by ONE variable's groups
datasummary(sex * (body_mass_g + flipper_length_mm) ~ mean + sd,
            data = penguins)

# 3d) Means of ONE VARIABLE by MULTIPLE variables' groups
datasummary(flipper_length_mm + body_mass_g ~ mean * island * sex,
            data = penguins)
# or by other way around for sorting into groups
datasummary(flipper_length_mm + body_mass_g ~ mean * sex * island,
            data = penguins)

# 3e) Means of MULTIPLE VARIABLES by MULTIPLE variables' groups
datasummary(flipper_length_mm + body_mass_g ~ mean * sex * island,
            data = penguins,
            sparse_header = FALSE)

## 
# 4) Renaming variables in the table

# 4a) simple example
datasummary(Heading("Flipper length (mm)") * flipper_length_mm ~ Mean + SD , data = penguins )

# 4b) similarly works if summary stats' name is different
datasummary(Heading("Flipper length (mm)") * flipper_length_mm ~
              Mean + Heading("Std. Dev.") * SD , data = penguins )

# 4c) works for more complicated cases as well
datasummary(Heading("Flipper length (mm)") * flipper_length_mm 
            + Heading("Body mass (g)") * body_mass_g ~ island * (Mean + SD),
            data = penguins)
#
# See more on: https://vincentarelbundock.github.io/modelsummary/articles/datasummary.html#datasummary-1


