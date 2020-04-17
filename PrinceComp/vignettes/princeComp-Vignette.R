## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = F, warning = F------------------------------------------
library(PrinceComp)

## ----warning = F, message = F-------------------------------------------------
## Run PCA
pca <- prince(X = iris, cor = F); pca

## ----warning = F, message = F-------------------------------------------------
## Plot
plot(pca)

## ----eval = F-----------------------------------------------------------------
#  ### run SHINY app; not going to evaluate in this vignette
#  shinyPrince(X = iris)

