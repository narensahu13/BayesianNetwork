##### Global: load libraries #####
if (!require(shiny)) {install.packages("shiny"); library(shiny)}
if (!require(shinyjs)) {install.packages("shinyjs"); library(shinyjs)}
if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if (!require(purrr)) {install.packages("purrr"); library(purrr)}
if (!require(stringr)) {install.packages("stringr"); library(stringr)}
if (!require(argonR)) {install.packages("argonR"); library(argonR)}
if (!require(argonDash)) {install.packages("argonDash"); library(argonDash)}
if (!require(data.table)) {install.packages("data.table")}
if (!require(DT)) {install.packages("DT"); library(DT)}
if (!require(shinycssloaders)) {install.packages("shinycssloaders"); library(shinycssloaders)}
if (!require(shinyWidgets)) {install.packages("shinyWidgets"); library(shinyWidgets)}
if (!require(waiter)) {install.packages("waiter"); library(waiter)}
if (!require(stringi)) {install.packages("stringi"); library(stringi)}
if (!require(shinyBS)) {install.packages("shinyBS"); library(shinyBS)}
if (!require(rintrojs)) {install.packages("rintrojs"); library(rintrojs)}
if (!require(scales)) {install.packages("scales"); library(scales)}
if (!require(htmltools)) {install.packages("htmltools"); library(htmltools)}
if (!require(htmlwidgets)) {install.packages("htmlwidgets"); library(htmlwidgets)}
if (!require(parallel)) {install.packages("parallel"); library(parallel)}
if (!require(knitr)) {install.packages("knitr"); library(knitr)}
if (!require(rmarkdown)) {install.packages("rmarkdown"); library(rmarkdown)}
if (!require(gsubfn)) {install.packages("gsubfn"); library(gsubfn)}
if (!require(shinytest)) {install.packages("shinytest"); library(shinytest)}
if (!require(shinyalert)) {install.packages("shinyalert"); library(shinyalert)}
if (!require(e1071)) {install.packages("e1071"); library(e1071)}
if (!require(backports)) {install.packages("backports"); library(backports)}
if (!require(highcharter)) {install.packages("highcharter"); library(highcharter)}

library(HydeNet)
library(bnlearn)
library(BiocManager)
library(DiagrammeR)
library(DiagrammeRsvg)
library(networkD3)
library(bs4Dash)