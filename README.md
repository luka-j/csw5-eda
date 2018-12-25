# csw5-eda

[Shiny](https://github.com/rstudio/shiny) app containing visualisations for [upis data](https://github.com/luka-j/UpisStats). (Also [CSWeek](http://csnedelja.mg.edu.rs) materials.)

Live @ <https://lukaj.shinyapps.io/csw5/>

## Installation
1. Clone this repo (`git clone https://github.com/luka-j/csw5-eda.git`)
1. Download latest data.zip from [releases](https://github.com/luka-j/csw5-eda/releases) and put it in 'app' directory 
1. Install R (`sudo apt install r-base` or `sudo zypper in R-base` or equivalent)
2. Run R (using IDE like RStudio, or just `R`)
3. Install shiny and other packages used by the app (`install.packages(c("shiny", "readr", "ggplot2", "dplyr", "magrittr", "modelr"))`) _(this might take a while)_
4. Load shiny (`library(shiny)`)
5. Check working directory is in the root of this repo (`setwd("path-to-repo")`)
6. Run app (`runApp("app")`)
7. After loading, the app should open in your default browser!

## Screenshot
![Screenshot](https://i.imgur.com/YwgQcZB.png)
