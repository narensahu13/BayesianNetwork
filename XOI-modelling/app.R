##### Global: options #####
rm(list=ls())
Production = T 
options(scipen = 1000, expressions = 10000)
appVersion = "v2.0"
appName = "Exposure Based Modelling Platform"
appLongName = "Exposure Based Modelling Platform"
lastUpdate = Sys.Date()
options(repos = c(getOption("repos"), BiocManager::repositories()))
loader <- tagList(
  waiter::spin_loaders(42),
  br(),
  h3("Loading app")
)

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)
##### User interface #####
ui <- tagList( # dependencies
  waiter::use_waiter(),
  shinyWidgets::useSweetAlert(),
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  rintrojs::introjsUI(),
  extendShinyjs(text = jsToggleFS,functions =  c("foo", "bar")),
  waiter::waiter_show_on_load(loader, color = "#000"),
# shows before anything else
  ##### CSS and style functions #####
  CSS, #CSS.R
  # Loading message
  argonDash::argonDashPage(
    title = appLongName,
    header = argonDash::argonDashHeader(
      gradient = T,
      color = NULL,
      top_padding = 2,
      bottom_padding = 0,
      background_img = "bg.jpg",
      height = 70,
      argonR::argonRow(
        argonR::argonColumn(width = 8,
                    div(style="display:inline-block;vertical-align:top;",img(src="ey-logo-black.jpg", width=92
                                                                             )), 
                    div(style="display:inline-block;vertical-align:bottom;",h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:40px;')
                    )),
        argonR::argonColumn(
          width = 4,
          h6(HTML(paste0("Creator & Maintainer: <a href='https://www.ey.com' target = '_blank'>Narendra Sahu</a>")), style = 'color:white;
                                  text-align: right;
                                  font-size:15px;
                                  margin-bottom: 0em')
        ),
        shiny::fixedPanel(
          div(
            shinyWidgets::actionBttn("fullScreen",
                       style = "material-circle",
                       icon = icon("arrows-alt"),
                       size = "xs",
                       color = "warning"),
            shinyBS::bsPopover("fullScreen", title = NULL, content = "Click to view in full screen", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "shinyjs.toggleFullScreen();"
          ),
          top = 55,
          right = 40
          
        ),
        shiny::fixedPanel(
          div(
            shinyWidgets::actionBttn("kofi",
                       style = "material-circle",
                       icon = icon("coffee"),
                       size = "xs",
                       color = "success"),
            shinyBS::bsPopover("kofi", title = NULL, content = "Contribute to the project", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('mailto:narendra.sahu@in.ey.com', '_blank')"
          ),
          top = 55,
          right = 70
          
        ),
        shiny::fixedPanel(
          div(
            shinyWidgets::actionBttn("userGuide",
                       style = "material-circle",
                       icon = icon("address-card"),
                       size = "xs",
                       color = "royal"),
            shinyBS::bsPopover("userGuide", title = NULL, content = "About EY", placement = "left", trigger = "hover",
                      options = NULL),
            onclick = "window.open('https://ey.com', '_blank')"
          ),
          top = 55,
          right = 100
          
        )
                    )
      
      
      ),
    sidebar = NULL,
    body = argonDash::argonDashBody(
      tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
      tags$br(),
           dashboardUI
    )
  )
  )

##### server #####
server <- function(input, output, session) {
  printLogJs = function(x, ...) {
    logjs(x)
    T
  }
  # addHandler(printLogJs)
  if (!Production) options(shiny.error = recover)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
    # q("no")
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  # Hide the loading message when the rest of the server function has executed
  waiter::waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)