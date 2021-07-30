####  Packages  ####
library(dplyr)
library(lubridate)
library(shiny)
library(markdown)
library(shinydashboard)
library(shinyjs)
library(waiter)
library(magick)
library(shinyalert)
library(stringr)
library(shinydisconnect)
library(tippy)
library(httr)
library(shinyWidgets)
library(googledrive)
library(googlesheets4)
library(purrr)
library(jsonlite)
library(readr)

#------------- Read in info ----------------
project_info <- readr::read_csv("./ui/project_info.csv") 
camera_info <- readr::read_csv("./ui/camera_info.csv") %>% 
  filter(use == T)
badges_info = NULL


####  Google Auth  ####

# Keys for Google Auth
source("./keys/google_keys.R") 

# load google authentications
folder_ID <- Sys.getenv("GOOGLE_FOLDER_ID")
sheets_ID <- Sys.getenv("GOOGLE_SHEET_ID")

googledrive::drive_auth(path = "./keys/google_key.json")
googlesheets4::gs4_auth(token = googledrive::drive_token())

# Create temp directory for storing pictures
tmp_dir <- tempdir()

#------- camera list --------------------

# Lat and Long aren't currently in use but exist in the csv for later mapping
# Create layout info for UI

panel_data <- tibble("panels" = 1:length(camera_info$camera_name)) %>% 
  mutate("rows" = ceiling(panels/2),
         "position" = c(0, abs(diff(rows)-1)))

button_classes <- project_info %>% filter(variable == "button_classes") %>% pull(value) %>% stringr::str_split(.,pattern=",") %>% unlist()

## 1. Load Model ---------------------------------------------------------------------
use_model <- project_info %>% filter(variable == "use_model") %>% pull(value) %>% as.logical()

if(use_model){
# Best model. 4 class classification model
  library(keras)
  badges_info <- readr::read_csv("./ui/badges.csv") 
  model <- keras::load_model_tf(paste0("./models/",project_info %>% filter(variable == "model") %>% pull(value)))
}

## 2. Functions to load NCDOT Images ---------------------------------------------------------------------

get_traffic_cam <- function(camera_name){
  
  URL <- camera_info$url[camera_info$camera_name == camera_name] 
  
  # retrieve the image
  pic <- magick::image_read(URL)
  time <-  Sys.time() %>% lubridate::with_tz("UTC")
  
  # write the image to temporary file. This will be handy for Shiny where renderImage requires an "outfile".
  magick::image_write(pic, path = paste0(tmp_dir,"/",camera_name,'.jpg'), format = "jpg")
  
  return(time)
}

# Download pictures on initilization
walk(.x = camera_info$camera_name, .f = get_traffic_cam)


write_traffic_cam <- function(camera_name, cam_time) {
  suppressMessages(googledrive::drive_upload(
    media =  paste0(tmp_dir,"/",camera_name,'.jpg'),
    path = as_id(folder_ID),
    name =  paste0(camera_name, "_", cam_time, ".jpg")
  ))
}

## 3. Functions to classify Images ---------------------------------------------------------------------

rescale <- function(dat, mn, mx){
  m = min(dat)
  M = max(dat)
  
  z <- ((mx-mn)*(dat-m))/((M-m)+mn)
  return(z)
}

standardize <- function(img) {
  s = sd(img)
  m = mean(img)
  img = (img - m) / s
  
  img =rescale(img, 0, 1)
  
  rm(s, m)
  
  return(img)
}

# Function to Apply to Each Camera
get_cam <- function(cam_name){
  get_traffic_cam(cam_name)
}

time_reactive_list <- reactiveValues()

walk(.x = camera_info$camera_name, .f = function(.x){
  time_reactive_list[[paste0(tolower(.x),"_time_reactive")]] <- get_cam(.x)
})

if(use_model){
  predict_model <- function(camera_name){
    
    # Reshape to correct dimensions (1, 224, 224, 3)
    img_array <- keras::image_load(paste0(tmp_dir,"/",camera_name,'.jpg'),
                                   target_size = c(224,224)) %>% 
      keras::image_to_array() %>% 
      standardize() %>%
      keras::array_reshape(., c(1, dim(.)))
    
    # Model prediction
    prediction <- model %>% 
      predict(x = img_array) %>% 
      t()
    
    colnames(prediction) <- "prob"
    
    prediction <- prediction %>% 
      as_tibble() %>% 
      transmute(prob = round(prob, 2),
                label = project_info %>% filter(variable == "model_classes") %>% pull(value) %>% stringr::str_split(.,pattern=",") %>% unlist()) %>% 
      filter(prob == max(prob, na.rm=T)) %>% 
      slice(1)
    
    prediction
  }
  
  get_prediction <-  function(cam_name){
    predict_model(cam_name)
  }
  
  predict_reactive_list <- reactiveValues()
  
  walk(.x = camera_info$camera_name, .f = function(.x){
    predict_reactive_list[[paste0(tolower(.x),"_predict_reactive")]] <- get_prediction(.x)
    
  })
}

waiting_screen <- tagList(
  spin_wave(),
  h4(paste0("Loading ", project_info %>% filter(variable == "title") %>% pull(value)))
)

# some javascript to make sidebar automatically go away after hitting tab name while on mobile

jsCode <- "shinyjs.init = function() {
  $(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia('only screen and (max-width: 768px)').matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});
}"

####____________________________________####
#------------------------ Define UI ---------------------------------------
ui <- dashboardPage(
  title = project_info %>% filter(variable == "title") %>% pull(value), 
  skin = "black",
  
  
  #####  Header  ####
  header = dashboardHeader(
    title =  p(project_info %>% filter(variable == "title") %>% pull(value), style="color:white;"),
    titleWidth = 350,
    tags$li(class = "dropdown", 
            actionButton(inputId = "submit", label = "SUBMIT ASSESSMENT", class = "btn btn-success", style="color:white;font-size:12pt,font-weight:bold;"),
            style="margin:8px 20px 8px 0px;"
    )
  ),
  
  
  #####  Sidebar  ####
  sidebar = dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "nav",
      
      #####_ Models  ####
      menuItem("Cameras", tabName = "Cameras", icon = icon("camera-retro")),
      
      conditionalPanel(
        condition = "input.nav === 'Cameras'",
        div(style= "border-left-style: solid; 
                    border-left-width: medium; 
                    border-left-color: white;
                    overflow-wrap: anywhere;
                    padding: 1px 20px;",
            includeMarkdown("./text/cameras.md"),
            br()
        )
      ), 
      
      
      # ------------ _About the project -----------
      menuItem("About the Project", tabName = "About", icon = icon("info-circle")),
      
      if(use_model){
        menuItem("The Model", tabName = "Model", icon = icon("robot"))
      },
      menuItem("Contact Us", tabName = "Contact", icon = icon("envelope"))
      
    )
  ),
  
  #####  Dashboard Body  ####
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    fluidPage(
      disconnectMessage(
        text = "Your session has timed out! Try refreshing the page.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#000000",##000000
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.25,
        width = 450,
        top = "center",
        size = 24,
        css = ""),
      shinyjs::useShinyjs(),
      extendShinyjs(text = jsCode, functions = c()),
      useShinyalert(),
      use_waiter(),
      # waiter::waiter_show_on_load(html = waiting_screen, color = "#222d32"),
      waiter::waiter_preloader(html = waiting_screen, color = "#222d32"),
      tags$head(
        tags$link(rel = "shortcut icon", href = project_info %>% filter(variable == "logo_url") %>% pull(value)),
        tags$style(HTML('
        .skin-black .main-header .logo {
          background-color: #000000;
          border-right: 1px solid #000000;
        }
        .skin-black .main-header .logo:hover {
          background-color: #000000;
        }
        
        .skin-black .main-header .navbar {
          background-color: #000000;
        }
        
        .skin-black .main-header .navbar>.sidebar-toggle {
          color: #FFFFFF;
          border-right: 1px solid #000000;
        }
        
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          color: #fff;
          background: #000;
        }
        
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: black;
        }
        
        .main-sidebar .user-panel, .sidebar-menu, .sidebar-menu>li.header {
          white-space: normal;
          overflow: hidden;
        }
        
        .content {
          padding: 5px;
        }
        
        body.skin-black.fixed {
          overflow-y: auto;
        }

        a {
        color: #5dbeff;
          font-weight: bold;
        }

        a:hover {
        color: #5dbeff;
            text-decoration: underline;
        }

        .skin-black .sidebar a {
          font-weight: bold;
          color:#5dbeff;
        }

        .skin-black .sidebar a:hover {
          font-weight: bold;
          color:#5dbeff;
          text-decoration: underline;

        }

        .skin-black .sidebar-menu>li>a {
            color: white;
            border-left: 3px solid transparent;
        }
        
        .badge {
          font-size:14px;
        }

        .footer-div-body {
            position:relative;
            bottom:0;
            right:0;
            left:0;
            background:#ecf0f5;
            padding:10px;
            z-index: 1000; 
            text-align:center;
        }
        

      '))),
      
      ##### Tab Items  ####
      tabItems(
        
        
        ###### Model ####
        tabItem(tabName = "Cameras",
                fluidRow(
                  div(
                    style = "background-color: #ffffff;
                    border-radius: 10px;
                    margin: 0 15px;
                    overflow-y: auto;
                    display: inline-block;",
                    align = "center",
                  ######_ Prediction Key  ####
                        column(width=6,
                               
                           h2(project_info %>% filter(variable == "subtitle") %>% pull(value)),
                           h5(project_info %>% filter(variable == "subtitle_description") %>% pull(value)),
                           uiOutput(outputId = "badges"),
                           br()
                        ),
                    column(width=6,
                           div(style="padding-top:25px",
                           uiOutput(outputId = "description")
                           )
                           )
                  )
                  ),
                
                
                ######_ Cams  ####
                uiOutput(outputId = "picture_panel")
        ),
        
        # ------------- About --------------
        tabItem(tabName = "About",
                
                fluidRow(column(
                  width = 12,
                  div(
                    style = "background-color: #ffffff;
                      padding: 10px;
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                    # height=300,
                    align  = "left",
                    includeMarkdown("./text/about_project.md")
                  )
                ))
        ),
        tabItem(tabName = "Model",
                
                fluidRow(column(
                  width = 12,
                  div(
                    style = "background-color: #ffffff;
                      padding: 10px;
                      
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                    # height=300,
                    align  = "left",
                    includeMarkdown("text/about_ML.md")
                  )
                ))
        ),

        tabItem(tabName = "Contact",
                
                fluidRow(column(
                  width = 12,
                  div(
                    style = "background-color: #ffffff;
                      padding: 10px;
                      
                      border-radius: 10px;
                      margin: 10px 0;
                      overflow-y: auto;
                      display: inline-block;
                      width:100%;",
                    align  = "left",
                    includeMarkdown("text/contact_us.md")
                  )
                ))
        )
      ),
      div(class = "footer-div-body",
          span(style="text-align:center;",p(paste0("Copyright © ",format(Sys.Date(), "%Y")," ",project_info %>% filter(variable == "organization") %>% pull(value),". Built with"), style= "color:black;display:inline;"),a("CamML", href = "https://floodcamml.github.io", style="color:#007eff;"))
      )
    )))









####_______________________________####
####  Server  ####

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Popup on load to display info
  shinyalert(title = "",
             html = T,
             text = includeMarkdown("./text/landing_text.md"),
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonText = "OK",
             animation=F,
             size = "s",
             inputId = "splash_page", 
             closeOnEsc = T)
  
  #---------------- badges render --------------------
  output$badges <- renderUI({
    req(badges_info)
    
    badge_pieces <- c()
    
    for(i in 1:nrow(badges_info)){
      badge_pieces[[i]] <- tippy::tippy(shiny::span(class="badge",badges_info$value[i],style=paste0("background-color:",badges_info$color[i],";")),shiny::h5(badges_info$description[i]))
    }
    
    h4(badge_pieces)
  })
  
  #---------------- description render ---------------
  output$description <- renderUI({
    HTML(project_info %>% filter(variable == "description") %>% pull(value)) 
  })
  
  #---------------- picture panel render ---------------
  output$picture_panel <- renderUI({
    ui_pieces <- c()
    
    for(i in 1:length(unique(panel_data$rows))){
      numbers <- panel_data %>% 
        filter(rows == i) %>% 
        pull(panels)
      
      if(nrow(panel_data %>% filter(rows == i)) == 2){
        ui_pieces[[i]] <- fluidRow(
          column(width=6,
                 uiOutput(outputId = paste0(tolower(camera_info$camera_name)[numbers[1]],"_selection"))),
          column(width=6,
                 uiOutput(outputId = paste0(tolower(camera_info$camera_name)[numbers[2]],"_selection")))
        )
      }
      
      if(nrow(panel_data %>% filter(rows == i)) == 1){
        ui_pieces[[i]] <- fluidRow(
          column(width=6,
                 uiOutput(outputId = paste0(tolower(camera_info$camera_name)[numbers[1]],"_selection")))
        )
      }
    }
    
    ui_pieces
    
  })
  
  #-------------- Link to About section --------------
  observeEvent(input$to_about_section, {
    
    updateTabItems(session = session, 
                   inputId = "nav", 
                   selected = "About")
    
  })
  
  observeEvent(input$nav,
               {
                 req(input$is_mobile_device == T)
                 # for desktop browsers
                 addClass(selector = "body", class = "sidebar-collapse")
                 # for mobile browsers
                 removeClass(selector = "body", class = "sidebar-open")
               })
  
  #-------------- Reactive Value Holders -------------
  # These capture user inputs for later
  
  # feedback on model 1
  button_info_model1 <- reactiveValues()
  
  
  ####____________________________####
  ####__  Supervised Model Displays __####
  
  #--------------- Get Cam Images ----------------------
  

  # Get Traffic Cam Images

  walk(.x = camera_info$camera_name, .f = function(.x){
    time_reactive_list[[paste0(tolower(.x),"_time_reactive")]] <- get_cam(.x)
  })
  
  if(use_model){
    walk(.x = camera_info$camera_name, .f = function(.x){
      predict_reactive_list[[paste0(tolower(.x),"_predict_reactive")]] <- get_prediction(.x)
    })
  }
  
  #--------------- Display Camera Feeds ----------------------
  
  # 1. Build UI for Camera Image Displays
  
  # Function to apply to each
  render_cam_image <- function(cam_name, alt_name){
    out_image <- renderImage({
      outfile <- paste0(tmp_dir,"/",cam_name,'.jpg')
      list(src = outfile,
           alt = alt_name,
           width = "100%"#, height="180px"
      )
    }, deleteFile=F)
    
    return(out_image)
  }
  
  # Run Each Camera
  
  walk(.x = camera_info$camera_name, .f = function(.x){
    output[[paste0(tolower(.x),"_picture")]] <- render_cam_image(cam_name = .x,
                                                                 alt_name = .x)
  })
  
  #--------------- Camera Feedback UI ----------------------
  
  # 2. Display for image box / model classification
  
  # Function to apply to each
  # takes the camera name, the reactive time, and the model predictions
  render_camera_ui_model <- function(cam_name, cam_time, model_prediction, tzone = project_info %>% filter(variable == "tzone") %>% pull(value), tzone_alias = project_info %>% filter(variable == "tzone_alias") %>% pull(value),id_suffix = ""){
    model_predict_info <- model_prediction
    
    model_prediction_val <- model_predict_info$prob * 100
    model_prediction_class <- model_predict_info$label
    cam_time_val <- cam_time
    lst_time <- format(cam_time_val %>% lubridate::with_tz(tzone), "%m/%d/%Y %H:%M")
    
    # string prep for naming patterns for UI elements
    # option to add suffix for "_unsupervised" ui elements
    name_lcase <- tolower(cam_name)
    img_output_id <- str_c(name_lcase, "_picture", id_suffix)
    radio_button_id <- str_c(name_lcase, "_button_select", id_suffix)
    button_clear <- str_c(name_lcase, "_clear", id_suffix)
    
    camera_button_ui <- renderUI({
      div(width="100%",
          style="background-color: #ffffff;
            padding: 10px;
            border-radius: 10px;
            margin: 10px 0px;",
          align  = "center",
          div(style="display:inline-block",
              h2(gsub("([a-z])([A-Z])", "\\1 \\2", cam_name))),
          div(style="display:inline-block",
              
              # Edit Badges above pictures created using model output
              span(class="badge",badges_info %>% 
                     filter(value == model_prediction_class) %>% 
                     pull(value),
                   style=paste0("background-color:",badges_info %>% 
                                  filter(value == model_prediction_class) %>% 
                                  pull(color),";position: relative; bottom: 5px; color:white;"))

          ),
          
          # Display Cam Image
          imageOutput(img_output_id,
                      height="100%"),
          
          # Datetime for image
          p(paste0("ML probability of ", model_prediction_class,": ", model_prediction_val,"%")),
          p(paste0("Time: ", lst_time," ", tzone_alias)),
          
          # Inline boxes for user feedback
          div(style="display:inline-block",
              
              # Edit choiceNames and choiceValues for buttons under pictures for labelling
              shinyWidgets::radioGroupButtons(inputId = radio_button_id,
                                              choiceNames = button_classes,
                                              choiceValues = button_classes,
                                              direction = "horizontal",
                                              width = '100%' ,
                                              individual = F,
                                              selected = character(0)
              )
          ),
          
          # clear selection button
          div(style="display:inline-block",
              actionButton(inputId = button_clear,
                           label = "Clear",
                           class = "btn btn-primary",
                           style = "font-size:10pt;color:white")
          )
      )
      
    })
    
    #return the UI
    return(camera_button_ui)
  }
  
  render_camera_ui_no_model <- function(cam_name, cam_time, tzone = project_info %>% filter(variable == "tzone") %>% pull(value), tzone_alias = project_info %>% filter(variable == "tzone_alias") %>% pull(value),id_suffix = ""){

    cam_time_val <- cam_time
    lst_time <- format(cam_time_val %>% lubridate::with_tz(tzone), "%m/%d/%Y %H:%M")
    
    # string prep for naming patterns for UI elements
    # option to add suffix for "_unsupervised" ui elements
    name_lcase <- tolower(cam_name)
    img_output_id <- str_c(name_lcase, "_picture", id_suffix)
    radio_button_id <- str_c(name_lcase, "_button_select", id_suffix)
    button_clear <- str_c(name_lcase, "_clear", id_suffix)
    
    camera_button_ui <- renderUI({
      div(width="100%",
          style="background-color: #ffffff;
            padding: 10px;
            border-radius: 10px;
            margin: 10px 0px;",
          align  = "center",
          div(style="display:inline-block",
              h2(gsub("([a-z])([A-Z])", "\\1 \\2", cam_name))),

          # Display Cam Image
          imageOutput(img_output_id,
                      height="100%"),
          
          # Datetime for image
          p(paste0("Time: ", lst_time," ", tzone_alias)),
          
          # Inline boxes for user feedback
          div(style="display:inline-block",
              
              # Edit choiceNames and choiceValues for buttons under pictures for labelling
              shinyWidgets::radioGroupButtons(inputId = radio_button_id,
                                              choiceNames = button_classes,
                                              choiceValues = button_classes,
                                              direction = "horizontal",
                                              width = '100%' ,
                                              individual = F,
                                              selected = character(0)
              )
          ),
          
          # clear selection button
          div(style="display:inline-block",
              actionButton(inputId = button_clear,
                           label = "Clear",
                           class = "btn btn-primary",
                           style = "font-size:10pt;color:white")
          )
      )
      
    })
    
    #return the UI
    return(camera_button_ui)
  }
  
  if(use_model){
    observe({
      walk(.x = camera_info$camera_name, .f = function(.x){
        output[[paste0(tolower(.x), "_selection")]] <- render_camera_ui_model(
          cam_name = .x,
          cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
          model_prediction = predict_reactive_list[[paste0(tolower(.x), "_predict_reactive")]]
        )
      })
    })
  }
  
  if(!use_model){
    observe({
      walk(.x = camera_info$camera_name, .f = function(.x){
        output[[paste0(tolower(.x), "_selection")]] <- render_camera_ui_no_model(
          cam_name = .x,
          cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
        )
      })
    })
  }
  
  
  ####____________________________####
  ####__  User Data Collection  __####
  
  
  #------------------ Reactive reset buttons ----------------
  
  #####__ 1. Reset supervised buttons  ####
  
  # Edit button choiceNames and choiceValues
  walk(.x = camera_info$camera_name, .f = function(.x){
    observeEvent(input[[paste0(tolower(.x),"_clear")]],{
      updateRadioGroupButtons(session = session,
                              inputId = paste0(tolower(.x),"_button_select"),
                              choiceNames  = button_classes, 
                              choiceValues = button_classes, 
                              selected = character(0) 
      )
    })
  })
  
  
  ###########  Reactive Button Info #######################
  walk(.x = camera_info$camera_name, .f = function(.x){
    observeEvent(c(input[[paste0(tolower(.x),"_button_select")]], input[[paste0(tolower(.x),"_clear")]]),{
      button_info_model1[[paste0(tolower(.x),"_button_info")]] <- input[[paste0(tolower(.x),"_button_select")]]
    })
  })
  
  
#------------------- Submit button for model 1 -------------------
  
  # 1. Observe the user submission
  observeEvent(input$submit,{
    
    shinyalert(
      inputId = "shinyalert",
      title = "Submit?",
      text = "Are you ready to submit your answers?",
      size = "s",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "warning",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#AEDEF4",
      cancelButtonText = "No",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  # 2. Put user data into table, push to google sheets:
  # Final submission for model 1 (tab 1)
  observeEvent(input$shinyalert == T,{
    req(input$shinyalert)
    
    shinyalert(
      title = "Submitting responses...",
      inputId = "submitting_alert",
      showConfirmButton =F,
      showCancelButton = F,
      closeOnEsc = F,
      animation = T,
      text = "Please do not close the page or click 'Back'"
    )
    
    updateActionButton(session = session,
                       inputId = "submit",
                       label = "SUBMITTED!", 
                       icon = icon("ok", lib = "glyphicon"))
    
    # disables submit button
    shinyjs::disable("submit")
    
    
    ######  Supervised Model Feedback  ####
    
    # Function to pull relevant camera data from models and feedback
    store_cam_data_model <- function(cam_name, cam_time, model_prediction, button_response){
      cam_data <- tibble(
        "date"          = c(cam_time),
        "location"      = c(cam_name),
        "filename"      = str_c(cam_name,"_",cam_time,".jpg"), 
        "model_score"   = model_prediction$prob,
        "model_class"   = model_prediction$label,
        "user_response" = ifelse(is.null(button_response), NA, button_response)
      )
    }
    
    store_cam_data_no_model <- function(cam_name, cam_time,  button_response){
      cam_data <- tibble(
        "date"          = c(cam_time),
        "location"      = c(cam_name),
        "filename"      = str_c(cam_name,"_",cam_time,".jpg"), 
        "model_score"   = NA,
        "model_class"   = NA,
        "user_response" = ifelse(is.null(button_response), NA, button_response)
      )
    }
    
    # Create reactive list to hold all of user and model data
    data_reactive_list <- reactiveValues()
    
    if(use_model){
      walk(
        .x = camera_info$camera_name,
        .f = function(.x) {
          data_reactive_list[[paste0(tolower(.x), "_data")]] <-
            store_cam_data_model(
              cam_name = .x,
              cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
              model_prediction = predict_reactive_list[[paste0(tolower(.x), "_predict_reactive")]],
              button_response = button_info_model1[[paste0(tolower(.x), "_button_info")]]
            )
          
        }
      )
    }
    
    if(!use_model){
      walk(
        .x = camera_info$camera_name,
        .f = function(.x) {
          data_reactive_list[[paste0(tolower(.x), "_data")]] <-
            store_cam_data_no_model(
              cam_name = .x,
              cam_time = time_reactive_list[[paste0(tolower(.x), "_time_reactive")]],
              button_response = button_info_model1[[paste0(tolower(.x), "_button_info")]]
            )
          
        }
      )
    }
    
    
    # Join tibbles of user and model data into one tibble
    data <- map_dfr(reactiveValuesToList(data_reactive_list), bind_rows)
    
    # Append data to google sheet
    suppressMessages(googlesheets4::sheet_append(ss = sheets_ID,
                                                 data = data))
    
    # Write pictures to Google Drive
    purrr::map2(data$location, data$date, write_traffic_cam)
    
    shinyalert(
      inputId = "submitted_alert",
      title = "Submitted!",
      type = "success",
      immediate = T,
      animation = T,
      text = project_info %>% filter(variable == "submit_success") %>% pull(value)
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
