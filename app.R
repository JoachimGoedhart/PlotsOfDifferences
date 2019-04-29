# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotsOfDifferences: Shiny app for plotting the data and DIFFERENCES (aka effect size)
# Created by Joachim Goedhart (@joachimgoedhart), first version september 2018
# Takes non-tidy, spreadsheet type data as input or tidy format
# Non-tidy data is converted into tidy format
# For tidy data the x and y variables need to be selected
# Raw data is displayed with user-defined visibility (alpha)
# Summary statistics are displayed with user-defined visibility (alpha)
# Inferential statistics (95%CI) can be added
# The 95%CI of the median is determined by resampling (bootstrap)
# The differences (effect sizes) are determined from bootstrap samples of either mean or median
# A plot and a table with stats are generated
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Copyright (C) 2018  Joachim Goedhart
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ToDo
# Implement fold-change as an effect size

##### Define dependencies ######

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)
library(readxl)
library(DT)
library(RCurl)
library(gridExtra)
library(shinycssloaders)

source("geom_flat_violin.R")

######## Increase maximum upload size to 30 MB #######
options(shiny.maxRequestSize=30*1024^2)


###### Functions ##########

#Function that resamples a vector (with replacement) and calculates the median value
boot_median = function(x) {
  median(sample(x, replace = TRUE))
}

#Function that resamples a vector (with replacement) and calculates the mean value
boot_mean = function(x) {
  mean(sample(x, replace = TRUE))
}

i=0
#Number of bootstrap samples
nsteps=1000

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

alpha=1-Confidence_level
lower_percentile=(1-Confidence_level)/2
upper_percentile=1-((1-Confidence_level)/2)


#Several qualitative color palettes that are colorblind friendly
#From Paul Tol: https://personal.sron.nl/~pault/
#Code to generate vectors in R to use these palettes

#Red, Green, Blue, yellow, cyan, purple, grey
Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255', '#AA4499', '#332288', '#DDDDDD')
Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')


#Read a text file (comma separated values)
df_wide_example <- read.csv("Area_in_um-GEFs.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")

###### UI: User interface #########

ui <- fluidPage(
  titlePanel("PlotsOfDifferences - Plots all Of the Data and Differences"),
  sidebarLayout(
    sidebarPanel(width=3,
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        radioButtons("jitter_type", "Data offset", choices = list("Quasirandom" = "quasirandom", 
                                                                  #Uncomment for sinaplot                                           "Sinaplot" = "sina", 
                                                                  "Random" = "random", 
                                                                  "None; stripes" = "stripes",
                                                                  "None (for small n)" = "none"), selected = "quasirandom"),

        
        sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),

        # conditionalPanel(
        #   condition = "input.adjust_jitter == true",
        #   sliderInput("jitter_width", "Width:", 0,0.5,0.3),
        #   checkboxInput(inputId = "random_jitter", label = ("Randomize Jitter"), value = TRUE)
        # ),
          
        radioButtons("summaryInput", "Statistics", choices = list("Median" = "median", "Mean" = "mean", "Boxplot (minimal n=10)" = "boxplot", "Violin Plot (minimal n=10)" = "violin"), selected = "median"),
#        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
        checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (minimal n=10)"), value = FALSE),
        sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),

        radioButtons(inputId = "ordered",
             label= "Order of the data/statistics:",
             choices = list("As supplied" = "none", "By median value" = "median", "By alphabet/number" = "alphabet"),
             selected = "none"),
 

        checkboxInput(inputId = "show_diffs",
              label = "Display Effect Size",
              value = TRUE),

        conditionalPanel(
                          condition = "input.show_diffs == true",
  
       selectInput("zero", "Set reference (Control):", choices = "")),

        h4("Plot Layout"),      

        checkboxInput(inputId = "rotate_plot",
              label = "Rotate plot 90 degrees",
              value = FALSE),

        checkboxInput(inputId = "no_grid",
                      label = "Remove gridlines",
                      value = FALSE),

          checkboxInput(inputId = "change_scale",
                        label = "Change scale",
                        value = FALSE),
          conditionalPanel(condition = "input.change_scale == true",
                           checkboxInput(inputId = "scale_log_10",
                                         label = "Log scale",
                                         value = FALSE),
                 
                 textInput("range", "Range of values (min,max)", value = "")),
        
                checkboxInput("color_data", "Use color for the data", value=FALSE),
        checkboxInput("color_stats", "Use color for the stats", value=FALSE),

        conditionalPanel(
            condition = "input.color_data == true || input.color_stats == true",
            ########## Choose color from list
            #selectInput("colour_list", "Colour:", choices = ""),

          radioButtons("adjustcolors", "Color palette:", choices = list("Standard" = 1,"Colorblind safe (bright)" = 2,"Colorblind safe (muted)" = 3,"Colorblind safe (light)" = 4, "User defined"=5) , selected =  1),
              conditionalPanel(condition = "input.adjustcolors == 5",
                 textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen"), 
                 
                 h5("",
                    a("Click here for more info on color names",
                      href = "http://www.endmemo.com/program/R/color.php", target="_blank"))
                 
        )),

        numericInput("plot_height", "Height (# pixels): ", value = 480),
        numericInput("plot_width", "Width (# pixels):", value = 480),

        h4("Labels"),

        checkboxInput(inputId = "add_title",
                        label = "Add title",
                        value = FALSE),
        conditionalPanel(
        condition = "input.add_title == true",
        textInput("title", "Title:", value = "")
        ),

        checkboxInput(inputId = "label_axes",
              label = "Change labels",
              value = FALSE),
        conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = ""),
              textInput("lab_y", "Y-axis:", value = "")
        ),
        checkboxInput(inputId = "adj_fnt_sz",
              label = "Change font size",
              value = FALSE),
       conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
              numericInput("fnt_sz_ax", "Size axis labels:", value = 18)),
        checkboxInput(inputId = "add_description",
                      label = "Add figure description",
                      value = FALSE),
        NULL      ),
conditionalPanel(
  condition = "input.tabs=='Data upload'",
  h4("Data upload"),
  radioButtons(
    "data_input", "",
    choices = 
      list("Example 1 (wide format)" = 1,
           "Example 2 (tidy format)" = 2,
           "Upload file" = 3,
           "Paste data" = 4,
           "URL (csv files only)" = 5
      )
    ,
    selected =  1),
  conditionalPanel(
    condition = "input.data_input=='1'"
    
  ),
  conditionalPanel(
    condition = "input.data_input=='3'",
    h5("Upload file: "),
    fileInput("upload", "", multiple = FALSE),
    selectInput("file_type", "Type of file:",
                list("text (csv)" = "text",
                     "Excel" = "Excel"
                ),
                selected = "text"),
    conditionalPanel(
      condition = "input.file_type=='text'",
      
      radioButtons(
        "upload_delim", "Delimiter",
        choices = 
          list("Comma" = ",",
               "Tab" = "\t",
               "Semicolon" = ";",
               "Space" = " "),
        selected = ",")),
    
    actionButton("submit_datafile_button",
                 "Submit datafile")),
  conditionalPanel(
    condition = "input.data_input=='4'",
    h5("Paste data below:"),
    tags$textarea(id = "data_paste",
                  placeholder = "Add data here",
                  rows = 10,
                  cols = 20, ""),
    actionButton("submit_data_button", "Submit data"),
    radioButtons(
      "text_delim", "Delimiter",
      choices = 
        list("Tab (from Excel)" = "\t",
             "Space" = " ",
             "Comma" = ",",
             "Semicolon" = ";"),
      selected = "\t")),
  
  ### csv via URL as input      
  conditionalPanel(
    condition = "input.data_input=='5'",
    #         textInput("URL", "URL", value = "https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"), 
    textInput("URL", "URL", value = ""), 
    NULL
  ),
  
  checkboxInput(inputId = "tidyInput",
                label = "These data are Tidy",
                value = FALSE),
  
  conditionalPanel(
    condition = "input.tidyInput==false", selectInput("data_remove", "Select columns to remove", "", multiple = TRUE)),
  
  conditionalPanel(
    condition = "input.tidyInput==true",
    
    selectInput("x_var", "Conditions to compare:", choices = ""),
    selectInput("y_var", "Variables:", choices = ""),
    #         selectInput("h_facet", "Separate horizontal:", choices = ""),
    #         selectInput("v_facet", "Separate vertical:", choices = ""),
    
    NULL
  ), 
  
  conditionalPanel(
    condition = "input.tidyInput==false", (downloadButton("downloadData", "Download in tidy format (csv)"))),
    hr(),
    checkboxInput(inputId = "info_data",
                label = "Show information on data formats",
                value = FALSE),
  
  conditionalPanel(
    condition = "input.info_data==true",
    img(src = 'Data_format.png', width = '100%'), h5(""), a("Background info for converting wide data to tidy format", href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")
    )
  
    ),

      conditionalPanel(
        condition = "input.tabs=='About'",
        h4("About")    
      ),
      
      conditionalPanel(
        condition = "input.tabs=='Summary'",
        h4("Data summary"),
        checkboxInput(inputId = "calc_p",
                      label = "Display p-value",
                      value = FALSE),
        p("The p-value is determined by a randomization test, calculation may take some time"),
        numericInput("digits_diff", "Digits (Difference):", 3, min = 0, max = 7),
        numericInput("digits", "Digits (Summary):", 2, min = 0, max = 7),
        checkboxGroupInput("stats_select", label = h5("Statistics for table:"), 
                           choices = list("mean", "sd", "sem","95CI mean", "median", "MAD", "IQR", "Q1", "Q3", "95CI median"),
                           selected = "sem"),
        actionButton('select_all1','select all'),
        actionButton('deselect_all1','deselect all')
        
      )

    ),
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"), dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"),
 #                          downloadButton("downloadPlotSVG", "Download svg-file"),
                           downloadButton("downloadPlotPNG", "Download png-file"),
                           actionButton("settings_copy", icon = icon("clone"),
                                        label = "Clone current setting"),
                           actionButton("legend_copy", icon = icon("clone"),
                                        label = "Copy Legend"),
                           
                          div(`data-spy`="affix", `data-offset-top`="10", withSpinner(plotOutput("coolplot")),
                                  htmlOutput("LegendText", width="200px", inline =FALSE),
                              NULL)
                  ), 
                  tabPanel("Summary",
                           conditionalPanel(
                             condition = "input.summaryInput=='mean'",
                             h4("Summary of the differences - based on means")),
                           conditionalPanel(
                             condition = "input.summaryInput!='mean'",
                             h4("Summary of the differences - based on medians")),
                           withSpinner(dataTableOutput('data_diffs')),
                           h4("Summary of the data"),
                           withSpinner(dataTableOutput('data_summary'))

                            ),
                  tabPanel("About", includeHTML("about.html")
                           )
                  
      )
    )
  )         
)

########## SERVER ########

server <- function(input, output, session) {

  ###### DATA INPUT ###################
  
  df_upload <- reactive({
    
    if (input$data_input == 1) {
      data <- df_wide_example
    }  else if (input$data_input == 2) {
      data <- df_tidy_example 
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- read_delim(file_in$datapath,
                               delim = input$upload_delim,
                               col_names = TRUE)
          } else if (input$file_type == "Excel") {
            data <- read_excel(file_in$datapath)
          } 
        })
      }
    } else if (input$data_input == 5) {
      
      #Read data from a URL
      #This requires RCurl
      if(input$URL == "") {
        return(data.frame(x = "Enter a full HTML address, for example: https://zenodo.org/record/2545922/files/FRET-efficiency_mTq2.csv"))
      } else if (url.exists(input$URL) == FALSE) {
        return(data.frame(x = paste("Not a valid URL: ",input$URL)))
      } else {data <- read_csv(input$URL)}
      
      #Read the data from textbox
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
        }
      }
  }
    updateSelectInput(session, "data_remove", choices = names(data))
    return(data)
})
  
  
  ##### REMOVE SELECTED COLUMNS #########
  df_filtered <- reactive({     
    
    if (!is.null(input$data_remove)) {
      columns = input$data_remove
      df <- df_upload() %>% select(-one_of(columns))
    } else if (is.null(input$data_remove)) {
      df <- df_upload()}
    
  })
  
##### CONVERT TO TIDY DATA ##########
  
#Need to tidy the data?!
#Untidy data will be converted to long format with two columns named 'Condition' and 'Value'
#The input for "Condition" will be taken from the header, i.e. first row
#Tidy data will be used as supplied
df_upload_tidy <- reactive({
    if(input$tidyInput == FALSE ) {
      klaas <- df_filtered() %>% gather(Condition, Value)
    }
    else if(input$tidyInput == TRUE ) {
      klaas <- df_upload()
    }
  return(klaas)
})

##### Get the Variables (necessary for tidy data with multiple variables) ##############

observe({ 
        var_names  <- names(df_upload_tidy())
        
        var_list <- c("none", var_names)
#        updateSelectInput(session, "colour_list", choices = var_list)
        updateSelectInput(session, "y_var", choices = var_list)
        updateSelectInput(session, "x_var", choices = var_list)
        })


##### Get the list of Conditions (necessary to select the control condition for comparison) ##############

        
observe({ 
          
  ########### This is NULL for tidy - needs to be fixed ######

        if(input$tidyInput == FALSE ) {
          koos <- df_upload_tidy()
          conditions_list <- as.factor(koos$Condition)
          observe(print((conditions_list)))
          updateSelectInput(session, "zero", choices = conditions_list)
        }


        
})


########### When x_var is selected for tidy data, get the list of conditions

observeEvent(input$x_var != 'none' && input$y_var != 'none', {
  
  
  # if (input$x_var == 'none' && input$tidyInput == TRUE && input$tabs=="Plot") {
  #   showModal(modalDialog(
  #     title = NULL,
  #     "Select a row for the conditions", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
  #   ))
  # } else
    
    if (input$x_var != 'none' && input$y_var != 'none') {

    koos <- df_selected()
    conditions_list <- as.factor(koos$Condition)
    observe(print((conditions_list)))
    updateSelectInput(session, "zero", choices = conditions_list)
    } 
})



########### Check whether  conditions and values are selected for tidy data

observeEvent(input$tabs=="Plot" && input$tidyInput == TRUE, {
  


  if (input$x_var == 'none' || input$y_var == 'none') {
  showModal(modalDialog(
    title = NULL,
    "Select a row for the condition and a row for the values", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
  ))
  
  }
  
  
})







########### GET INPUT VARIABLEs FROM HTML ##############

observe({
  
  ############ ?data ################
  
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['data']])) {
    presets_data <- query[['data']]
    presets_data <- unlist(strsplit(presets_data,";"))
    observe(print((presets_data)))
    
    updateRadioButtons(session, "data_input", selected = presets_data[1])    
    updateCheckboxInput(session, "tidyInput", value = presets_data[2])
    
    #To Implement:
    #presets_data[3], x_var
    #presets_data[4], y_var
    #presets_data[5], h_facet
    #presets_data[6], v_facet
  }
  
  ############ ?vis ################
  
  if (!is.null(query[['vis']])) {
    
    presets_vis <- query[['vis']]
    presets_vis <- unlist(strsplit(presets_vis,";"))
    observe(print((presets_vis)))
    
    #radio, slider, radio, check, slider
    updateRadioButtons(session, "jitter_type", selected = presets_vis[1])
    updateSliderInput(session, "alphaInput", value = presets_vis[2])
    updateRadioButtons(session, "summaryInput", selected = presets_vis[3])
    updateCheckboxInput(session, "add_CI", value = presets_vis[4])
    updateSliderInput(session, "alphaInput_summ", value = presets_vis[5])
    updateRadioButtons(session, "ordered", selected = presets_vis[6])
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?layout ################
  
  if (!is.null(query[['layout']])) {
    
    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    observe(print((presets_layout)))
    
    updateCheckboxInput(session, "rotate_plot", value = presets_layout[1])
    updateCheckboxInput(session, "no_grid", value = (presets_layout[2]))
    
    updateCheckboxInput(session, "change_scale", value = presets_layout[3])
    updateCheckboxInput(session, "scale_log_10", value = presets_layout[4])
    updateTextInput(session, "range", value= presets_layout[5])
    updateCheckboxInput(session, "color_data", value = presets_layout[6])
    updateCheckboxInput(session, "color_stats", value = presets_layout[7])
    updateRadioButtons(session, "adjustcolors", selected = presets_layout[8])    
    updateCheckboxInput(session, "add_description", value = presets_layout[9])
    if (length(presets_layout)>10) {
      updateNumericInput(session, "plot_height", value= presets_layout[10])
      updateNumericInput(session, "plot_width", value= presets_layout[11])
    }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?color ################
  
  if (!is.null(query[['color']])) {
    
    presets_color <- query[['color']]
    presets_color <- unlist(strsplit(presets_color,";"))
    
    updateSelectInput(session, "colour_list", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= presets_color[2])
  }
  
  ############ ?label ################
  
  if (!is.null(query[['label']])) {
    
    presets_label <- query[['label']]
    presets_label <- unlist(strsplit(presets_label,";"))
    observe(print((presets_label)))
    
    
    updateCheckboxInput(session, "add_title", value = presets_label[1])
    updateTextInput(session, "title", value= presets_label[2])
    
    updateCheckboxInput(session, "label_axes", value = presets_label[3])
    updateTextInput(session, "lab_x", value= presets_label[4])
    updateTextInput(session, "lab_y", value= presets_label[5])
    
    updateCheckboxInput(session, "adj_fnt_sz", value = presets_label[6])
    updateNumericInput(session, "fnt_sz_ttl", value= presets_label[7])
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[8])
    updateCheckboxInput(session, "add_description", value = presets_label[9])
  }
  
  ############ ?url ################
  
  if (!is.null(query[['url']])) {
    updateRadioButtons(session, "data_input", selected = 5)  
    updateTextInput(session, "URL", value= query[['url']])
    observe(print((query[['url']])))
    updateTabsetPanel(session, "tabs", selected = "Plot")
  }
})

########### RENDER URL ##############

output$HTMLpreset <- renderText({
  url()
})

######### GENERATE URL with the settings #########

url <- reactive({
  
  base_URL <- paste(sep = "", session$clientData$url_protocol, "//",session$clientData$url_hostname, ":",session$clientData$url_port, session$clientData$url_pathname)
  
  data <- c(input$data_input, input$tidyInput, input$x_var, input$y_var, input$h_facet, input$v_facet)
  
  vis <- c(input$jitter_type, input$alphaInput, input$summaryInput, input$add_CI, input$alphaInput_summ, input$ordered)
  layout <- c(input$rotate_plot, input$no_grid, input$change_scale, input$scale_log_10, input$range, input$color_data, input$color_stats,
              input$adjustcolors, input$add_description, input$plot_height, input$plot_width)
  
  #Hide the standard list of colors if it is'nt used
  if (input$adjustcolors != "5") {
    color <- c(input$colour_list, "none")
  } else if (input$adjustcolors == "5") {
    color <- c(input$colour_list, input$user_color_list)
  }
  
  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_ttl, input$fnt_sz_ax, input$add_description)
  
  #replace FALSE by "" and convert to string with ; as seperator
  data <- sub("FALSE", "", data)
  data <- paste(data, collapse=";")
  data <- paste0("data=", data) 
  
  vis <- sub("FALSE", "", vis)
  vis <- paste(vis, collapse=";")
  vis <- paste0("vis=", vis) 
  
  layout <- sub("FALSE", "", layout)
  layout <- paste(layout, collapse=";")
  layout <- paste0("layout=", layout) 
  
  color <- sub("FALSE", "", color)
  color <- paste(color, collapse=";")
  color <- paste0("color=", color) 
  
  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label) 
  
  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}
  
  parameters <- paste(data, vis,layout,color,label,url, sep="&")
  
  preset_URL <- paste(base_URL, parameters, sep="?")
  
  observe(print(parameters))
  observe(print(preset_URL))  
  return(preset_URL)
})

############# Pop-up that displays the URL to 'clone' the current settings ################

observeEvent(input$settings_copy , {
  showModal(urlModal(url=url(), title = "Use the URL to launch PlotsOfDifferences with the current setting"))
})

observeEvent(input$legend_copy , {
  showModal(urlModal(url=Fig_legend(), title = "Legend text"))
})

############# Pop-up appears when a boxplot or violinplot is selected when n<10 ###########

observeEvent(input$summaryInput , {
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)
  if (input$summaryInput == "box" && min_n<10) {
    showModal(modalDialog(
      title = NULL,
      "You have selected a boxplot as summary, but one of the conditions has less than 10 datapoints - For n<10 the boxplot is not a suitable summary", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
    ))
  } else if (input$summaryInput == "violin" && min_n<10) {
    showModal(modalDialog(
      title = NULL,
      "You have selected a violinplot as summary, but one of the conditions has less than 10 datapoints - For n<10 the violinplot is not a suitable summary", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
    ))
  }
})

############# Pop-up appears when the 95%CI is selected when n<10 ###########

observeEvent(input$add_CI , {
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)
  
  if (input$add_CI == TRUE && min_n<10) {
    showModal(modalDialog(
      title = NULL,
      "Confidence Intervals are used to make inferences, but one of the conditions has less than 10 datapoints - It is not recommended to show inferential statistics (CI, sem) for n<10", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
    ))
  }  
})

############# Pop-up appears when n<10 ###########

observeEvent(input$tabs=="Plot" , {
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)

  if (input$show_diffs == TRUE && min_n<10) {
    showModal(modalDialog(
      title = NULL,
      "One of the conditions has less than 10 datapoints - calculating the effect size based on bootsrapping is not recommended", easyClose=TRUE, footer = modalButton("Click anywhere to dismiss")
    ))
    updateCheckboxInput(session, "show_diffs", value = FALSE)

  }
})

######## ORDER the Conditions ####### 
df_sorted <- reactive({
  
#  klaas <- df_upload_tidy()
klaas <-  df_selected()

   if(input$ordered == "median") {
     klaas$Condition <- reorder(klaas$Condition, klaas$Value, median, na.rm = TRUE)

   } else if (input$ordered == "none") {
      klaas$Condition <- factor(klaas$Condition, levels=unique(klaas$Condition))

   } else if (input$ordered == "alphabet") {
     klaas$Condition <- factor(klaas$Condition, levels=unique(sort(klaas$Condition)))
   }  

  return(klaas)
  
})


######## Make a new dataframe with selected variables for display & summary stats #######  

df_selected <- reactive({
    if(input$tidyInput == TRUE ) {
    df_temp <- df_upload_tidy() 
    x_choice <- input$x_var
    y_choice <- input$y_var
    
    koos <- df_temp %>% select(Condition = !!x_choice , Value = !!y_choice) %>% filter(!is.na(Value))

    } else if (input$tidyInput == FALSE ) {
      koos <- df_upload_tidy() %>% filter(!is.na(Value))
    }

    return(koos)
})

#### DISPLAY UPLOADED DATA (as provided) ##################

output$data_uploaded <- renderDataTable(
  
  #    observe({ print(input$tidyInput) })
  df_filtered(),
  rownames = FALSE,
  options = list(pageLength = 100, autoWidth = FALSE,
                 lengthMenu = c(10, 100, 1000, 10000)),
  editable = FALSE,selection = 'none'
)

########### Caluclate stats for the MEAN ############

df_summary_mean <- reactive({

  koos <- df_selected() %>%
    group_by(Condition) %>% 
    summarise(n = n(),
            mean = mean(Value),
            sd = sd(Value)) %>%
  mutate(sem = sd / sqrt(n - 1),
         mean_CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
         mean_CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
  
  return(koos)
})
  
  ############ Caluclate stats for the MEDIAN ##########
  
  df_summary_median <- reactive({
    
    klaas <- df_bootstrap_stats()
    kees  <- df_selected() %>%
    group_by(Condition) %>% 
    summarise(
            median= median(Value, na.rm = TRUE),
            MAD= mad(Value, na.rm = TRUE, constant=1),
            IQR= IQR(Value, na.rm = TRUE),
            Q1=quantile(Value, probs=0.25),
            Q3=quantile(Value, probs=0.75))

      kees$median_CI_lo <- tapply(klaas$resampled_median, klaas$Condition, quantile, probs=lower_percentile)
      kees$median_CI_hi <- tapply(klaas$resampled_median, klaas$Condition, quantile, probs=upper_percentile)
  
  return(kees)
  })

#### Caluclate boostrap samples for the Median and Mean ####

df_bootstrap_stats <- reactive({
  
  kees <- df_selected()

  df_boostraps <- data.frame()
  
    
    #Perform the resampling nsteps number of times (typically 1,000-10,000x)
    for (i in 1:nsteps) {
    
    #Caclulate the median and mean from a boostrapped sample (resampled_median, resampled_mean) and add to the dataframe
      df_temp <- data.frame(Condition=levels(factor(kees$Condition)), resampled_median=tapply(kees$Value, kees$Condition, boot_median), resampled_mean=tapply(kees$Value, kees$Condition, boot_mean))
    
    #Add the new median and mean to a datafram that collects all the resampled stats
      df_boostraps <- bind_rows(df_boostraps, df_temp)
    }
  
  return(df_boostraps)
  
})

#### Calculate the differences ####

df_diffs <- reactive({
  
  control_condition <- as.character(input$zero)
#  observe({ print(control_condition)})
  kees <- df_selected()
  koos <- df_bootstrap_stats()
  
  
  #Select the statistic (mean or median) for calculating the difference based on user input
  if (input$summaryInput != "mean") {
  df_spread <- koos %>% select(Condition, resampled_median) %>% group_by(Condition) %>% mutate(id = 1:n()) %>% spread(Condition, resampled_median)
  } else if (input$summaryInput == "mean") {
  df_spread <- koos %>% select(Condition, resampled_mean) %>% group_by(Condition) %>% mutate(id = 1:n()) %>% spread(Condition, resampled_mean)
  }
    
  #need this to get the base R syntax in the next line to calculate differences to work
  df_spread <- as.data.frame(df_spread)
  
  #Subtract the Column with " Control" from the other columns and move these 'differences' into a new dataframe
  #there is probably a tidyverse solution using mutate or transmute
  #  df %>% mutate_at(.funs = funs(diffs = .-Control), .vars = vars(2:ncol(.)))

  ## Subtracting the control from all Conditions  
  df_spread_diffs <- df_spread[,2:ncol(df_spread)] - df_spread[,control_condition]
  
  #Convert the dataframe with differences between medians into a long format
  df_differences <- gather(df_spread_diffs, Condition, Difference)

  return(df_differences)
})

df_summary_diffs <- reactive({

  koos <- df_diffs()
  df_diff_summary <-   koos %>% 
    group_by(Condition) %>%
    summarise(
      difference = mean(Difference, na.rm = TRUE),
      CI_lo=quantile(Difference, probs=lower_percentile),
      CI_hi=quantile(Difference, probs=upper_percentile))

 #   observe({ print(df_diff_summary)})
  
  return(df_diff_summary)
})

####### Calculate the p-value by randomization #######

df_p <- reactive({
  
  control_condition <- as.character(input$zero)
  kees <- df_selected()
  
  df_spread <- kees %>% group_by(Condition) %>% mutate(id = 1:n()) %>% spread(Condition, Value)
  #need this to get the base R syntax in the next line to calculate differences to work
  df_spread <- as.data.frame(df_spread)
  
  #Extract the list of values that correspond to control condition
#  Controls <- (df_spread[,control_condition]) %>% na.omit %>% unlist(use.names = FALSE)
  Controls <- df_spread %>% select(!!control_condition) %>% filter(!is.na(.)) %>% unlist(use.names = FALSE)
  
  
  #Median and mean values as reference/observed values
  df_obs_stats <- kees %>% group_by(Condition) %>% summarise(mean=mean(Value, na.rm=TRUE), median=median(Value, na.rm=TRUE))
  
  #generate a df with differences from the observations
  df_obs_stats$mean <- df_obs_stats$mean - mean(Controls)
  df_obs_stats$median <- df_obs_stats$median - median(Controls)

  observe({ print(df_obs_stats)})
  
    
  #Determine number of observations in Control sample
  number_controls <- length(Controls)

  observe({ print(number_controls)})
#  observe({ print(Controls)})

  #Make a new dataframe with control values for each of the conditions
  df_controls <- data.frame(Condition=rep(levels(factor(kees$Condition)), each=number_controls), Value=Controls)

  #Add the original data, generating (per condition) Control&Sample values in the column "Value".
  df_combi <- bind_rows(df_controls, kees) %>% filter(!is.na(Value))
  
  df_new_stats <- data.frame()

  #Perform the randomization nsteps number of times (typically 1,000x)
  for (i in 1:nsteps) {
    
    #Randomize the dataframe
    df_permutated <- df_combi %>% group_by(Condition) %>% sample_frac()
    
    #Determine the (new) control mean and (new) sample mean
    df_control <- df_permutated %>% slice(1: number_controls) %>% summarise(new_control_mean=mean(Value), new_control_median=median(Value))
    df_sample <- df_permutated %>% slice((number_controls+1):length(Value))%>% summarise(new_sample_mean=mean(Value), new_sample_median=median(Value))
    df_diff <- full_join(df_control, df_sample,by="Condition")
    
    df_new_stats <- bind_rows(df_new_stats, df_diff)
    
  }
  
  #Calculate the difference in mean and median (sample-control) for all the calculated new stats
  df_all_diffs <- df_new_stats %>% mutate(new_diff_mean=new_sample_mean-new_control_mean,
                                          new_diff_median=new_sample_median-new_control_median)
  

  
  #Add the observed stats to stats from permutated df
  df_all_diffs <- full_join(df_all_diffs, df_obs_stats, by="Condition")
  
  observe({ print(head(df_all_diffs))})
  
  #Determine the occurences where the permutated difference is more extreme than the observed difference
  if (input$summaryInput == "mean") {
  def_p <- df_all_diffs %>% group_by(Condition) %>% mutate(count = if_else(abs(new_diff_mean) >= abs(mean), 1, 0)) %>% summarise(p_mean=mean(count))
  } else if (input$summaryInput != "mean") {
  def_p <- df_all_diffs %>% group_by(Condition) %>% mutate(count2 = if_else(abs(new_diff_median) >= abs(median), 1, 0)) %>% summarise(p_median=mean(count2))
  }
  
  
  def_p <- as.data.frame(def_p)

  #Replace p-values of zero with <0.001 (0 is theoretically not possible, but an upper bound can estimated, which is 1/nsteps = 1/1000)
  def_p[def_p==0]<-"<0.001"
  
  return(def_p)
})

######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({
  width <- input$plot_width
  if (input$show_diffs && !input$rotate_plot) {width <- width*2}
  return(width)
  })
height <- reactive ({
  height <- input$plot_height
  if (input$show_diffs && input$rotate_plot) {height <- height*2}
  return(height)
  })


output$downloadPlotPDF <- downloadHandler(
    filename <- function() {
    paste("PlotsOfDiffs_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {

    plotlist <- list(plot_data(), plot_diffs())
    to_keep <- !sapply(plotlist,is.null)
    plotlist <- plotlist[to_keep] 
    if (input$rotate_plot == TRUE) {
      pdf(file, width = input$plot_width/72, height = input$plot_height/72*2)
      
      grid.arrange(grobs=plotlist, nrow=length(plotlist), ncol=1)
    } else if (input$rotate_plot == FALSE) {
      pdf(file, width = input$plot_width/72*2, height = input$plot_height/72)
      grid.arrange(grobs=plotlist, nrow=1, ncol=length(plotlist)) 
    }
    
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

# output$downloadPlotSVG <- downloadHandler(
#   filename <- function() {
#     paste("PlotsOfDiffs_", Sys.time(), ".svg", sep = "")
#   },
#   content <- function(file) {
#     
#     plotlist <- list(plot_data(), plot_diffs())
#     to_keep <- !sapply(plotlist,is.null)
#     plotlist <- plotlist[to_keep] 
#     if (input$rotate_plot == TRUE) {
#       svg(file, width = input$plot_width/72, height = input$plot_height/72*2)
#       grid.arrange(grobs=plotlist, nrow=length(plotlist), ncol=1)
#     } else if (input$rotate_plot == FALSE) {
#       svg(file, width = input$plot_width/72*2, height = input$plot_height/72)
#       grid.arrange(grobs=plotlist, nrow=1, ncol=length(plotlist)) 
#     }
#     
#     dev.off()
#   },
#   contentType = "application/svg" # MIME type of the image
# )



output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("PlotsOfDiffs_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    
    plotlist <- list(plot_data(), plot_diffs())
    to_keep <- !sapply(plotlist,is.null)
    plotlist <- plotlist[to_keep]
    if (input$rotate_plot == TRUE) {
      png(file, width = input$plot_width*4, height = input$plot_height*8, res=300)
      grid.arrange(grobs=plotlist, nrow=length(plotlist), ncol=1)
    } else if (input$rotate_plot == FALSE) {
      png(file, width = input$plot_width*8, height = input$plot_height*4, res=300)
      
      grid.arrange(grobs=plotlist, nrow=1, ncol=length(plotlist)) 
    }
    
    dev.off()
    
  },
  contentType = "application/png" # MIME type of the image
)

######## PREPARE PLOT FOR DISPLAY ##########

plot_data <-  reactive ({

  
  ####### Read the order from the ordered dataframe #############  
  koos <- df_sorted()
  
  #   observe({ print(koos) })
  
  custom_order <-  levels(factor(koos$Condition))
  #    custom_labels <- levels(factor(koos$label))
  
  observe({ print(custom_order) })
  #   observe({ print(custom_labels) })    
  width_column <- 0.7
  
  
  ########## Define alternative color palettes ##########
  
  newColors <- NULL
  
  if (input$adjustcolors == 2) {
    newColors <- Tol_bright
  } else if (input$adjustcolors == 3) {
    newColors <- Tol_muted
  } else if (input$adjustcolors == 4) {
    newColors <- Tol_light
  } else if (input$adjustcolors == 5) {
    newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
  }
  
########## Set default to Plotting "Condition" and "Value"
    if (input$x_var == "none") {
      x_choice <- "Condition"
    } else if (input$x_var != "none") {
      x_choice <- as.character(input$x_var)
    }  
    
    if (input$y_var == "none") {
      y_choice <- "Value"
    } else if (input$y_var != "none") {
      y_choice <- as.character(input$y_var)
    }

  ########## Define if color is used for the data
  #    observe({ print(class(input$colour_list)) })
  if (input$color_data == FALSE) {
    kleur <- NULL
  } else if (input$color_data == TRUE) {
    #    kleur <- as.character(input$colour_list)
    kleur <- x_choice
  }
  
  
  ######## The df_upload_tidy is used for defining colors, needed for compatibility with tidy data and for coloring factors
  klaas <- df_upload_tidy() 
  klaas <- as.data.frame(klaas)
  
  
  
  
  max_colors <- nlevels(as.factor(koos$Condition))
  if(length(newColors) < max_colors) {
    newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
  }
  
  
  
  
  
  
  ########## Define if/how color is used for the stats ############
  #    observe({ print(class(input$colour_list)) })
  if (input$color_stats == FALSE) {
    kleur_stats <- NULL
  } else if (input$color_stats == TRUE && input$summaryInput == "boxplot") {
    kleur_stats <- x_choice
  } else if (input$color_stats == TRUE && input$summaryInput == "violin") {
    kleur_stats <- x_choice
  } else if (input$color_stats == TRUE) {
    kleur_stats <- "Condition"
  }  
    
  
  # Define minimal n - only plot box/violinplots for min_n>9
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)
  
  ######## Change appearance of plot width under for non-jittered points or segments when n is low #########
  if (input$jitter_type == "none") {width_column <- width_column/2}
  #    if (input$jitter_type == "stripes" && min_n <10) {width_column <- width_column/2}
  

############## GENERATE PLOT LAYERS #############

    p <- ggplot(data=df_selected(), aes_string(x="Condition")) 
    
    # Setting the order of the x-axis
    p <- p + scale_x_discrete(limits=custom_order)

  ##### plot selected data summary (bottom layer) ####
    if (input$summaryInput == "boxplot" && min_n>9) {
      p <- p + geom_boxplot(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, fill=kleur_stats), notch = input$add_CI, outlier.color=NA, width=0.8, size=0.5, alpha=input$alphaInput_summ)
  
    } else if (input$summaryInput == "violin" && min_n>9) {
      p <- p + geom_violin(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, fill=kleur_stats),scale = "width", draw_quantiles = c(0.5), width=0.8, size=0.5, alpha=input$alphaInput_summ) 
      if (input$add_CI == TRUE) {
        p <- p + geom_linerange(data=df_summary_median(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi"), colour="black", size =3,alpha=input$alphaInput_summ)
       
      }
    }

    #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "quasirandom") {
      p <- p + geom_quasirandom(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), shape = 16, varwidth = TRUE, cex=3.5, alpha=input$alphaInput)
      
      #Uncomment for sinaplot    } else if (input$jitter_type == "sina") {
      #Uncomment for sinaplot p <- p + geom_sina(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), method="density", maxwidth = .8, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), shape = 16, width=0.3, height=0.0, cex=3.5, alpha=input$alphaInput)
    } else if (input$jitter_type == "stripes") {
      p <- p + geom_segment(data=df_sorted(), aes(x=match(Condition, levels(Condition))-((width_column/2)-0.1), xend=match(Condition, levels(Condition))+((width_column/2)-0.1), y=Value, yend=Value), size=1, color="black", alpha=input$alphaInput)
    } else if (input$jitter_type == "none") {
      p <- p + geom_jitter(data=klaas, aes_string(x=x_choice, y=y_choice, colour = kleur), shape = 16, width=0, height=0.0, cex=3.5, alpha=input$alphaInput)
    }
    
  ##### plot selected data summary (top layer) ####
    if (input$summaryInput == "median"  && input$add_CI == TRUE && min_n>9) {
    p <-  p + geom_point(data=df_summary_median(), aes_string(x="Condition", y = "median", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)+
              geom_linerange(data=df_summary_median(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)
    }

    else if (input$summaryInput == "median"  && input$add_CI == FALSE || min_n<10) {
      p <-  p + geom_errorbar(data=df_summary_median(), aes_string(x="Condition", ymin="median", ymax="median", colour = kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == TRUE && min_n>9) {
      p <- p + geom_linerange(data=df_summary_mean(), aes_string(x="Condition", ymin = "mean_CI_lo", ymax = "mean_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)+
        geom_point(data=df_summary_mean(), aes_string(x="Condition", y = "mean", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == FALSE || min_n<10) {
      p <- p + geom_errorbar(data=df_summary_mean(), aes_string(x="Condition", ymin="mean", ymax="mean", colour=kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)
    }  

########### Do some formatting of the lay-out

     p <- p+ theme_light(base_size = 16)
    
     # if log-scale checked specified
     if (input$scale_log_10)
       p <- p + scale_y_log10() 
     
     #Adjust scale if range (min,max) is specified
     if (input$range != "" &&  input$change_scale == TRUE) {
       rng <- as.numeric(strsplit(input$range,",")[[1]])
       
       #If min>max invert the axis
       if (rng[1]>rng[2]) {p <- p+ scale_y_reverse()}
       
       #Autoscale if rangeis NOT specified
     } else if (input$range == "" || input$change_scale == FALSE) {
       rng <- c(NULL,NULL)
     }
     
     p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
     #### If selected, rotate plot 90 degrees CW ####
     if (input$rotate_plot == FALSE) { p <- p + coord_flip(ylim=c(rng[1],rng[2]))}
     
    # if title specified
    if (input$add_title)
      p <- p + ggtitle(input$title)
    
    # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)
    
    # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
    }
     
     #remove legend (if selected)
     if (input$add_description == FALSE) {
       p <- p + theme(legend.position="none")
     }

     #remove gridlines (if selected)
     if (input$no_grid == TRUE) {  
       p <- p+ theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank())
     }
     
          
   if (input$adjustcolors >1) {
       p <- p+ scale_color_manual(values=newColors)
       p <- p+ scale_fill_manual(values=newColors)
   }
    
    ### Output the plot ######
    p
    
  }) 

######## PREPARE Differences PLOT FOR DISPLAY ##########


plot_diffs <- reactive ({ 

  ### Do not show differences if not selected
  if (!input$show_diffs) return(NULL)
  
  
  ########## Define minimal n - only 95% CI for min_n>9
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)

  ####### Read the order from the ordered dataframe
  koos <- df_sorted()
  custom_order <-  levels(factor(koos$Condition))
  
  ########## Define alternative color palettes
  
  newColors <- NULL
  
  if (input$adjustcolors == 2) {
    newColors <- Tol_bright
  } else if (input$adjustcolors == 3) {
    newColors <- Tol_muted
  } else if (input$adjustcolors == 4) {
    newColors <- Tol_light
  } else if (input$adjustcolors == 5) {
    newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
  }
  
  ######## Repeat the colors, if number of colors < number of conditions
  klaas <- df_selected()
  max_colors <- nlevels(as.factor(klaas$Condition))
  if(length(newColors) < max_colors) {
    newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
  }
  
  
  
  ########## Set default to Plotting "Condition" and "Value"
  if (input$x_var == "none") {
    x_choice <- "Condition"
  } else if (input$x_var != "none") {
    x_choice <- as.character(input$x_var)
  }  
  
  if (input$y_var == "none") {
    y_choice <- "Difference"
  } else if (input$y_var != "none") {
    y_choice <- as.character(input$y_var)
  }
  
  ########## Define if color is used for the data
  #    observe({ print(class(input$colour_list)) })
  if (input$color_data == FALSE) {
    kleur <- NULL
  } else if (input$color_data == TRUE) {
    #    kleur <- as.character(input$colour_list)
    kleur <- x_choice
  }
  
  ########## Define if/how color is used for the stats
  #    observe({ print(class(input$colour_list)) })
  if (input$color_stats == FALSE) {
    kleur_stats <- NULL
  } else if (input$color_stats == TRUE) {
    kleur_stats <- "Condition"
  }  
  
  
  p2 <- ggplot(data=df_summary_diffs(), aes_string(x="Condition")) 

    # Setting the order of the x-axis
  p2 <- p2 + scale_x_discrete(limits=custom_order)
  
  if (input$color_data == FALSE) {
    
    p2 <- p2 + geom_flat_violin(data = df_diffs(), aes_string(x="Condition", y="Difference"), alpha=input$alphaInput, fill = "grey20", scale = "width", color =NA,position = position_nudge(x = -0.0, y = 0))
  } else if (input$color_data == TRUE) {
    p2 <- p2 + geom_flat_violin(data = df_diffs(), aes_string(x="Condition", y="Difference", fill=kleur), alpha=input$alphaInput, scale = "width", color =NA,position = position_nudge(x = -0.0, y = 0))
    
  }
  
  p2 <- p2 + geom_point(data=df_summary_diffs(), aes_string(x="Condition", y = "difference", colour=kleur_stats), shape = 21,color = "black",fill=NA,size = 8, alpha=input$alphaInput_summ)

  #Plot the 95%CI of the difference if selected AND if n>9
  if (input$add_CI == TRUE && min_n>9) {
    p2 <- p2 + geom_linerange(data=df_summary_diffs(), aes_string(ymin = "CI_lo", ymax = "CI_hi", colour=kleur_stats), size =3, alpha=input$alphaInput_summ)
  }
  p2 <- p2 + geom_hline(yintercept = 0, col = "black", size = .5)

  
  ########### Do some formatting of the lay-out
  p2 <- p2 + theme_light(base_size = 16)
  
  # if font size is adjusted
  if (input$adj_fnt_sz) {
    p2 <- p2 + theme(axis.text = element_text(size=input$fnt_sz_ax))
    p2 <- p2 + theme(axis.title = element_text(size=input$fnt_sz_ttl))
  }
  
  # if title specified
  if (input$add_title)
    p2 <- p2 + ggtitle(" ")
  if (input$rotate_plot == FALSE) {
    p2 <- p2 + coord_flip()
    p2 <- p2 + labs(x = NULL)
    p2 <- p2 + theme(axis.text.y = element_text(size=0))
  }
  
  #remove legend (if selected)
  if (input$add_description == FALSE) {  
    p2 <- p2 + theme(legend.position="none")
  }
  
  #remove gridlines (if selected)
  if (input$no_grid == TRUE) {  
    p2 <- p2+ theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
  }
  
  if (input$adjustcolors >1) {
    p2 <- p2+ scale_color_manual(values=newColors)
    p2 <- p2+ scale_fill_manual(values=newColors)
  }
  
  p2 
  
})

############### RenderPLOT #########
output$coolplot <- renderPlot(width = width, height = height, {     

  plotlist <- list(plot_data(), plot_diffs())
  to_keep <- !sapply(plotlist,is.null)
  plotlist <- plotlist[to_keep]
  if (input$rotate_plot == TRUE) {
    grid.arrange(grobs=plotlist, nrow=length(plotlist), ncol=1)
  } else if (input$rotate_plot == FALSE) {
    
    grid.arrange(grobs=plotlist, nrow=1, ncol=length(plotlist)) 
  }
  
}) #close output$coolplot


#### Export the data in tidy format ###########

output$downloadData <- downloadHandler(
  filename = function() {
    paste("PlotsOfDiffs_Tidy", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_selected(), file, row.names = FALSE)
  }
)




#### Combine the statistics in one table and filter ###########

df_filtered_stats <- reactive({
  
  digits <- as.numeric(input$digits)
  
  #Combine the numbers from the 95% CI for the mean to show the interval
  klaas <- df_summary_mean() %>% mutate(mean_CI_lo = round(mean_CI_lo, digits), mean_CI_hi = round(mean_CI_hi, digits)) %>% unite("95CI mean", c("mean_CI_lo","mean_CI_hi"), sep=" - ")
  observe({ print((klaas)) }) 
  
  #Combine the numbers from the 95% CI for the median to show the interval
  koos <- df_summary_median() %>% mutate(median_CI_lo = round(median_CI_lo, digits), median_CI_hi = round(median_CI_hi, digits)) %>% unite("95CI median", c("median_CI_lo","median_CI_hi"), sep=" - ")
  
  klaas  <- full_join(klaas, koos,by="Condition")
  
  # Round down to the number of selected digits
  klaas <- klaas %>% mutate_at(c(3:5, 7:11), round, input$digits)
  
  ##### Show the statistics selected by the user ############  
  
  if (!is.null(input$stats_select)) {
    columns = input$stats_select
    columns <- c("Condition", "n", columns)
    df <- klaas %>% select(one_of(columns))
  } else if (is.null(input$stats_select)) {
    df <- klaas %>% select("Condition", "n")}
})





#### A predined selection of stats for the table  ###########

observeEvent(input$summaryInput, {
  if (input$summaryInput=="mean")  {
    updateSelectInput(session, "stats_select", selected = list("mean", "sd", "95CI mean"))
  }
  else if (input$summaryInput=="median")  {
    updateSelectInput(session, "stats_select", selected = list("median", "MAD", "95CI median"))
  }
  else if (input$summaryInput=="box")  {
    updateSelectInput(session, "stats_select", selected = list("median", "IQR", "95CI median"))
  }
  else if (input$summaryInput=="violin")  {
    updateSelectInput(session, "stats_select", selected = list("median", "95CI median"))
  }
  
})

observeEvent(input$select_all1, {
  updateSelectInput(session, "stats_select", selected = list("mean", "sd", "sem", "95CI mean","median", "MAD", "IQR", "Q1", "Q3", "95CI median"))
})

observeEvent(input$deselect_all1, {
  updateSelectInput(session, "stats_select", selected = "")
})


#### Render the data summary as a table ###########

output$data_summary <- renderDataTable(
  datatable(
    df_filtered_stats(),
    #  colnames = c(ID = 1),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip', pageLength = 100,
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ) 
  ) 
  #   %>% formatRound(n, digits=0)
) 



#### RenderDataTable: differences ###########

output$data_diffs <- renderDataTable({
  
  control_condition <- as.character(input$zero)
  df_temp <- df_summary_diffs()  %>% mutate_at(c(2:4), round, input$digits_diff)
  #Remove the reference condition from the table
#  df_temp <- df_temp %>% filter(Condition != control_condition)

  ##Do not plot values for reference
  df_temp[df_temp==0]<-NA
  
  if (input$calc_p == TRUE) {
    def_p <- df_p()
    df_temp <- left_join(df_temp,def_p,by="Condition")
  }
  #Print stats of differences to console
  observe({ print(df_temp)})

  #Add dowloadbuttons and allow reordering  
  datatable(
    df_temp,
    #  colnames = c(ID = 1),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip',
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ) 
  ) 
  
})


###### Figure legend #########
Fig_legend <- renderText({
  
  if (input$rotate_plot == TRUE) {
    x <- "horizontal"
    y <- "vertical"
    orient <- "bottom"
  }
  else if (input$rotate_plot == FALSE) {
    x <- "vertical"
    y <- "horizontal"
    orient <- "right"
  }
  
  df_temp <- df_summary_mean()
  min_n <- min(df_temp$n)
  
  if (input$jitter_type == "quasirandom" || input$jitter_type == "random") { jitter <- c("jittered dots")}
  else if (input$jitter_type == "stripes") {jitter <- c("stripes")}
  else if (input$jitter_type == "none") {jitter <- ("dots")}
  
  if (input$summaryInput == "median" && input$add_CI == FALSE)  { stats <- paste(x," line indicating the median ")}
  else if (input$summaryInput == "mean" && input$add_CI == FALSE) {stats <- paste(x," line indicating the mean ")}
  else if (input$summaryInput == "box" && input$add_CI == FALSE && min_n>9) {stats <- paste("a boxplot, with the box indicating the IQR and ", x," line indicating the median ")}
  else if (input$summaryInput == "violin" && min_n>9) {stats <- paste("a violinplot reflecting the data distribution and a ", x," line indicating the median ")}  
  
  
  else if (input$summaryInput == "median" && input$add_CI == TRUE)  { stats <- c("an open circle indicating the median ")}
  else if (input$summaryInput == "mean" && input$add_CI == TRUE) {stats <- c("an open circle indicating the mean ")}
  else if (input$summaryInput == "box" && input$add_CI == TRUE) {stats <- paste("a boxplot, with the box indicating the IQR and ", x," line indicating the median ")}
  
  if (input$add_CI == TRUE && min_n>9 && input$summaryInput != "box" && input$summaryInput != "mean") {stat_inf <- paste(" A ", y," bar indicates for each median the 95% confidence interval determined by bootstrapping. ")}
  else if (input$add_CI == TRUE && min_n>9 && input$summaryInput == "box") {stat_inf <- c("The notches represent for each median the 95% confidence interval (approximated by 1.58*IQR/sqrt(n)). ")}
  else if (input$add_CI == TRUE && min_n>9 && input$summaryInput == "mean") {stat_inf <- c(" A ", y," bar indicates for each mean the 95% confidence interval. ")}
  else {stat_inf <- NULL}
  
  
  Legend <- c('</br></br><h4>Figure description</h4>')
  
  #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
  
  Legend<-append(Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
  
  Legend <- NULL
  
  Legend<-append(Legend, paste("Graph that shows the data as ", jitter," (visibility: ", input$alphaInput, "). ", sep=""))
  
  #Concatenate the condition with n
  koos <- df_summary_mean() %>% unite(label_n, c(Condition, n), sep="=", remove = FALSE)
  label_n <- paste(koos$label_n,collapse=", ")
  Legend <-append(Legend, paste("The number of samples per condition is: " ,label_n, ". ",sep=""))
  
  Legend<-append(Legend, paste("The summary of the data is shown as ", stats," (visibility: ", input$alphaInput_summ, "). ", sep=""))
  
  Legend <-append(Legend, paste(stat_inf, sep=""))
  
  if (input$color_data ==TRUE || input$color_stats) {Legend<-append(Legend, "The color coding is indicated in the legend next to the plot. ")		}
  
  if (input$ordered =="median") {Legend<-append(Legend, "The data are ordered according to their median value. ")		}
  
  if (input$scale_log_10) {Legend<-append(Legend, "The values are plotted on a log10 scale. ")		}
  
  if (input$show_diffs) {
    Legend<-append(Legend, paste("The plot on the ", orient," shows the effect size, relative to: ", input$zero,". The bootstrap samples that are used to calculate the 95%CI of the effect size are shown as a distribution", sep=""))
    
    }
  
  
  
    return(Legend)
  
})

######## Figure Legend in HTML format #############

output$LegendText <- renderText({
  
  if (input$add_description == FALSE || input$rotate_plot == TRUE) {return(NULL)}
  
  HTML_Legend <- c('</br></br></br></br><h4>Figure description</h4>')
  
  #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
  
  HTML_Legend <-append(HTML_Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
  
  HTML_Legend <- append(HTML_Legend, Fig_legend())
  
  HTML_Legend <- append(HTML_Legend, paste("</p>"))
  
})


######## The End; close server ########################

} #close "server"

shinyApp(ui = ui, server = server)