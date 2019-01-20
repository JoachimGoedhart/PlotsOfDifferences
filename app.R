##############################################################################
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
##############################################################################
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
##############################################################################

# ToDo
# Implement fold-change as an effect size

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(ggbeeswarm)
library(readxl)
library(DT)
library(gridExtra)

source("geom_flat_violin.R")

################

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
df_wide_example <- read.csv("Data_wide_example.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example.csv", na.strings = "")

 #######################################
###### Define the User interface #########

ui <- fluidPage(
  titlePanel("PlotsOfDifferences - Plots all Of the Data and Differences"),
  sidebarLayout(
    sidebarPanel(width=3,
      conditionalPanel(
        condition = "input.tabs=='Plot'",
        radioButtons("jitter_type", "Data offset", choices = list("Beeswarm" = "beeswarm", "Random" = "random", "None (for small n)" = "none"), selected = "beeswarm"),
        
        
        
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

        checkboxInput(inputId = "adjust_scale",
                      label = "Adjust scale",
                      value = FALSE),
        conditionalPanel(
          condition = "input.adjust_scale == true",
          textInput("range", "Range of values (min,max)", value = "0,2")),
        
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
        conditionalPanel(
          condition = "input.color_data == true || input.color_stats == true",
          checkboxInput(inputId = "add_legend",
                        label = "Add legend",
                        value = FALSE))
      ),
      conditionalPanel(
        condition = "input.tabs=='Data upload'",
        h4("Data upload"),
        radioButtons(
          "data_input", "",
          choices = 
            list("Example 1 (wide format)" = 1,
                 "Example 2 (tidy format)" = 2,
                 "Upload file" = 3,
                 "Paste data" = 4)
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
        checkboxInput(inputId = "tidyInput",
                      label = "These data are Tidy",
                      value = FALSE),
        conditionalPanel(
          condition = "input.tidyInput==true",
          h5("",
             a("Click here for more info on tidy data",
               href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
          selectInput("x_var", "Conditions to compare:", choices = ""),
          selectInput("y_var", "Variables:", choices = "")
          
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
        p("The p-value is determined by a randomization test, calculation may take some time")
      )

    ),
    mainPanel(
 
       tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"), dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"), downloadButton("downloadPlotPNG", "Download png-file"), div(`data-spy`="affix", `data-offset-top`="10", plotOutput("coolplot"))
                  ), 
                  tabPanel("Summary",
                           conditionalPanel(
                             condition = "input.summaryInput=='mean'",
                             h4("Summary of the differences - based on means")),
                           conditionalPanel(
                             condition = "input.summaryInput!='mean'",
                             h4("Summary of the differences - based on medians")),
                           dataTableOutput('data_diffs'),
                           h4("Summary of the data"),
                           dataTableOutput('data_summary')

                            ),
                  tabPanel("About", includeHTML("about.html")
                           )
                  
      )
    )
  )         
)

 #######################################

server <- function(input, output, session) {

  #####################################
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
    return(data)
})

 #####################################
  
 ####################################
##### CONVERT TO TIDY DATA ##########
  
#Need to tidy the data?!
#Untidy data will be converted to long format with two columns named 'Condition' and 'Value'
#The input for "Condition" will be taken from the header, i.e. first row
#Tidy data will be used as supplied
df_upload_tidy <- reactive({
    if(input$tidyInput == FALSE ) {
      klaas <- gather(df_upload(), Condition, Value)
    }
    else if(input$tidyInput == TRUE ) {
      klaas <- df_upload()
    }
  return(klaas)
})
 ###################################


 #############################################################################
##### Get the Variables (necessary for tidy data with multiple variables) ##############

observe({ 
        var_names  <- names(df_upload_tidy())
        
        var_list <- c("none", var_names)
#        updateSelectInput(session, "colour_list", choices = var_list)
        updateSelectInput(session, "y_var", choices = var_list)
        updateSelectInput(session, "x_var", choices = var_list)

        # Get a list of the different conditions        
        koos <- df_upload_tidy()
        conditions_list <- as.factor(koos$Condition)
        updateSelectInput(session, "zero", choices = conditions_list)
        
        })
 ###################################    

 ###########################################################  
######## Determine and set the order of the Conditions #######  
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

########################################################### 


 ###################################################################################
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
 ###########################################################  

  
 #############################################################
#### DISPLAY UPLOADED DATA (exactly as provided) ##################
    
output$data_uploaded <- renderDataTable({
#    observe({ print(input$tidyInput) })
      df_upload()
  })
 #############################################################


 ##################################################
#### Caluclate Summary of the DATA for the MEAN ####

df_summary <- reactive({
  klaas <- df_bootstrap_stats()
  koos <- df_selected() %>%
    group_by(Condition) %>% 
    summarise(n = n(),
            mean = mean(Value),
            median = median(Value),
            sd = sd(Value)) %>%
  mutate(sem = sd / sqrt(n - 1),
         mean_CI_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
         mean_CI_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
  
  
  if (input$summaryInput != "mean") {
  kees  <- df_selected() %>%
    group_by(Condition) %>% 
    summarise(
              IQR = IQR(Value),
              MAD = mad(Value, constant=1))

  kees$median_CI_lo <- tapply(klaas$resampled_median, klaas$Condition, quantile, probs=lower_percentile)
  kees$median_CI_hi <- tapply(klaas$resampled_median, klaas$Condition, quantile, probs=upper_percentile)
  koos <- left_join(koos,kees, by="Condition")
  
  }
  
  #Prints a table with stats in the Console
  observe({ print(koos) })

  return(koos)
  })

 #################################################


 #########################################################
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


####################################################
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

 ####################################################
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

 ####################################################


 ###########################################
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


 ###########################################


 ###########################################
######## PREPARE PLOT FOR DISPLAY ##########
 ###########################################

plot_data <-  reactive ({

  
####### Read the order from the ordered dataframe #############  
    koos <- df_sorted()
    custom_order <-  levels(factor(koos$Condition))
    
#    observe({ print(custom_order) })

    

  
  
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
    
  
########## Define minimal n - only plot box/violinplots for min_n>9
    df_temp <- df_summary()
    min_n <- min(df_temp$n)

 ###############################################
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
        p <- p + geom_linerange(data=df_summary(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi"), colour="black", size =3,alpha=input$alphaInput_summ)
       
      }
    }

   #### plot individual measurements (middle layer) ####
    if (input$jitter_type == "beeswarm") {
      p <- p + geom_quasirandom(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, colour = kleur), varwidth = TRUE, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "random") {
      p <- p + geom_jitter(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, colour = kleur), width=0.3, height=0.0, cex=3, alpha=input$alphaInput)
    } else if (input$jitter_type == "none") {
      p <- p + geom_jitter(data=df_upload_tidy(), aes_string(x=x_choice, y=y_choice, colour = kleur), width=0, height=0.0, cex=3, alpha=input$alphaInput)
    }
    
  ##### plot selected data summary (top layer) ####
    if (input$summaryInput == "median"  && input$add_CI == TRUE && min_n>9) {
    p <-  p + geom_point(data=df_summary(), aes_string(x="Condition", y = "median", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)+
              geom_linerange(data=df_summary(), aes_string(x="Condition", ymin = "median_CI_lo", ymax = "median_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)
    }

    else if (input$summaryInput == "median"  && input$add_CI == FALSE || min_n<10) {
      p <-  p + geom_errorbar(data=df_summary(), aes_string(x="Condition", ymin="median", ymax="median", colour = kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == TRUE && min_n>9) {
      p <- p + geom_linerange(data=df_summary(), aes_string(x="Condition", ymin = "mean_CI_lo", ymax = "mean_CI_hi", colour=kleur_stats), size =3,alpha=input$alphaInput_summ)+
        geom_point(data=df_summary(), aes_string(x="Condition", y = "mean", colour=kleur_stats), shape = 21,fill=NA,size = 8,alpha=input$alphaInput_summ)

    } else if (input$summaryInput == "mean"  && input$add_CI == FALSE || min_n<10) {
      p <- p + geom_errorbar(data=df_summary(), aes_string(x="Condition", ymin="mean", ymax="mean", colour=kleur_stats), width=.8, size=2, alpha=input$alphaInput_summ)
    }  

########### Do some formatting of the lay-out

     p <- p+ theme_light(base_size = 16)
    
     #### If selected, rotate plot 90 degrees CW ####
     rng <- as.numeric(strsplit(input$range,",")[[1]])
     
     # if the range of values is specified    
     if (input$adjust_scale == TRUE) { 
       p <- p + coord_cartesian(ylim=c(rng[1],rng[2]))
     } else if (input$adjust_scale == FALSE)
     {
       rng <- c(NULL,NULL)
     }
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
    if (input$add_legend == FALSE) {  
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

###########################################
######## PREPARE Differences PLOT FOR DISPLAY ##########


plot_diffs <- reactive ({ 

  ### Do not show differences if not selected
  if (!input$show_diffs) return(NULL)
  
  
  ########## Define minimal n - only 95% CI for min_n>9
  df_temp <- df_summary()
  min_n <- min(df_temp$n)

  ####### Read the order from the ordered dataframe #############  
  koos <- df_sorted()
  custom_order <-  levels(factor(koos$Condition))
  
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
  
  ########## Define if/how color is used for the stats ############
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
  if (input$add_legend == FALSE) {  
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

###########################################
output$coolplot <- renderPlot(width = width, height = height, {     
  ### Output the plot ######
  plotlist <- list(plot_data(), plot_diffs())
  to_keep <- !sapply(plotlist,is.null)
  plotlist <- plotlist[to_keep]
  if (input$rotate_plot == TRUE) {
    grid.arrange(grobs=plotlist, nrow=length(plotlist), ncol=1)
  } else if (input$rotate_plot == FALSE) {
    
    grid.arrange(grobs=plotlist, nrow=1, ncol=length(plotlist)) 
  }
  
}) #close output$coolplot



###########################################
#### Render the data summary as a table ###########

output$data_summary <- renderDataTable({

  #### Select the relevant stats and use these for a table with summary
  df_out <- NULL
  if (input$summaryInput == "mean") {
  df_out <- df_summary() %>% select(Condition,n,mean,sd,sem,CI_lo=mean_CI_lo,CI_hi=mean_CI_hi)
  } else if (input$summaryInput != "mean") {
  df_out <- df_summary() %>% select(Condition,n,mean,median,IQR,MAD,CI_lo=median_CI_lo,CI_hi=median_CI_hi)
  } 

  
  
#Add dowloadbuttons and allow reordering  
  datatable(
    df_out,
    #  colnames = c(ID = 1),
    selection = 'none',
    extensions = c('Buttons', 'ColReorder'),
    options = list(dom = 'Bfrtip',
                   buttons = c('copy', 'csv','excel', 'pdf'),
                   editable=FALSE, colReorder = list(realtime = FALSE), columnDefs = list(list(className = 'dt-center', targets = '_all'))
    ) 
  ) 
  
  

})
###########################################

###########################################
#### Render the differences summary as a table ###########

output$data_diffs <- renderDataTable({
  
  control_condition <- as.character(input$zero)
  df_temp <- df_summary_diffs()
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
###########################################

} #close "server"

shinyApp(ui = ui, server = server)