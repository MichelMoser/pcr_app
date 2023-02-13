##################################################################################
##################################################################################
#
#       WK APP
#       Niedrigdurchsatz  Version 13.02.2023
#
##################################################################################
##################################################################################

#lib_loc <- "//c1.resysabc.admin.ch/DFS/DEM-LS/Profile/momi/Downloads/mi_library"


#library(ggplot2, lib.loc = lib_loc)
#library(stringi, lib.loc = lib_loc)
#library(shiny, lib.loc = lib_loc)
#library(tidyverse, lib.loc = lib_loc)
#library(shinydashboard, lib.loc = lib_loc)
#library(readxl, lib.loc = lib_loc)
#library(openxlsx, lib.loc = lib_loc)
#library(plotly, lib.loc = lib_loc)
#library(reactable, lib.loc = lib_loc)
#library(scales, lib.loc = lib_loc)
#library(patchwork, lib.loc = lib_loc)
#library(ggpubr, lib.loc = lib_loc)
#library(ggtext, lib.loc = lib_loc)
#library(forcats, lib.loc = lib_loc)
#library(shinyWidgets, lib.loc = lib_loc)
#library(openxlsx, lib.loc = lib_loc)

library(RDML)
library(chipPCR)
library(ggplot2)
library(stringi)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(readxl)
library(openxlsx)
library(plotly)
library(reactable)
library(scales)
library(patchwork)
library(ggpubr)
library(ggtext)
library(forcats)
library(shinyWidgets)
library(openxlsx)


ui <- dashboardPage(
  dashboardHeader(title = "RT-PCR dashboard v1.0.1"),
  dashboardSidebar(
    
    ## Sidebar content
      sidebarMenu(
        menuItem("Curves and Layouts", tabName = "t1", icon = icon("th")),
        menuItem("Controls", tabName = "t2", icon = icon("th")),
        menuItem("Analysis", tabName = "t3", icon = icon("th")), 
        menuItem("Report", tabName = "t4", icon = icon("th"))
        
      ),    
    br(), 
    br(),
    br(), 
    textInput("label_pref", "Set sample prefix: ", value = "For example LD for LD0001"),
    
    br(),
    
    
    
    fileInput("file2", "Load LightCylcer xlsx File", accept = ".xlsx"), 
    fileInput("file3", "Load LightCylcer lc96p File", accept = ".lc96p"), 
    
    br(),
    numericInput("n_replicate", "Number of replicates", 2, min = 1, max = 10),
    
    br(),
    br(), 
    numericInput("ctrlCT", "Maximal CT cutoff value", 36, min = 10, max = 38),
    
    br(),
    
    numericInput("deltaCT", "Maximal delta CT value", 2, min = 0, max = 10),
    br(),
    
    
    numericInput("maxdeltaNPC", "Maximal delta CT to NPC value", 2, min = 0, max = 10),
    br(),
    
    
    sliderInput("extRange", "Extraction control cut offs:",
                min = 10, max = 42,
                value = c(15,36)),
    
    
    br(), 
    br(),
    br(), 
    br()#,
    
    #textInput("downloadData","Save My Data Frame:",value="Data Frame 1"),
    #textInput("filename", "Generate output file", value = paste0("data-", Sys.Date(),".txt")),
    #downloadButton("downloadDa", "Download RT-PCR Summary Results")
    
    #checkboxInput("header", "Header", TRUE)
    
    
  ),
  dashboardBody(
    tabItems(
    
      
      
      
      # First tab content  LAYOUT
      
      tabItem(tabName = "t1",
            h2("Raw qPCR results"),
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 12, height = 500, 
          plotlyOutput("curvesAll")
      ),
      box(width = 12, height = 800, 
          plotlyOutput("curves")
          
      ),      
      
      h2("Layout"),
      h3("  Set sample number"),
      box(width = 12,
        #title = "Set sample number",
        sliderInput("slider", "Number of samples:", 1, 13, 13)
      ), 
      box(width = 4, height = 700,
        plotlyOutput("plot_box", height = 650, width = 400)
      ), 
      
      box(width = 8, height = 700,
        plotlyOutput("plot_LC", height = 650, width = 1200)
      )
      
        )
    ),
    
    
    
    
    
    # Second tab content   CONTROLS
    
    
    tabItem(tabName = "t2",
            h2("Controls"), 
            
            # box(title = "Raw Light Cycler data",
            #   #reactableOutput("contents1",  width = "100%"),
            #   width = 12
            # ), 
            # 
            box(title = "", 
                switchInput(inputId = "check",onLabel = "Check samples", offLabel = "Show Ct-values",
                            value = TRUE) , width = 12
            ), 
            
            
            box(width = 6,title = "NTC (Negativkontrolle)",
                #tableOutput("neg1"),
                plotOutput("negative1")
            ), 
            
            box(width = 6, title = "NPC Mengo (Kontrolle der Extraktion, Positivkontrolle Mengo)",
                #tableOutput("neg2"),
                plotOutput("negative2")
            ),
            
            
            
            box(width = 6,title = "PPC E (Positivkontrolle M-Prot, H3)",
                #tableOutput("pos1"),
                plotOutput("positive1")
            ),
            
            box(width = 6, title = "PPC PCR (Positivkontrolle Mengo)",
                #tableOutput("pos2"),
                plotOutput("positive2")
            ) 
            
    ), 
    
    
    
    
# Third tab content   ANALYSIS
        
    tabItem(tabName = "t3",
            h2("Analyse"), 
            
            fluidRow( 
              
              box(title = "Influenza Cts", 
                  plotlyOutput("Influenza"),
                  width = 6, height = 450
              ),
              box(title = "Mengo Cts", 
                  plotlyOutput("Mengo"),
                  width = 6, height = 450
              )),
            
            
            fluidRow(box(title = "Settings", 
                tableOutput("settings"),
                width = 3, 
                height = 350
            ),
            
            box(title = "Dye", 
                
                selectInput("dye", "Select Dye", choices = c("FAM", "Cy5", "VIC"), selected = "Cy5"),
                #plotlyOutput("PCR_test"),
                #tableOutput("settings"),
                textOutput("NPC_mean"),
                tags$head(tags$style("#NPC_mean{color: black;
                                 font-size: 20px;
                                 font-style: normal;
                                 }"
                )
                ),
                width = 9, 
                height = 350
            )),
            # box(title = "", width = 5, 
            #     textOutput("NPC_mean"),
            #     tags$head(tags$style("#NPC_mean{color: black;
            #                      font-size: 20px;
            #                      font-style: normal;
            #                      }"
            #     )
            #     ),
            #      height = 250), 
            
            
            fluidRow( 
                column(title = "Plate Output", align="center",
                plotOutput("plate"),
                width = 6
            ),
            
                column( align="center",
                plotOutput("delta"), 
                width = 6
                )
            )
                        
            
    
  ), 






########################################################
#
#   FINAL REPORT
#
########################################################



tabItem(tabName = "t4",
        h2("Report"), 
        
        
        
        box(title = "Settings", 
            tableOutput("settingsF"),
            width = 3, 
            height = 250
        ),
        
        box(title = "Download Report", 
            textInput("downloadData","Save Report as Excel sheet:",value="Report"),
            downloadButton("downloadDa", "Download RT-PCR Summary Results"),
            width = 3, 
            height = 250
        ),
        box(title = "Final Report", 
            reactableOutput("report"),
            width = 12, 
            height = 550
        )
        
        
        
        
        )
        
  )
  ))











###  SERVER SIDE

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  

  v <- reactiveValues(df_sheet = NULL)
  
  
  #read in Light Cycler 
  observe({
    file2 = input$file2
    file3 = input$file3
    
    if (is.null(file2) | is.null(file3) | is.null(input$label_pref)) {
      return(NULL)
    }


      #############################################################
      ##
      ##   RDML start 
      ##
      ##############################################################





	LC_data <- RDML$new(file3$datapath, show.progress = F)
      
      
      
      tab <- LC_data$AsTable(
        # Custom name pattern 'position~sample~sample.type~target~dye'
        name.pattern = paste(
          react$position,
          react$sample$id,
          private$.sample[[react$sample$id]]$type$value,
          data$tar$id,
          target[[data$tar$id]]$dyeId$id,
          sep = "~"),
        # Custom column 'quantity' - starting quantity of added sample 
        quantity = {
          value  <- sample[[react$sample$id]]$quantity$value
          if (is.null(value) || is.na(value)) NULL
          else value
        }
      )
      
      rownames(tab) <- NULL
      
      
      
      ## Get all fluorescence data
      fdata <- as.data.frame(LC_data$GetFData(tab,
                                           # We don't need long table format for CPP()
                                           long.table = FALSE))
      
      
      fdata.cpp <- cbind(cyc = fdata[, 1],
                         apply(fdata[, -1], 2,
                               function(x){ CPP(fdata[, 1],
                                               x)$y}))
      
      
      
      tab$run.id <- "run.cpp"
      ## Set fluorescence data from previous section
      LC_data$SetFData(fdata.cpp,
                       tab)
      
      
      fdata <- LC_data$GetFData(tab,
                                # long table format for usage with ggplot2
                                long.table = TRUE)
      
    
    output$curves <- renderPlotly({
      
     
      
      
       curv_plot <- ggplot(fdata, aes(cyc, fluor)) +
         geom_line(aes(group = fdata.name,
                       color = sample))+ facet_grid(~ target) +
		xlab("cycles") +
		ylab("fluorescence")
       
       ggplotly(curv_plot) %>% layout(height = 800)






       #############################################################
       ##
       ##   RDML end 
       ##
       ##############################################################
       

       }
    )
    

    output$curvesAll <- renderPlotly({
      
     
      
      
       curv_plot <- ggplot(fdata, aes(cyc, fluor)) +
         geom_line(aes(group = fdata.name,
                       color = sample))+
		xlab("cycles") +
		ylab("fluorescence")
       
       ggplotly(curv_plot) %>% layout(height = 400)

    })


    
    
    
    
    
    
    
    prefix = "P"
    prefix = input$label_pref
    
    data2 = read_excel(file2$datapath) 
    
    data2 <- data2 %>% 
      mutate(row = str_sub(Position, start = 1, end = 1), 
             col = as.numeric(str_sub(Position, start = 2, end = -1))) %>% 
      arrange(Position)
    
    
    primer <- data2 %>% group_by(Position) %>%  filter(`Gene Name` != "Mengo") %>% 
      mutate(primer = `Gene Name`) %>% pull(primer)
    
    data2$primer <- rep(primer ,each = 2 )
    
    
    
    data2 <- data2 %>% arrange(col, row) %>% 
      mutate(replicate = rep(c(1,1,2,2), nrow(data2)/4), 
             # assign primers 
             `Gene Name` = ifelse(`Gene Name` == "Mengo", paste(primer, "Mengo"), `Gene Name`)) %>% 
      mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq)))
    
    
    
    #  HOW MANY SAMPLES ARE THERE
    n_samples <-  data2 %>% filter(grepl(paste0("^", prefix, "[0-9]"), ignore.case = T, x = `Sample Name`), 
                                                    Dye %in% c("VIC", "Cy5"), 
                                                    `Gene Name` == "M-Prot Mengo") %>% 
                                                    nrow()
    
    n_samples = n_samples/(input$n_replicate)
    
    sample_names <- data2 %>% 
      dplyr::select(`Sample Name`) %>% unique() %>% pull()
    
    
    
    NTC <- data2 %>% filter(grepl("NTC", `Sample Name`, ignore.case = T))  %>% mutate(Color = NULL)
    NPC <- data2 %>% filter(grepl("NPC", `Sample Name`, ignore.case = T))  %>% mutate(Color = NULL)
    PPC_E <- data2 %>% filter(grepl("PPC E", `Sample Name`, ignore.case = T))  %>% mutate(Color = NULL)
    PPC_P <- data2 %>% filter(grepl("PPC P", `Sample Name`, ignore.case = T))  %>% mutate(Color = NULL)
    
    
    # calculate NPC mean and use as baseline for control
    #TODO
    
    NPC_mean <- NPC %>% filter(Dye %in%  c("VIC", "Cy5")) %>% summarise(mean = mean(Cq)) %>% pull(mean)
    
    
    ###  VARIABLE SETTINGS
    
    
    output$settings <- renderTable({
      
      data.frame(  Name = c("# samples",
                            "# replicates",
                             "max delta Ct",
                            "Range of Ct values",
                            "max delta to NPC"), 
                   Value = c(as.character(n_samples),
                             as.character(input$n_replicate),
                             as.character(input$deltaCT),
                             paste(paste("min: ", input$extRange[1]), 
                             paste("max: ", input$extRange[2]), collapse = " "),
                             as.character(input$maxdeltaNPC) 
                             ))
    })
    
  
    output$settingsF <- renderTable({
      
      data.frame(  Name = c("max delta Ct",
                 "Range of Ct values",
                 "NPC average Ct", 
                 "max delta to NPC"),
                  Value = c(as.character(input$deltaCT),
                            paste(paste("min: ", input$extRange[1]), 
                                  paste("max: ", input$extRange[2]), collapse = " "),
                            
                              as.character(NPC_mean), 
                            
                            as.character(input$maxdeltaNPC)))
    })
    

    
    output$NPC_mean <- renderText( {
      
      paste0(c("Average Ct values for NPC: ", NPC_mean, "\nSelected dye: ", input$dye))
       
    })
    #NPC_range compared with other samples VIC values +- 2 
    

  
    
    #######################################################################  
    #######################################################################  
    
    # MENGO CONTROL 
    
    #######################################################################  
    #######################################################################  
    
    
    output$Mengo <- renderPlotly({
      
      pm <- data2 %>% filter(grepl("Mengo", `Gene Name`)) %>% 
        #mutate(Cq = ifelse(Cq == 0, NA, Cq)) %>% 
        ggplot(aes(`Sample Name`, Cq, color = `Gene Name`)) + geom_point(size = 3) +
        ylab("Ct")+
        xlab("samples")+
        theme_bw(base_size = 12)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
     
      pm2 <- ggplotly(pm) %>% layout(height = 300)
       pm2 
    })
    
    
    output$Influenza <- renderPlotly({
      
      pm <- data2 %>% filter(!grepl("Mengo", `Gene Name`)) %>% 
        #mutate(Cq = ifelse(Cq == 0, NA, Cq)) %>% 
        ggplot(aes(`Sample Name`, Cq, color = `Gene Name`)) + geom_point(size = 3) +
        ylab("Ct")+
        xlab("samples")+
        theme_bw(base_size = 12)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      pm2 <- ggplotly(pm) %>% layout(height = 300)
      pm2 
    })
    
    
    
    
    #######################################################################  
    #######################################################################  
    
    # FOR ALL PROBES  TESTs and Validity : 
    
    #######################################################################  
    #######################################################################  
    
    
    # todo : 
    #        - add raw values to the output report 
    
    
    
  # prepare all the necessary tests and requirements (process controls)
    
    
    meanFAM <- data2 %>% filter(grepl(paste0("^", prefix, "[0-9]"), ignore.case = T, x = `Sample Name`), Dye == "FAM") %>% 
      group_by(`Sample Name`, `Gene Name`) %>% filter(Cq != 0) %>% 
      summarise(meanCt = mean(Cq), 
                deltaCt = round(abs(Cq[1] - Cq[2]),2), 
                replicate1 = Cq[1], 
                replicate2 = Cq[2]) 
    
    
    
    meanVIC <-   data2 %>%  
      filter(grepl(paste0("^", prefix, "[0-9]"), ignore.case = T, x = `Sample Name`), Dye %in% c("VIC", "Cy5")) %>% 
      group_by(`Sample Name`) %>% filter(Cq != 0) %>% 
      
      summarise(meanMengoCt = mean(Cq)) 
    
    
    
    out1 <- left_join(meanFAM, meanVIC)  %>%  
      pivot_wider(names_from  = `Gene Name`, values_from = c(meanCt, deltaCt, meanMengoCt))
     
    
    
    
    # EXTRACTION CONTROL 
    meanVIC <- meanVIC %>% mutate("test1" = ifelse(meanMengoCt > input$extRange[1] & 
                                meanMengoCt < input$extRange[2], 
                                "passed", "failed"), 
                       "test2" = ifelse(meanMengoCt < NPC_mean + input$maxdeltaNPC & 
                                          meanMengoCt > NPC_mean - input$maxdeltaNPC, 
                                "passed", "failed")) #%>% select(c(1,2,4,5)) 
    # %>% 
    #   pivot_wider(names_from  = `Gene Name`, values_from = c(test1, test2))
    #   
    
    
    out2 <- left_join(meanFAM, meanVIC)  %>%  
      pivot_wider(names_from  = `Gene Name`, values_from = c(meanCt, deltaCt, meanMengoCt, 
                                                             test1, test2)) %>% 
      
    #adjust HERE
      mutate("valid" = ifelse(`test1_M-Prot` == "passed" &
                                `test2_M-Prot`  == "passed", "yes", "no"),
             "H3 test" = ifelse(`meanCt_H3` > input$extRange[1] &
                                `meanCt_H3` < input$extRange[2],
                              "positive", "negative"),
             "M-Prot test" = ifelse(`meanCt_M-Prot` > input$extRange[1] &
                                `meanCt_M-Prot` < input$extRange[2],
                              "positive", "negative")) %>%
      mutate( "H3 + M-Prot test" = ifelse(`H3 test` == "positive" & `M-Prot test` == "positive",
                                          "positive", ifelse(`H3 test` == "negative" & `M-Prot test` == "negative",
                                                             "negative", "unclear"  )),
             "Overall validity" = ifelse(valid == "yes", "yes", "no"))   %>% 
      dplyr::select(c(1, 2, 4, 3, 5, 8,10, 12:16))

    
    # 
    names(out2) <- c("sample Name",
                     "H3 meanCt FAM",
              "H3 delta Ct FAM",
              "M-Prot meanCt FAM",
                     "M-Prot delta Ct FAM",
                     "Mengo positiv", "< 2 delta Ct to NPC",
                     "Valid",
                     "H3 test",
                     "M-Prot test",
                     "H3 + M-Prot test", "Overall validity")

    
      
    
    
    
    output$report <- renderReactable({
      
      reactable(out2,
                rowStyle = function(index) {
                  if(out2[index,2] > input$extRange[1] & 
                      out2[index,2] < input$extRange[2] ) {
                    list(fontWeight = "bold", background = "#abf7b1")}}, 
                defaultPageSize = 15
                )
      
      
      })
    
    

    
    
##  COLORS
  
# green light #abf7b1
    
# redish light 
    
    
    
    
    
           
      
      
      
    
    
    
# NEGATIVE CONTROL NTC non template control  H2O

    output$neg1 <- renderTable({
      NTC %>% dplyr::select(all_of(c("Position",	"Sample Name",	"Gene Name",	"Cq" , 
      "Sample Type" ,  "Replicate Group"     ))) 
      })
    
    
    output$negative1 <- renderPlot({
     
      if (input$check == FALSE){
        # PLOT replicates
        pNTC1<- NTC %>% 
          mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
          group_by(`Gene Name`, Dye) %>% 
          unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
          #TEST
          mutate(Color = ifelse(Cq < input$ctrlCT, "green", "red"), 
                 Cq = ifelse(Cq < input$ctrlCT, "passed", "failed")) %>% 
        
          ggplot(aes(factor(replicate), full, fill = Color)) + 
          geom_tile(color = "black") +
          ylab("") +
          scale_fill_manual(values = c("#abf7b1", "red"))+
          geom_text(aes(label = Cq), color = "black", size = 6) +
          xlab("Replicates") +
          coord_equal() +
          theme_bw(base_size = 15) +
          theme(legend.position = "none")
           
        #implement color change as well for delta CT
        pNTC2 <- NTC %>% 
          mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
          group_by(`Sample Name`, `Gene Name`, Dye) %>% 
          mutate("average Ct" = mean(Cq)) %>%  ungroup() %>% group_by(`Gene Name`, Dye) %>% 
          mutate("delta Ct" = round(abs(Cq[1] - Cq[2]),1)) %>% 
          
          unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
          pivot_longer(c(`average Ct`, `delta Ct`), names_to = "mean", values_to = "Ct") %>% 
          #TEST
          mutate(Color = ifelse(Ct < input$ctrlCT & mean == "average Ct", "green", 
                                ifelse(Ct < input$deltaCT & mean == "delta Ct", "green", "red"))) %>% 
          
          
          ggplot(aes(mean, full, fill = Color)) + 
          geom_tile(color = "black") +
          ylab("") +
          xlab("") +
          scale_fill_manual(values = c("#abf7b1", "red"))+
          geom_text(aes(label = Ct), color = "black", size = 6) +
          coord_equal() +
          theme_bw(base_size = 15)+
          
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1))
        
        
        
         
        
        
      } else {
        pNTC1<- NTC %>% 
          mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
          group_by(`Gene Name`, Dye) %>% 
          unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
          #mutate("Cq_rep" = paste0("Cq_", replicate)) %>% 
          ggplot(aes(factor(replicate), full, fill = Cq)) + 
          #  ggplot(aes(factor(replicate), full, fill = meanCt)) + 
          geom_tile(color = "black") +
          ylab("") +
          geom_text(aes(label = Cq), color = "black", size = 6) +
          xlab("Replicas") +
          coord_equal() +
          theme_bw(base_size = 15) +
          theme(legend.position = "none")
        
        #PLOT averages
        pNTC2 <- NTC %>% 
          mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
          group_by(`Sample Name`, `Gene Name`, Dye) %>% 
          mutate("average Ct" = mean(Cq)) %>%  ungroup() %>% group_by(`Gene Name`, Dye) %>% 
          mutate("delta Ct" = round(abs(Cq[1] - Cq[2]),1)) %>% 
          
          unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
          pivot_longer(c(`average Ct`, `delta Ct`), names_to = "mean", values_to = "Ct") %>% 
          
          ggplot(aes(mean, full, fill = Ct)) + 
          geom_tile(color = "black") +
          ylab("") +
          xlab("") +
          geom_text(aes(label = Ct), color = "black", size = 6) +
          coord_equal() +
          theme_bw(base_size = 15)+
          
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1))
    
        
      }
        
      
      
      
      
       pNTC1 + pNTC2
      
      
    
    })
    
    
# NEGATIVE PROCESS CONTROL NPC     
      
    output$neg2 <- renderTable({
      NPC %>% dplyr::select(all_of(c("Position",	"Sample Name",	"Gene Name",	"Cq" , 
                              "Sample Type" ,  "Replicate Group"     ))) 
      })
    
    
    output$negative2 <- renderPlot({
      
      
      if (input$check == FALSE){
        # PLOT replicates
        pNPC1<- NPC %>% 
          mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
          group_by(`Gene Name`, Dye) %>% 
          unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
          #TEST
          mutate(Color = ifelse(Cq < input$ctrlCT, "green", "red"), 
                 Cq = ifelse(Cq < input$ctrlCT, "passed", "failed")) %>% 
          
          ggplot(aes(factor(replicate), full, fill = Color)) + 
          geom_tile(color = "black") +
          ylab("") +
          scale_fill_manual(values = c("#abf7b1", "red"))+
          geom_text(aes(label = Cq), color = "black", size = 6) +
          xlab("Replicates") +
          coord_equal() +
          theme_bw(base_size = 15) +
          theme(legend.position = "none")
        
        #implement color change as well for delta CT
        pNPC2 <- NPC %>% 
          mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
          group_by(`Sample Name`, `Gene Name`, Dye) %>% 
          mutate("average Ct" = mean(Cq)) %>%  ungroup() %>% group_by(`Gene Name`, Dye) %>% 
          mutate("delta Ct" = round(abs(Cq[1] - Cq[2]),1)) %>% 
          
          unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
          pivot_longer(c(`average Ct`, `delta Ct`), names_to = "mean", values_to = "Ct") %>% 
          #TEST
          mutate(Color = ifelse(Ct < input$ctrlCT & mean == "average Ct", "green", 
                                ifelse(Ct < input$deltaCT & mean == "delta Ct", "green", "red"))) %>% 
          
          
          ggplot(aes(mean, full, fill = Color)) + 
          geom_tile(color = "black") +
          ylab("") +
          xlab("") +
          scale_fill_manual(values = c("#abf7b1", "red"))+
          geom_text(aes(label = Ct), color = "black", size = 6) +
          coord_equal() +
          theme_bw(base_size = 15)+
          
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank() ,
                axis.text.x = element_text(angle = 90, hjust = 1))
    
      } else {
      
      # PLOT replicates
      pNPC1<- NPC %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Gene Name`, Dye) %>% 
        unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
        #mutate("Cq_rep" = paste0("Cq_", replicate)) %>% 
        ggplot(aes(factor(replicate), full, fill = Cq)) + 
        #  ggplot(aes(factor(replicate), full, fill = meanCt)) + 
        geom_tile(color = "black") +
        ylab("") +
        geom_text(aes(label = Cq), color = "white", size = 6) +
        xlab("Replicates") +
        coord_equal() +
        theme_bw(base_size = 16) +
        theme(legend.position = "none")
        
      
      
      #PLOT averages
      pNPC2 <- NPC %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Sample Name`, `Gene Name`, Dye) %>% 
        mutate("average Ct" = mean(Cq)) %>%  ungroup() %>% group_by(`Gene Name`, Dye) %>% 
        mutate("delta Ct" = round(abs(Cq[1] - Cq[2]),1)) %>% 
        
        unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
        pivot_longer(c(`average Ct`, `delta Ct`), names_to = "mean", values_to = "Ct") %>% 
        
        ggplot(aes(mean, full, fill = Ct)) + 
        geom_tile(color = "black") +
        ylab("") +
        xlab("") +
        geom_text(aes(label = Ct), color = "white", size = 6) +
        coord_equal() +
        theme_bw(base_size = 16)+
        
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank() ,
              axis.text.x = element_text(angle = 90, hjust = 1))
    
      }
      
      
      pNPC1 + pNPC2
      
      
    })
    
    
# POSITIVE PCR CONTROL ERREGER  PPCE    
    
    output$pos1 <- renderTable({
      PPC_E %>% dplyr::select(all_of(c("Position",	"Sample Name",	"Gene Name",	"Cq" , 
                              "Sample Type" ,  "Replicate Group"     ))) 
      })
    
    output$positive1 <- renderPlot({
      
      # PLOT replicates
      pNPC1<- PPC_E %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Gene Name`, Dye) %>% 
        unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
        #mutate("Cq_rep" = paste0("Cq_", replicate)) %>% 
        ggplot(aes(factor(replicate), full, fill = Cq)) + 
        #  ggplot(aes(factor(replicate), full, fill = meanCt)) + 
        geom_tile(color = "black") +
        ylab("") +
        geom_text(aes(label = Cq), color = "white", size = 6) +
        xlab("Replicates") +
        coord_equal() +
        theme_bw(base_size = 16) +
        theme(legend.position = "none")
      
      
      #PLOT averages
      pNPC2 <- PPC_E %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Sample Name`, `Gene Name`, Dye) %>% 
        mutate("average Ct" = mean(Cq)) %>%  ungroup() %>% group_by(`Gene Name`, Dye) %>% 
        mutate("delta Ct" = round(abs(Cq[1] - Cq[2]),1)) %>% 
        
        unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
        pivot_longer(c(`average Ct`, `delta Ct`), names_to = "mean", values_to = "Ct") %>% 
        
        ggplot(aes(mean, full, fill = Ct)) + 
        geom_tile(color = "black") +
        ylab("") +
        xlab("") +
        geom_text(aes(label = Ct), color = "white", size = 6) +
        coord_equal() +
        theme_bw(base_size = 16)+
        
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank() )
      
      
      
      pNPC1 + pNPC2
      
    })
    
    
# POSITIVE PCR CONTROL PCR  PPC_P    
    
    output$contents1 <- renderReactable({
      data2 %>% mutate(Color = NULL) %>% 
      reactable( height = 250, pagination = F)
    
         })
    
    
    output$positive2 <- renderPlot({
      
      
      # PLOT replicates
      pNPC1<- PPC_P %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Gene Name`, Dye) %>% 
        unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
        #mutate("Cq_rep" = paste0("Cq_", replicate)) %>% 
        ggplot(aes(factor(replicate), full, fill = Cq)) + 
        #  ggplot(aes(factor(replicate), full, fill = meanCt)) + 
        geom_tile(color = "black") +
        ylab("") +
        geom_text(aes(label = Cq), color = "white", size = 6) +
        xlab("Replicates") +
        coord_equal() +
        theme_bw(base_size = 16)  +
        theme(legend.position = "none")

      
      
      #PLOT averages
      pNPC2 <- PPC_P %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Sample Name`, `Gene Name`, Dye) %>% 
        mutate("average Ct" = mean(Cq)) %>%  ungroup() %>% group_by(`Gene Name`, Dye) %>% 
        mutate("delta Ct" = round(abs(Cq[1] - Cq[2]),1)) %>% 
        
        unite("full", `Gene Name`, Dye, sep = "  ")  %>% 
        pivot_longer(c(`average Ct`, `delta Ct`), names_to = "mean", values_to = "Ct") %>% 
        
        ggplot(aes(mean, full, fill = Ct)) + 
        geom_tile(color = "black") +
        ylab("") +
        xlab("") +
        geom_text(aes(label = Ct), color = "white", size = 6) +
        coord_equal() +
        theme_bw(base_size = 16)+
        
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank() )
      
      
      
      pNPC1 + pNPC2
      
      
    })
    
    
    
    
    dyeS <- input$dye
    
    
    
    
# WHOLE PLATE   
    
    output$plate <- renderPlot({
      
      #set color according to threshold input 
      
      p1 <- data2 %>% 
        #mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        filter(Dye == input$dye) %>% 
        mutate(color = ifelse(Cq > input$extRange[1] & Cq < input$extRange[2], "ok", "fail"), 
               Cq = round(Cq, 2)) %>% 
        
        mutate(row = rep(c("H3 R1", "M-Prot R1", "H3 R2", "M-Prot R2"), nrow(data2)/8), 
               `Sample Name` = factor(`Sample Name`, 
                      levels = c(sample_names))) %>%  #, "PPC P","PPC E" , "NTC","NPC"   ))) %>% 
        #                c(paste0(rep("P",n_samples), stri_pad_left(n_samples:1, 3, 0)),
                      #                    ))) %>% 
        ggplot(aes(row, `Sample Name`, 
                   fill = color), text = paste0(`Sample Name`, "\n", "Cq: ", Cq)) + #alpha = `Gene Name`)) + 
        geom_tile(color = "black") +
        ylab("") +
        scale_fill_manual(values = c( "red","#abf7b1"))+
        #scale_alpha_discrete(range = c(0.6, 1))+
        xlab("") +
        coord_equal() +
        geom_text(aes(label = paste0(`Sample Name`, "\n", "Cq: ", Cq)), color = "black", size = 3) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      
      # p1 <- data2 %>%
      #   mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
      #   filter(Dye == input$dye) %>% 
      #   mutate(color = ifelse(Cq > input$extRange[1] & Cq < input$extRange[2], "ok", "fail"), 
      #                                                Cq = round(Cq, 2)) %>%  
      #   
      #   
      #   ggplot(aes(factor(col), forcats::fct_rev(reorder(row, row)), 
      #              fill = color), text = paste0(`Sample Name`, "\n", "Cq: ", Cq)) + #alpha = `Gene Name`)) + 
      #   geom_tile(color = "black") +
      #   ylab("") +
      #   scale_fill_manual(values = c( "red","#abf7b1"))+
      #   #scale_alpha_discrete(range = c(0.6, 1))+
      #   xlab("") +
      #   coord_equal() +
      #   geom_text(aes(label = paste0(`Sample Name`, "\n", "Cq: ", Cq)), color = "black", size = 3)  
      
      p1 + ggtitle("<span style='font-size: 20pt;'>Plate Overview</font>") + 
        theme(plot.title = element_markdown())
      
      #ggplotly(p1, tooltip = list("text"))
      
    }, height = 1000, width = 700)
    
    
    
    
    
    
#  WHOLE PLATE  MAX DELTA TEST
    
    output$delta <- renderPlot({
    
      d1 <- data2 %>%  filter(Dye == input$dye) %>% 
        mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
        group_by(`Sample Name`, `Gene Name`) %>% 
        mutate(       diffCt = round(abs(Cq[1] - Cq[2]),2)) %>% 
        mutate(color = ifelse(diffCt < input$deltaCT, "ok", "xfail")) %>% 
        
        dplyr::select(all_of(c("Sample Name", "Gene Name", "color", "diffCt"))) %>% 
        unique() %>% 
        mutate(`Sample Name` = factor(`Sample Name`,
                                      levels = c(sample_names,
                                                 "PPC P","PPC E" , "NTC","NPC"  ))) %>%
        
        ggplot(aes(`Gene Name`, `Sample Name`, 
                   fill = color), text = paste0(`Sample Name`, "\n", "delta CT: ", diffCt)) + #alpha = `Gene Name`)) + 
        geom_tile(color = "black") +
        ylab("") +
        scale_fill_manual(values = c( "#abf7b1","red"))+ 
        geom_text(aes(label = paste0("delta Ct: ", diffCt)), color = "black", size = 4) 
      
      
      d1 + ggtitle("<span style='font-size: 20pt;'>Delta Ct</font>") +
        theme(plot.title = element_markdown())
      
      # 
      # d1 <- data2 %>%  filter(Dye == input$dye) %>% 
      #   mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
      #   group_by(`Sample Name`, `Gene Name`) %>% 
      #   mutate(meanCt = mean(Cq), 
      #          diffCt = round(abs(Cq[1] - Cq[2]),2)) %>% 
      #   
      #     mutate(color = ifelse(diffCt < input$deltaCT, "ok", "xfail")
      #                                                 ) %>%
      #   
      #   ggplot(aes(factor(col), forcats::fct_rev(reorder(row, row)), fill = color)) +  
      #   geom_tile(color = "black") +
      #   ylab("") +
      #   scale_fill_manual(values = c( "#abf7b1","red"))+
      #   #scale_alpha_discrete(range = c(0.6, 1))+
      #   xlab("") +
      #   coord_equal() +
      #   geom_text(aes(label = diffCt), color = "white", size = 4)  
      #   
      #   
      
      
    
    }, height = 900, width = 400)

    
    
    ###########################################################
    ###########################################################
    
####  DOWNLOAD HANDLER
    
    ###########################################################
    ###########################################################
    
    
    posStyle1 <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE") # green 
    posStyle2 <- createStyle(fontColour = "#ff0000", bgFill = "#FEC2CC")  # red 
    posStyle3 <- createStyle(fontColour = "#000000", bgFill = "#FFFFFF")  # white 
    
    
    hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                      halign = "center", valign = "center", textDecoration = "Bold",
                      border = "TopBottomLeftRight" , textRotation = 90)
    
    
    output$downloadDa <- downloadHandler(
      filename = function() { paste(input$downloadData, "_",Sys.Date(),".xlsx",sep="") },
      content = function(file) {
    
          
            wb <- createWorkbook()
            addWorksheet(wb, "sheet1")
            addWorksheet(wb, "graph")
            
            addStyle(wb, "sheet1", cols = 1:12, rows = 1, style = hs)
            writeData(wb, "sheet1", out2)
            
            
            # create plot and write to excel 
            
            png("qPCR_curves.png", width = 26, height = 18, unit = "cm", res = 200)
            
	    p1 <- ggplot(fdata, aes(cyc, fluor)) +
              geom_line(aes(group = fdata.name,
                            color = sample))+ facet_grid(~ target) +
              xlab("cycles") +
              ylab("fluorescence")  
            
	    print(p1)
	    dev.off()

            insertImage(wb, "graph", "qPCR_curves.png", width = 26, height = 18, unit = "cm")
            
            conditionalFormatting(wb, "sheet1", cols = 2, rows = 2:14, 
                                  rule = " < 36", type = "expression", style = posStyle1)
            conditionalFormatting(wb, "sheet1", cols = 2, rows = 2:14, 
                                  rule = " < 6", type = "expression", style = posStyle3)
            
            conditionalFormatting(wb, "sheet1", cols = 4, rows = 2:14, 
                                  rule = " < 36", type = "expression", style = posStyle1)
            conditionalFormatting(wb, "sheet1", cols = 4, rows = 2:14, 
                                  rule = " < 6", type = "expression", style = posStyle3)
            
            conditionalFormatting(wb, "sheet1", cols = 3, rows = 2:14, 
                                  rule = " > 2", type = "expression", style = posStyle2)
            conditionalFormatting(wb, "sheet1", cols = 5, rows = 2:14, 
                                  rule = " > 2", type = "expression", style = posStyle2)
            
            #conditionalFormatting(wb, "sheet1", cols = 9:11, rows = 2:14, 
            #                      rule = "== `positive`", type = "expression", style = posStyle2)
            
            saveWorkbook(wb, file = file, overwrite = TRUE)
            #write.xlsx(out1, file, rowNames=F, headerStyle = hs)
              
            }
    )
    
    
    })
    
    
    
    

  
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
  dat <- reactive({
    
    #input input$slider
    
    d1 <-  data.frame(sample = 
                 
                 c("NPC", "NTC", "PPC E", "PPC P", 
                   paste0(rep("sample", input$slider), seq(1,input$slider)), 
                   rep("empty", 24 - (4 + input$slider))),
               color = c(rep("control", 4), rep("sample", input$slider), rep("empty", 24 - (4 + input$slider))), 
               id = seq(1,24), 
               col = c(rep(1,8), rep(2,8), rep(3,8)), 
               row = c(rep(letters[1:8],3))) 
        d1    
    
            })
  

    
  
   dat1 <-  reactive({  d2 <- rbind(dat(), dat(), dat(), dat()) %>% 
     arrange(match(sample, c("NPC", "NTC", "PPC E", "PPC P", paste0(rep("sample", input$slider), seq(1,input$slider)), 
                             rep("empty", 24 - (4 + input$slider))))) %>% 
     mutate(col = rep(1:12, each = 8), 
            replicate = rep(1:2, 48),
            row = c(rep(letters[1:8],12)),
            duplex = rep(c(rep("M-Prot FAM & Mengo VIC", 2), rep("H3 FAM & Mengo VIC", 2)) ,24)) %>% 
            #dye = rep(c("FAM", "VIC"), 48)) %>% 
            unite("duprep", duplex, replicate, remove = F )  
          d2
   })
  
   
   
  ### output data
   
   myout <- reactive({
     mylist = NULL
     mylist[1] <- dat1()
     names(mylist) <- "dataframe1"
     return(mylist)
   })
   
   
   
   
   
   
   
   
   
   
   
   


   
#####################################################################
#
#    EXAMPLE RUN data
#
 #####################################################################
   
#    
#    
#    
# #    
# #    
# 
# fq <- readxl::read_excel("C:/Users/michelmo/DATA/LABOR_SPIEZ/WK_app/L001_12.09.2022_WK.xlsx")
# fq
# 
# n_replicates <- 2
# 
# # get number of samples
# n_samples <- fq %>% filter(grepl("^P[0-9]", ignore.case = T, x = `Sample Name`),
#                            Dye == "FAM",
#                            `Gene Name` == "M-Prot") %>% nrow()/n_replicates
# 
# 
# 
# 
# fq <- fq %>% mutate(row = str_sub(Position, start = 1, end = 1),
#          col = as.numeric(str_sub(Position, start = 2, end = -1))) %>%
#   arrange(col, row) %>%
#   mutate(replicate = rep(c(1,1,2,2), nrow(fq)/4),
#          `Gene Name` = rep(c("M-Prot Mengo", "M-Prot", "M-Prot Mengo", "M-Prot",
#                              "H3 Mengo", "H3", "H3 Mengo", "H3"), nrow(fq)/8)) %>%
#   mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq)))
# 
# 
# # inspect Mengo control
# 
# fq %>% filter(grepl("Mengo", `Gene Name`)) %>%
#   #mutate(Cq = ifelse(Cq == 0, NA, Cq)) %>%
#   ggplot(aes(`Sample Name`, Cq)) + geom_point(size = 4) +
#   theme_bw(base_size = 12)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# n_samples:1
# 
#    sample_names <- fq %>% filter(grepl("^P[0-9]", `Sample Name`)) %>% 
#      select(`Sample Name`) %>% unique() %>% pull()
#    
# sample_names   
#    
# 
# 
# # control Mengo on plate
# fq %>% 
#   mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
#   filter(Dye == "Cy5") %>% 
#   mutate(color = ifelse(Cq > 12 & Cq < 36, "ok", "fail"), 
#          Cq = round(Cq, 2))  %>% 
#   mutate(row = rep(c("H3 R1", "M-Prot R1", "H3 R2", "M-Prot R2"), nrow(fq)/8), 
#          `Sample Name` = factor(`Sample Name`, 
#                   levels = c(paste0(rep("P",n_samples), stri_pad_left(n_samples:1, 3, 0)),
# "PPC P","PPC E" , "NTC","NPC"  ))) %>% 
#   
#   
#   ggplot(aes(row, `Sample Name`, 
#              fill = color), text = paste0(`Sample Name`, "\n", "Cq: ", Cq)) + #alpha = `Gene Name`)) + 
#   geom_tile(color = "black") +
#   ylab("") +
#   scale_fill_manual(values = c( "red","#abf7b1"))+
#   #scale_alpha_discrete(range = c(0.6, 1))+
#   xlab("") +
#   coord_equal() +
#   geom_text(aes(label = paste0(`Sample Name`, "\n", "Cq: ", Cq)), color = "black", size = 3) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# 
# 
# # delta Ct on plate
# 
# 
# 
# fq %>%  filter(Dye == "Cy5") %>%
#   mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>%
#   group_by(`Sample Name`, `Gene Name`) %>%
#   mutate(       diffCt = round(abs(Cq[1] - Cq[2]),2)) %>%
#   mutate(color = ifelse(diffCt < 2, "ok", "xfail")) %>%
#   select(all_of(c("Sample Name", "Gene Name", "color", "diffCt"))) %>%
#   unique() %>%
#   mutate(`Sample Name` = factor(`Sample Name`,
#                               levels = c(sample_names,
#                                          "PPC P","PPC E" , "NTC","NPC"  ))) %>%
#   ggplot(aes(`Gene Name`, `Sample Name`,
#              fill = color), text = paste0(`Sample Name`, "\n", "delta CT: ", diffCt)) + #alpha = `Gene Name`)) +
#   geom_tile(color = "black") +
#   ylab("") +
#   scale_fill_manual(values = c( "#abf7b1","red"))+
#   geom_text(aes(label = paste0("delta Ct: ", diffCt)), color = "black", size = 4)

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dNTC <- fq %>% filter(grepl("NTC", `Sample Name`, ignore.case = T))  %>% mutate(Color = NULL)
# dNPC <- fq %>% filter(grepl("NPC", `Sample Name`, ignore.case = T))  %>% mutate(Color = NULL)
# dNTC
# dNPC
# 
# 
# 
# 
# # format color according to thresholds
# 
# 
# 
# tplot <- fq %>% mutate(Cq = as.numeric(gsub(pattern = "-", replacement = 0, x = Cq))) %>% 
#   group_by(`Sample Name`, `Gene Name`, Dye) %>% mutate(meanCt = mean(Cq), 
#                                       diffCt = abs(Cq[1] - Cq[2]))  %>%  
#   filter(Dye == "VIC") %>% 
# 
# ggplot(aes(col, forcats::fct_rev(reorder(row, row)), fill = meanCt)) + 
#         #   alpha = factor(replicate))) +
#      geom_tile(color = "black") +
#      coord_equal() +
#      geom_text(aes(label = `Sample Name`), color = "white", size = 4) +
#      theme_bw(base_size = 16) + xlab("") + ylab("") + ggtitle("LightCycler layout") +
#      theme(plot.title = element_text(hjust = 0.5)) + scale_alpha_discrete(range = c(0.5,1)) 
# 
# 
# tplot
# #ggplotly(tplot)
# 
#    
# # run all tests and controlls 
# 
# NPC_mean <- dNPC %>% filter(Dye == "VIC") %>% summarise(mean = mean(Cq)) %>% pull(mean)
# NPC_mean 
# 
# #max delta to NPC == 2
# 
# ctrlCT <- 30
# Dcutoff2 <- 2
# 
# 
# 
# 
# 
# # })
# # 
# # observeEvent(
# #   #setting dye to look at 
# #   c(input$dye, input$check, input$ctrcCT, input$deltaCT)
# #   {
# #     
# 
# 
# # FOR ALL PROBES: 
# 
# # prepare all the necessary tests and requirements (process controls)
# 
# #mean and delta
# meanFAM <- fq %>% filter(grepl("sample", ignore.case = T, x = `Sample Name`), Dye == "FAM") %>% 
#   group_by(`Sample Name`, `Gene Name`) %>% 
#   summarise(meanCt = mean(Cq), 
#             deltaCt = abs(Cq[1] - Cq[2])) %>% 
#   
# 
#   mutate(n = as.numeric(str_replace(`Sample Name`, "sample", ""))) %>% 
#   arrange(n)  
#   
# 
# meanVIC <-   fq %>%  
#   filter(grepl("sample", ignore.case = T, x = `Sample Name`), Dye == "VIC") %>% 
#   group_by(`Sample Name`, `Gene Name`) %>% 
#   summarise(meanMengoCt = mean(Cq)) %>% 
#     mutate(n = as.numeric(str_replace(`Sample Name`, "sample", ""))) %>% 
#     arrange(n)  
#   
#   
# out2 <- left_join(meanFAM, meanVIC)  %>%  
#   pivot_wider(names_from  = `Gene Name`, values_from = c(meanCt, deltaCt, meanMengoCt)) %>% 
#   select(c(1,3,5,4,6))
# 
# names(out2) <- c("sample Name", "H3 meanCt FAM", "H3 delta Ct FAM", "M-Prot meanCt FAM", "M-Prot meanCT FAM")
# 
# 
# 
  
   
   
   

#9ec*aGz44AaX

   
   
# SLIDER 
   
  output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
        })
  
  
  
  ## ggplot with samples to show
  
  
  output$plot_box <- renderPlotly({
    
    b1 <- ggplot(dat() ,aes(col, forcats::fct_rev(reorder(row, row)), fill = factor(color))) +
    geom_tile(color = "black") + 
    coord_equal() + 
    geom_text(aes(label = sample), color = "white", size = 3) + 
      theme_bw(base_size = 16) +
      xlab("") +
      ylab("") +
      ggtitle("QIAgility layout") +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_fill_hue(name = "Sample types", labels = c("control", "sample", "empty"))  
    
    
    ggplotly(b1)
    
  })
  

  output$plot_LC <- renderPlotly({
    
    b2 <- ggplot(dat1() ,aes(col,
                             #row,
                             forcats::fct_rev(reorder(row, row)),
                             fill = color, alpha = factor(duprep))) +
      #scale_fill_manual(values = c("red"))
      scale_alpha_manual(values = c(0.7,0.6, 1, 0.9))+
      geom_tile(color = "black") + 
      coord_equal() + 
      geom_text(aes(label = sample), color = "white", size = 3) + 
      theme_bw(base_size = 16) + 
      xlab("") + 
      ylab("") +
      ggtitle("LightCycler 96-plate layout") +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
      scale_x_continuous(breaks = c(1:12))
      
    
    
    ggplotly(b2)
    
  })
  
  
  
  
  
  
  
  
  

}













shinyApp(ui, server)
