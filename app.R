library(shiny)
library(DT)
library(tidyverse)
library(bslib)
library(png)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(shiny)
library(tigris)
library(readxl)
library(haven)
library(readr)
library(sf)
library(htmltools)

safe_includeHTML <- function(path) {
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  # strip any bootstrap 3 link
  html <- gsub('<link[^>]*bootstrap[^>]*>', "", html, perl = TRUE)
  html <- gsub('<script[^>]*bootstrap[^>]*>[\\s\\S]*?</script>', '', html, perl = TRUE)
  html <- gsub('<script[^>]*jquery[^>]*>[\\s\\S]*?</script>',   '', html, perl = TRUE)
  html <- gsub('<link[^>]*font[-]?awesome[^>]*>', '', html, perl = TRUE)
  HTML(html)
}



#setwd("C:/Users/vince/OneDrive/Desktop/Rshiny")
center_data_1 = read_xls("data/csrs_final_tables_2505_KI.xls",sheet = 1)
center_data_2 = read_xls("data/csrs_final_tables_2505_KI.xls",sheet = 5)
center_data = data.frame(center_data_1,center_data_2)

all_counties_2023 <- read_sas("data/all_counties_2023.sas7bdat.filepart")
county_dsa <- all_counties_2023 %>%
  mutate(
    fips_cd = fips_cnty,
    opo_ctr_cd = ifelse(opo_ctr_cd == "DCTC", "MDPC", opo_ctr_cd),
    # Optional: Uncomment the next line if needed
    opo_ctr_cd = ifelse(opo_ctr_cd == "OHOV", "KYDA", opo_ctr_cd)
  )
load("data/us_counties.RData")
# opo_map <- us_counties %>%
#   left_join(county_dsa, by = "fips_cd") %>%
#   filter(!is.na(opo_ctr_cd)) %>%
#   group_by(opo_ctr_cd) %>%
#   summarise(geometry = st_union(geometry)) %>%
#   st_as_sf()
opo_map = readRDS("data/opo_map.rds")
set.seed(2024)
opo_map$color_cd = sample(1:54)
geo_info = read_xls("data/csrs_final_tables_22_11_KI.xls")
center_data = center_data[-1,]
geo_info = geo_info[-1,] %>% select(CTR_CD,Latitude,Longitude)
center_data = merge(center_data,geo_info,by = "CTR_CD") %>% select(ENTIRE_NAME,CTR_CD,TMR_CadTxR_c,Latitude,Longitude)

Site <- center_data[-c(195),]
#Site <- merge(Site,center_data[,c("CTR_CD","TMR_CadTxR_c")],all.x = TRUE,by="CTR_CD")
Site$Latitude <- as.numeric(Site$Latitude)
Site$Longitude <- as.numeric(Site$Longitude)
Site$TMR_CadTxR_c <- as.numeric(Site$TMR_CadTxR_c)

vars <- c(
  "Deceased Donor Transplant Rate" = "TMR_CadTxR_c"
)
# -------------------- Load data --------------------

excel_path <- "data/csrs_final_tables_2505_KI.xls"
sheet_list <- excel_sheets(excel_path)

# -------------------- UI ---------------------------
ui <- navbarPage(
  title = "Transplant Data Portal",
  theme = bs_theme(version = 5, bootswatch = "zephyr"),

  # bootswatch =
  # "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal",
  # "litera", "lumen", "lux", "materia", "minty", "morph",
  # "pulse", "quartz", "sandstone", "simplex", "sketchy", "slate",
  # "solar", "spacelab", "superhero", "united", "vapor", "yeti", "zephyr"

  # --- 1. Overview tab now shows a PDF ---
  tabPanel(
    "Overview",
    fluidRow(
      column(
        width = 12,
        h3("Dataset Overview"),
        tags$iframe(
          src   = "SRTR_Data_Dictionary.pdf",  # served from /www
          style = "width:100%; height:1100px; border:none;"
        )
      )
    )
  ),

  # --- 2. HTML – Data Dictionary (static) ---
  # tabPanel(
  #   "Data Dictionary (HTML)",
  #   tags$iframe(
  #     src   = "dataDictionary_utf8.html",
  #     style = "width:100%; height:900px; border:none;"
  #   )
  # ),

  tabPanel(
    "Data Dictionary",
    safe_includeHTML("www/dataDictionary_utf8.html")
  ),

  # # --- 3. Summary Report (HTML) -------------
  # tabPanel(
  #   "Summary Report (HTML)",
  #   tags$iframe(
  #     src   = "tx_ki_summary_custom1.html",
  #     style = "width:100%; height:1100px; border:none;"
  #   )
  # ),

  tabPanel(
    "Data Summary Report",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId  = "rep_year",
          label    = "Transplant Year (REC_TX_DT)",
          choices  = c("1987-2025 (Full)", "2015-2025 (Frequently Used)", "2015", "2016", "2017"
                       , "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
          selected = "1987-2025 (Full)"
        ),
        width = 2
      ),
      mainPanel(
        uiOutput("report_ui")
      )
    )
  ),


  # # --- 3. KDPI and EPTS -------------
  # tabPanel(
  #   "KDPI and EPTS",
  #   includeHTML("www/KDPI-and-EPTS-html.html")
  # ),

  # --- 3. KDPI and EPTS -------------
  tabPanel(
    "KDPI and EPTS",
    tags$head(
      tags$script(src = "iframeResizer.min.js")
    ),
    tags$iframe(
      src   = "KDPI-and-EPTS-html.html",
      style = "width:100%; height:1100px; border:none;"
    ),
    # activate resizer
    tags$script("iFrameResize({log:false, checkOrigin:false}, '#rep');")
  ),

  # # --- 3. KDPI and EPTS -------------
  # tabPanel(
  #   "KDPI and EPTS",
  #   tags$iframe(
  #     src   = "KDPI-and-EPTS-html.html",
  #     style = "width:100%; height:1100px; border:none;"
  #   )
  # ),
  # tabPanel(
  #   "Summary Report (HTML)",
  #   safe_includeHTML("www/tx_ki_summary_custom.html")
  # ),

  # tabPanel("Overview",
  #          fluidRow(
  #            column(
  #              width = 12,
  #              h3("Dataset Overview"),
  #              includeMarkdown("www/description.md")   # use iframe if PDF/HTML
  #            )
  #          )
  # ),

  # # --- 4. Interactive Dictionary (DT table) ---
  # tabPanel("Data Dictionary",
  #          DTOutput("dict_tbl")
  # ),

  # --- 5. Summary Statistics (interactive) ---
  # tabPanel("Summary Statistics",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("var_sum", "Choose a variable", choices = names(tx_ki))
  #            ),
  #            mainPanel(
  #              verbatimTextOutput("sum_text")
  #            )
  #          )
  # ),
  # --- 6. Outcome Definition ---
  tabPanel(
    "Variable Definition",
    sidebarLayout(
      sidebarPanel(
        selectInput("var_name", "Choose a variable", choices = c("KDPI","eGFR","Transplant Rate","Post-Transplant Survival",
                                                                "Pre-transplant Mortality Rate")),
        width = 2
      ),
      mainPanel(
        fluidRow(
          column(
            width = 12,
            h3("Variable Definition Table"),
            tableOutput("table_var"),
            h3("The following section shows example code on how to build these variables."),
            h3("You can choose the variable of interest from the left panel.")
          )
        ),
        verbatimTextOutput("code_var")
      )
    )
  ),

  tabPanel(
    "Center Data",
    sidebarLayout(
      sidebarPanel(
        ## drop-down that looks like a big button (optional shinyWidgets) ----
        selectInput(
          inputId  = "sheet",
          label    = "Select worksheet",
          choices  = sheet_list,
          selected = sheet_list[1]
        ),
        width = 2
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data table",   DTOutput("tbl")),
          tabPanel("Summary",      verbatimTextOutput("summary")),
          tabPanel("Histogram",    uiOutput("plot_ui"))
        )
      )
    )
  ),
  # --- Center Geographic Map ---
  tabPanel("Center Map",
           fluidRow(
             column(
               width = 12,
               h3("Center-level Information"),
               h5("Currently, 317 centers are included in the dataset. However, not all of them have available geographic information."),
               h5("In all, 232 centers have available geographic information and is displayed in the map below."),
               h5("Each circle represents a transplant center. You can click on the circle to see the name and the selected outcome.")
             )
           ),
           div(class="outer",
               leafletOutput("map", width = "70%", height = 600),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 500, height = "auto",

                             h2("Transplant explorer"),

                             selectInput(inputId =  "outcome",label =  "Outcome", choices = vars,selected ="Deceased Donor Transplant Rate"))

               #plotOutput("lineplot", height = 300),
               #plotOutput("histplot", height = 300))
           )
  ),
  # # --- 7. Explorer (plots) ---
  # tabPanel("Explorer",
  #          sidebarLayout(
  #            sidebarPanel(
  #              selectInput("var_plot", "Variable to plot", choices = names(tx_ki)),
  #              checkboxInput("by_group", "Display by group"),
  #              conditionalPanel(
  #                condition = "input.by_group == true",
  #                selectInput("grp_var", "Grouping variable", choices = names(tx_ki))
  #              )
  #            ),
  #            mainPanel(
  #              plotOutput("dist_plot", height = "500px")
  #            )
  #          )
  # ),
  # --- 8. Outcome Exploration ---
  tabPanel("Outcome Exploration",
           sidebarLayout(
             sidebarPanel(
               selectInput("group_plot", "Variable to group by", choices = c("Transplant Type","Age","Gender","Diabetes","Waiting Time")),
             ),
             mainPanel(
               h3("This panel explores post-transplant survival. The first plot is a 2-year Kaplan-Meier plot comparing waitlist to different transplant types."),
               img(src = "2yr Compare Version 2.png", align = "left", width = 1100, height = 900),
               h3("This panel explores post-transplant survival. The next plot is a 2-year Relative Risk plot comparing waitlist to different transplant types."),
               h3("You may indicate the grouping variable in the left panel."),
               img(src = "RR 2yr Compare.png", align = "left", width = 1100, height = 900)
               #plotOutput("outcome_plot", height = "500px")
             )
           )
  ),
  # # --- 7. Raw Data ---
  # tabPanel("Raw Data",
  #          DTOutput("raw_tbl")
  # ),

  # --- 9. About ---
  tabPanel("About",
           HTML("
      <p>This portal is maintained by XXX.</p>
      <p>Contact: <a href='mailto:XXX@email'>XXX@email</a></p>
    ")
  )
)

# -------------------- Server -----------------------
server <- function(input, output, session) {

# ## 1. Data dictionary ----
# dict <- tibble(
#   Variable = names(tx_ki),
#   Class    = sapply(tx_ki, class),
#   Missing  = colSums(is.na(tx_ki)),
#   Examples = sapply(tx_ki, \(x) paste0(head(unique(x), 3), collapse = ", "))
# )
#
#   output$dict_tbl <- renderDT(
#     dict,
#     options = list(pageLength = 12, scrollX = TRUE),
#     rownames = FALSE
#   )

  # output$dict_tbl <- renderDT({
  #   dat <- tibble(
  #     Variable = names(tx_ki),
  #     Class    = sapply(tx_ki, class),
  #     Missing  = colSums(is.na(tx_ki))
  #   )
  #   datatable(dat, options = list(pageLength = 12, scrollX = TRUE))
  # })

  # 3-2 Data Summary Report – dynamic iframe -------------------------------
  output$report_ui <- renderUI({
    req(input$rep_label)
    file_stub <- report_choices[[input$rep_label]]
    iframe_src <- sprintf("tx_ki_summary_%s.html", file_stub)

    tags$iframe(src   = iframe_src,
                style = "width:100%; height:1100px; border:none;")
  })

  # 3-3 Workbook Explorer – read selected sheet on demand ------------------
  sheet_data <- reactive({

    raw <- read_excel(
      path        = excel_path,
      sheet       = input$sheet,
      col_names   = FALSE
    )

    line1 <- raw %>% slice(1) %>% unlist(use.names = FALSE) %>% as.character()
    line2 <- raw %>% slice(2) %>% unlist(use.names = FALSE) %>% as.character()

    new_names <- paste0(line1, "(", line2, ")")
    new_names <- make.unique(new_names)

    df <- raw %>% slice(-c(1, 2))
    names(df) <- new_names

    df <- type_convert(df, na = c("", "NA"))

    date_candidates <- grepl("DATE|DT|_DT$", names(df), ignore.case = TRUE) &
      sapply(df, is.numeric)

    df[date_candidates] <- lapply(df[date_candidates],
                                  \(x) as.Date(x, origin = "1899-12-30"))

    df
  }) %>% bindCache(input$sheet)

  output$tbl <- renderDT({
    datatable(sheet_data(),
              escape     = FALSE,
              filter = "top",
              options = list(pageLength = 15, scrollX = TRUE),
              extensions = "Buttons",
              rownames = FALSE)
  })

  output$summary <- renderPrint({
    df <- sheet_data()
    num_cols <- sapply(df, is.numeric)
    summary(df[ , num_cols])
  })

  output$plot_ui <- renderUI({
    df <- sheet_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0)
      return(h4("No numeric columns."))
    tagList(
      selectInput("num_var", "Pick a numeric column", choices = num_cols),
      plotOutput("hist")
    )
  })

  output$hist <- renderPlot({
    req(input$num_var)
    x <- sheet_data()[[input$num_var]]

    if (inherits(x, "Date")) {
      hist(x, main = paste("Histogram of", input$num_var),
           xlab = input$num_var, freq = TRUE, breaks = "months")
    } else {
      hist(x, col = "#3E8ACC", border = "white",
           main = paste("Histogram of", input$num_var),
           xlab  = input$num_var)
    }
  })


  # ## 2. Summary statistics ----
  # output$sum_text <- renderPrint({
  #   req(input$var_sum)
  #   x <- tx_ki[[input$var_sum]]
  #   if (is.numeric(x)) summary(x) else table(x, useNA = "ifany")
  # })

  output$report_ui <- renderUI({
    req(input$rep_year)

    fname <- sprintf("tx_ki_summary_%s.html",
                     input$rep_year)

    tags$iframe(
      src   = fname,
      style = "width:100%; height:1100px; border:none;"
    )


  })

  ## 3. Var Definition
  table_var = data.frame("Variable Name" = c("KDPI","eGFR","Post-Transplant Survival",
                                             "Transplant Rate","Pre-transplant Mortality Rate",
                                             "Post-Transplant Mortality Rate"),
                         "Dataset Used" = c("Transplant File","Transplant File and Follow-up File",
                                            "Transplant File","Candidate File","Candidate File",
                                            "Transplant File"),
                         "Description" = c("Measures quality of organs","Measures kidney functionality",
                                           "Defines Post-Tx Death and Graft Failure",
                                           "Measures how fast patients move from waitlist to transplant",
                                           "Measures how quickly patients die on the waitlist",
                                           "Measures how quickly patients die after transplantation"))
  output$table_var = renderTable(table_var)
  output$code_var = renderText({
    req(input$var_name)
    if (input$var_name == "KDPI"){
      paste("#KDRI Calculate",
            "tx_kdpi <- tx_ki %>% mutate(KDRI_x = 0.0128*(DON_AGE-40)-0.0194*(DON_AGE-18)*(DON_AGE<18)+0.0107*(DON_AGE-50)*(DON_AGE>50)-0.0464*(DON_HGT_CM-170)/10-",
            "                                     0.0199*(DON_WGT_KG-80)/5+0.179*(DON_RACE==16)+0.126*(DON_HTN==1)+0.13*(DON_HIST_DIAB%in%c(2,3,4,5))+0.0881*(DON_CAD_DON_COD==2)+",
            "                                     0.22*(DON_CREAT-1)-0.209*(DON_CREAT-1.5)*(DON_HIGH_CREAT==1)+0.24*(DON_ANTI_HCV=='P')+0.133*(DON_NON_HR_BEAT=='Y'),",
            "                            KDRI_unsca = exp(KDRI_x),",
            "                            Year = as.numeric(substring(CAN_LISTING_DT,1,4)))",
            "#Scaling KDRI",
            "for (i in 1:nrow(tx_kdpi)){",
            "tx_kdpi[i,'KDRI'] = tx_kdpi[i,'KDRI_unsca']/KDPI_sca[which(KDPI_sca$Year==unlist(tx_kdpi[i,'Year'])),'Scale_Fac']",
            "tx_kdpi[i,'KDPI'] = KDPI_map[which((unlist(tx_kdpi[i,'KDRI'])<=KDPI_map$up)&(unlist(tx_kdpi[i,'KDRI'])>KDPI_map$low)&(KDPI_map$Year==unlist(tx_kdpi[i,'Year']))),'KDPI']",
            "}"
            ,sep="\n")
    }
    else if (input$var_name == "eGFR"){
      paste("#Note that eGFR can be calculated whenever a creatinine measurement is available.",
            "#The following code shows the eGFR calculation for eGFR immediately before transplant.",
            "tx_kdpi$REC_eGFR = ifelse(temp$CAN_GENDER == 'M',142*pmin(temp$REC_CREAT/0.9,1)^(-0.302)*pmax(temp$REC_CREAT/0.9,1)^(-1.2)*",
            "0.9938^(temp$REC_AGE_AT_TX),142*pmin(temp$REC_CREAT/0.7,1)^(-0.241)*pmax(temp$REC_CREAT/0.7,1)^(-1.2)*0.9938^(temp$REC_AGE_AT_TX)*1.012)"
            ,sep="\n")
    }
    else if (input$var_name == "Post-Transplant Survival"){
      paste("#compdth means all-cause graft survival: Both graft failure and patient death count as an event.",
            "#ptx_death means patient survival: Death is considered as an event, and graft failure is considered as censoring.",
            "#gft means death-censored graft survival: Only graft failure is considered an event and death is considered as censoring",
            "tx_kdpi = tx_kdpi %>% mutate(compdth = ifelse(is.na(TFL_DEATH_DT)==FALSE|is.na(TFL_GRAFT_DT)==FALSE|is.na(PERS_SSA_DEATH_DT)==F|is.na(PERS_OPTN_DEATH_DT)==F, 1, 0),
                                          ptx_death = ifelse(is.na(TFL_DEATH_DT)==FALSE|is.na(PERS_SSA_DEATH_DT)==F|is.na(PERS_OPTN_DEATH_DT)==F, 1, 0),
                                          gft = ifelse(is.na(TFL_GRAFT_DT)==FALSE, 1, 0)",sep="\n")
    }
  })
  ## 4. Explorer plot ----
  # output$dist_plot <- renderPlot({
  #   req(input$var_plot)
  #   var <- input$var_plot
  #   x   <- tx_ki[[var]]
  #
  #   if (!input$by_group) {
  #     if (is.numeric(x)) {
  #       hist(
  #         x, main = paste("Histogram of", var),
  #         xlab = var, col = "#3E8ACC", border = "white"
  #       )
  #     } else {
  #       barplot(
  #         table(x), main = paste("Bar plot of", var),
  #         col = "#2ECC71", las = 2
  #       )
  #     }
  #   } else {
  #     req(input$grp_var)
  #     g <- input$grp_var
  #     if (is.numeric(x)) {
  #       tx_ki %>%
  #         ggplot(aes(.data[[g]], .data[[var]])) +
  #         geom_boxplot(fill = "#3E8ACC") +
  #         labs(x = g, y = var)
  #     } else {
  #       tx_ki %>%
  #         ggplot(aes(.data[[g]], fill = .data[[var]])) +
  #         geom_bar(position = "dodge") +
  #         labs(x = g, y = "Count")
  #     }
  #   }
  # })

  output$map <- renderLeaflet({
    radius <- Site$TMR_CadTxR_c*100000
    paletteNum <- colorNumeric('YlOrRd', domain = opo_map$color_cd)
    pal = colorBin("Blues", Site$TMR_CadTxR_c, bins = quantile(Site$TMR_CadTxR_c))
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5) %>%
      addPolygons(data = opo_map,
                  color = 'black',
                  weight = 2,
                  smoothFactor = .3,
                  fillOpacity = .2,
                  fillColor = ~paletteNum(opo_map$color_cd),
                  popup = ~htmlEscape(opo_map$opo_ctr_cd)) %>%
      addCircleMarkers(data = Site, ~Longitude, ~Latitude,
                       stroke=FALSE, fillOpacity=0.8, color = ~pal(Site$TMR_CadTxR_c), popup = ~paste0("<i>",
                                                                                                       Site$ENTIRE_NAME,
                                                                                                       "</i>",
                                                                                                       "<br/>",
                                                                                                       "Deceased Donor Transplant Rate: ",
                                                                                                       round(Site$TMR_CadTxR_c,3))) %>%
      # addMarkers(data = Site, ~Longitude, ~Latitude)%>%
      addLegend(pal = pal,
                values = Site$TMR_CadTxR_c,
                position = "bottomright",
                title = "Deceased Donor Transplant Rate")
    #   addCircles(data=Site, ~Longitude, ~Latitude, radius=radius,
    #              color="#ffa500", stroke=FALSE, fillOpacity=0.7)  %>%
    # addLegend(pal = pal,
    #           values = Site$TMR_CadTxR_c,
    #           position = "bottomright",
    #           title = "Transplant Rate (%)")
  })

  # observe({
  #   #yearBy <- input$year
  #   radius <- 10000
  #
  #   leafletProxy("map", data = Site) %>%
  #     addCircleMarkers(~Longitude, ~Latitude,
  #                      stroke=FALSE, fillOpacity=0.8, color = ~pal(Site$TMR_CadTxR_c), popup = ~paste0("<i>",
  #                                                                                                      Site$ENTIRE_NAME,
  #                                                                                                      "</i>",
  #                                                                                                      "<br/>",
  #                                                                                                      "Deceased Donor Transplant Rate: ",
  #                                                                                                      round(Site$TMR_CadTxR_c,3))) %>%
  #     addLegend(pal = pal,
  #               values = Site$TMR_CadTxR_c,
  #               position = "bottomright",
  #               title = "Deceased Donor Transplant Rate")
  # })

  ## 5. Outcome plot ----
  # KP_2yr = readPNG("2yr Compare Version 2.png")
  # output$KP_2yr = renderPlot(KP_2yr)
  # RR_2yr = readPNG("RR 2yr Compare.png")
  # output$RR_2yr = renderPlot(RR_2yr)
  # ## 4. Raw data table ----
  # output$raw_tbl <- renderDT(
  #   tx_ki,
  #   options = list(scrollX = TRUE, pageLength = 15, dom = "Blfrtip"),
  #   filter = "top",
  #   extensions = "Buttons"
  # )
}

# -------------------- Run app ----------------------
shinyApp(ui, server)
