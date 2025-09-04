library(shiny)
library(DT)
library(tidyverse)
library(bslib)
library(png)
library(leaflet)
library(mapview)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(tigris)
library(readxl)
library(haven)
library(readr)
library(sf)
library(htmltools)
library(filelock)
library(uuid)
library(shinymanager)

# app security values ----------------------------------------------------------
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

# data.frame with credentials info
credentials <- data.frame(
  user = c("mkleinsa"),
  password = c("cure2!#www"),
  stringsAsFactors = FALSE
)

# webshot::install_phantomjs()

# ---- Screenshot backend bootstrap (runs once per R session) ----
# Prefer webshot2 (no PhantomJS needed). If webshot2 is missing, fall back to
# webshot + PhantomJS and install PhantomJS only when it's not present.
ensure_screenshot_backend <- function() {
  # 1) Prefer webshot2 (no system PhantomJS required)
  if (requireNamespace("webshot2", quietly = TRUE)) return("webshot2")

  # 2) Fallback to webshot + PhantomJS
  if (requireNamespace("webshot", quietly = TRUE)) {
    # Safely check whether PhantomJS is already installed
    ok <- tryCatch(webshot::is_phantomjs_installed(), error = function(e) FALSE)

    # If missing, try to install once; force=TRUE bypasses NA version checks
    if (!ok) {
      tryCatch({
        webshot::install_phantomjs(force = TRUE)  # install only when missing
      }, error = function(e) {
        message("PhantomJS install failed: ", conditionMessage(e))
      })
      ok <- tryCatch(webshot::is_phantomjs_installed(), error = function(e) FALSE)
    }

    # If PhantomJS is available, we can use webshot
    return(if (ok) "webshot" else "none")
  }

  # 3) Neither webshot2 nor webshot is available
  "none"
}


safe_includeHTML <- function(path) {
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  # strip any bootstrap 3 link
  html <- gsub('<link[^>]*bootstrap[^>]*>', "", html, perl = TRUE)
  HTML(html)
}

append_row <- function(df_row, file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)

  df_row[is.na(df_row)] <- ""

  lock <- filelock::lock(paste0(file, ".lock"))
  on.exit(filelock::unlock(lock), add = TRUE)

  if (!file.exists(file)) {
    write.table(df_row, file,
                sep = ",", row.names = FALSE, col.names = TRUE,
                quote = TRUE, na = "", qmethod = "double")
  } else {
    write.table(df_row, file,
                sep = ",", row.names = FALSE, col.names = FALSE,
                quote = TRUE, na = "", qmethod = "double", append = TRUE)
  }
}

# ------------- Feedback form URLs ------------------
form_url_full  <- "https://forms.gle/BQu99QmkQrZHLYiC8"
form_url_embed <- "https://docs.google.com/forms/d/e/1FAIpQLSfjD9MNAlupwUw8xYQ-emu1Qiv-IemUP_KU1DDJZAQiOARGdg/viewform?embedded=true"  #

# -------------------- Load data --------------------

center_data_1 = read_xls("data/csrs_final_tables_2505_KI.xls",sheet = 1)
center_data_2 = read_xls("data/csrs_final_tables_2505_KI.xls",sheet = 5)
center_data = data.frame(center_data_1,center_data_2)


excel_path <- "data/csrs_final_tables_2505_KI.xls"
sheet_list <- excel_sheets(excel_path)


time = c("January 2018", "July 2018", "January 2019", "July 2019", "January 2020", "July 2020",
         "January 2021", "July 2021", "January 2022", "July 2022", "January 2023", "July 2023",
         "January 2024", "July 2024", "January 2025", "July 2025")
vars <- c(
  "Deceased Donor Transplant Rate",
  "Transplant Rate",
  "Pre-transplant Mortality Rate"
)
# -------------------- Load data --------------------

excel_path <- "data/csrs_final_tables_2505_KI.xls"
sheet_list <- excel_sheets(excel_path)

# -------------------- UI ---------------------------
ui <- secure_app(navbarPage(
  tags$head(
    tags$style(
      HTML(".shiny-output-error-validation{
           color: red;}")
    )
  ),
  title = "Transplant Data Portal",
  theme = bs_theme(version = 5, bootswatch = "zephyr"),

  # bootswatch =
  # "cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal",
  # "litera", "lumen", "lux", "materia", "minty", "morph",
  # "pulse", "quartz", "sandstone", "simplex", "sketchy", "slate",
  # "solar", "spacelab", "superhero", "united", "vapor", "yeti", "zephyr"
  # --- Center Geographic Map ---

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

  # --- 2. Data Dictionary ---


  tabPanel(
    "Data Dictionary",
    safe_includeHTML("www/dataDictionary_utf8.html")
  ),

  # --- 3. Outcome Definition ---
  tabPanel(
    "Variable Definition",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("var_name", "Choose a variable", choices = c("KDPI","eGFR","Transplant Rate","Post-Transplant Survival",
                                                                "Pre-transplant Mortality Rate"))
      ),
      mainPanel(
        fluidRow(
          column(
            width = 12,
            h3("Variable Definition Table"),
            tableOutput("table_var"),
            uiOutput("text1")
          )
        ),
        verbatimTextOutput("code_var"),
        fluidRow(
          column(
            width = 12,
            h3("Reference"),
            uiOutput("text2")
          )
        )
      )
    )
  ),


  # --- 4. KDPI and EPTS -------------
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



  # --- 5. Center Map -------------


  tabPanel("Center Map",
           fluidRow(
             column(
               width = 12,
               h3("Center-level Information"),
               h5("Currently, 317 centers are included in the dataset. However, not all of them have available geographic information."),
               h5("In all, 232 centers have available geographic information and is displayed in the map below."),
               h5("Each circle represents a transplant center. You can click on the circle to see the name and the selected outcome."),
               h5("After choosing the zoom level and location, you can click the download button to download the map as a png file.")
             )
           ),
           div(class="outer",
               tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(document).on("shiny:visualchange", function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = document.getElementById("map").clientWidth;
                        dimension[1] = document.getElementById("map").clientHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
               leafletOutput("map", width = "70%", height = 600),
               downloadButton("dl","Download Map"),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                             width = 500, height = "auto",

                             h2("Transplant explorer"),

                             selectInput("outcome","Outcome", choices = vars,selected ="Deceased Donor Transplant Rate"),
                             selectInput("period","Period", choices = time,selected ="January 2025"))

               #plotOutput("lineplot", height = 300),
               #plotOutput("histplot", height = 300))
           ),
           fluidRow(
             column(
               width = 12,
               h3("Reference"),
               uiOutput("text3")
             )
           )
  ),

  # --- 6. Center Data -------------

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


  # --- 7. Data Use Agreement ---
  tabPanel(
    "Data Use Agreement",
    fluidRow(
      column(
        width = 10, offset = 1,
        h3("Data Use Agreement (Selected Clauses)"),
        tags$p("The following clauses are reproduced verbatim:"),
        tags$ol(
          tags$li(HTML('<strong>#9.</strong> Before submitting an abstract, manuscript, or other aggregation data to another party for presentation or publication, the Recipient must submit it to the SRTR and COR for review to ensure compliance with the terms of this agreement regarding confidentiality. The COR shall respond within 30 days. If the abstract, manuscript, or data aggregation does not reflect compliance with the terms of this agreement, the Recipient will revise and resubmit to the SRTR and COR. Upon publication, the Recipient shall provide a copy of the final work and a complete citation to the SRTR and COR.')),
          tags$li(HTML('<strong>#12.</strong> All publications using the released Data must contain the standard disclaimer, ``The data reported here have been supplied by the Hennepin Healthcare Research Institute (HHRI) as the contractor for the Scientific Registry of Transplant Recipients (SRTR). The interpretation and reporting of these data are the responsibility of the author(s) and in no way should be seen as an official policy of or interpretation by the SRTR or the U.S. Government.\'\'')),
          tags$li(HTML('<strong>#13.</strong>  All publications using the released Data must contain a statement confirming that the study was submitted to a functioning IRB for review and approval. The IRB determination status must be indicated in the text of any manuscript using the released Data.')),
          tags$li(HTML('<strong>#14.</strong> All publications using the released Data must contain this standard statement within the methods section of the publication, ``This study used data from the Scientific Registry of Transplant Recipients (SRTR). The SRTR data system includes data on all donor, wait-listed candidates, and transplant recipients in the US, submitted by the members of the Organ Procurement and Transplantation Network (OPTN). The Health Resources and Services Administration (HRSA), U.S. Department of Health and Human Services provides oversight to the activities of the OPTN and SRTR contractors.\'\'')))
      )
    )
  ),

  # --- 8. Feedback (embed + local header) ---
  tabPanel(
    "Feedback",
    fluidRow(
      column(
        width = 10, offset = 1,
        tags$div(class = "card shadow-sm p-3",
                 # tags$img(src = "um_bio_header.png",
                 #          style = "max-width:50%; height:auto; border-radius:8px; margin-bottom:12px;"),
                 h3("We value your feedback"),
                 tags$p("Please use the Google Form below to share your suggestions or questions. Thank you!"),
                 tags$a(href = form_url_full, target = "_blank",
                        class = "btn btn-primary btn-lg mb-3", "Open the feedback form"),
                 tags$div(
                   style = "background:#FFF6D6; border:1px solid #F2D57E; border-radius:12px; padding:8px;",
                   tags$iframe(
                     src   = form_url_embed,
                     style = "width:100%; height:900px; border:none; background:transparent;"
                   )
                 ),
                 tags$p(class = "mt-3 text-muted",
                        "We value your feedback. Please use the Google Form below to share your suggestions or questions. Thank you!")
        )
      )
    )
  ),


  # --- 9. About ---
  tabPanel("About",
           HTML("
      <p>This portal is maintained by XXX.</p>
      <p>Contact: <a href='mailto:XXX@email'>XXX@email</a></p>
    ")
  )
))

# -------------------- Server -----------------------
server <- function(input, output, session) {

  # app security server --------------------------------------------------------
  result_auth <- secure_server(check_credentials = check_credentials(credentials))

  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  # app security server --------------------------------------------------------


  # Detect/prepare the screenshot backend once per session
  backend <- ensure_screenshot_backend()

  all_data = readRDS("data/center_data.rds")

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


  output$text1 = renderUI(HTML(paste("The following section shows example code on how to build these variables.<br>",
                                       "You can choose the variable of interest from the left panel.<br>",
                                       sep = "")))
  output$text2 = renderUI(HTML(paste("1. KDPI is used to quantify the quality of deceased donor kidneys relative to other recovered kidneys. SRTR has a report detailing its usage.<br>",
                                     "<p style='text-indent: 15px;'>KDPI Reference: https://optn.transplant.hrsa.gov/media/j34dm4mv/kdpi_guide.pdf<br></p>",
                                     "2. eGFR is commonly used for renal functions. Currently, the 2021 CKD-EPI equation is used. The equation is derived from the following paper.<br>",
                                     "<p style='text-indent: 15px;'>eGFR Reference: https://www.nejm.org/doi/full/10.1056/NEJMoa2102953<br></p>",
                                     sep = "")))

  output$text3 = renderUI(HTML(paste("1. All center-level information are available on the SRTR website listed below.<br>",
                                     "<p style='text-indent: 15px;'>PSR Reference: https://www.srtr.org/reports/program-specific-reports/<br></p>",
                                     "2. The OPO county assignment can be found on the SRTR website listed below.<br>",
                                     "<p style='text-indent: 15px;'>eGFR Reference: https://www.srtr.org/reports/opo-specific-reports/interactive-report<br></p>",
                                     sep = "")))
  ## 1. Data dictionary ----
  # dict <- tibble(
  #   Variable = names(tx_ki),
  #   Class    = sapply(tx_ki, class),
  #   Missing  = colSums(is.na(tx_ki)),
  #   Examples = sapply(tx_ki, \(x) paste0(head(unique(x), 3), collapse = ", "))
  # )

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

    tagList(

      tags$style(HTML("

      iframe.reportFrame ~ * {}
    ")),
      tags$iframe(
        class = "reportFrame",
        src   = fname,
        style = "width:120%; height:1100px; border:none;"
      )
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
            "#Require External Mapping File",
            "KDPI_sca = read.csv('data/KDRI_Scale_Fac.csv')",
            "KDPI_map = read.csv('data/KDPI_Mapping_090424_updateto2023.csv')",
            "tx_kdpi <- tx_ki %>% mutate(KDRI_x = 0.0128*(DON_AGE-40)-0.0194*(DON_AGE-18)*(DON_AGE<18)+0.0107*(DON_AGE-50)*(DON_AGE>50)-0.0464*(DON_HGT_CM-170)/10-",
            "                                     0.0199*(DON_WGT_KG-80)/5+0.179*(DON_RACE==16)+0.126*(DON_HTN==1)+0.13*(DON_HIST_DIAB%in%c(2,3,4,5))+0.0881*(DON_CAD_DON_COD==2)+",
            "                                     0.22*(DON_CREAT-1)-0.209*(DON_CREAT-1.5)*(DON_HIGH_CREAT==1)+0.24*(DON_ANTI_HCV=='P')+0.133*(DON_NON_HR_BEAT=='Y'),",
            "                            KDRI_unsca = exp(KDRI_x),",
            "                            Year = as.numeric(substring(CAN_LISTING_DT,1,4)))",
            "#Scaling KDRI",
            "for (i in 1:nrow(tx_kdpi)){",
            "   tx_kdpi[i,'KDRI'] = tx_kdpi[i,'KDRI_unsca']/KDPI_sca[which(KDPI_sca$Year==unlist(tx_kdpi[i,'Year'])),'Scale_Fac']",
            "   tx_kdpi[i,'KDPI'] = KDPI_map[which((unlist(tx_kdpi[i,'KDRI'])<=KDPI_map$up)&(unlist(tx_kdpi[i,'KDRI'])>KDPI_map$low)&(KDPI_map$Year==unlist(tx_kdpi[i,'Year']))),'KDPI']",
            "}"
            ,sep="\n")
    }
    else if (input$var_name == "eGFR"){
      paste("#Note that eGFR can be calculated whenever a creatinine measurement is available.",
            "#The following code shows the eGFR calculation for eGFR immediately before transplant.",
            "tx_kdpi$REC_eGFR = ifelse(tx_kdpi$CAN_GENDER == 'M',142*pmin(tx_kdpi$REC_CREAT/0.9,1)^(-0.302)*pmax(tx_kdpi$REC_CREAT/0.9,1)^(-1.2)*",
            "0.9938^(tx_kdpi$REC_AGE_AT_TX),142*pmin(tx_kdpi$REC_CREAT/0.7,1)^(-0.241)*pmax(tx_kdpi$REC_CREAT/0.7,1)^(-1.2)*0.9938^(tx_kdpi$REC_AGE_AT_TX)*1.012)"
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
    else if(input$var_name == "Transplant Rate" | input$var_name == "Pre-transplant Mortality Rate"){
      paste("#Here we calculate the rate for each calendar month. We use the year 2010 as the starting point for illustration. The unit is per person-month.",
            "dt <- cand_kipa %>%",
            "      filter(WL_ORG=='KI',as.numeric(substr(CAN_REM_DT,1,4))>=2010 | is.na(CAN_REM_DT), !is.na(CAN_LISTING_DT)) %>%",
            "      mutate(start_month = pmax(as.numeric(substring(CAN_LISTING_DT,6,7))+12*(as.numeric(substring(CAN_LISTING_DT,1,4))-2010),0),",
            "             end_month = as.numeric(substring(CAN_REM_DT,6,7))+12*(as.numeric(substr(CAN_REM_DT,1,4))-2010),",
            "             start_week = pmax(as.numeric(difftime(CAN_LISTING_DT,as.Date('2010-01-01'),units = 'week')),0),",
            "             end_week = as.numeric(difftime(CAN_REM_DT,as.Date('2010-01-01'),units = 'week')),",
            "             day_start_month = as.numeric(substring(CAN_LISTING_DT,9,10)),",
            "             day_end_month = as.numeric(substring(CAN_REM_DT,9,10)))",
            "Txp_rate_plot_dt <- data.frame(month = c(1:max(dt$end_month,na.rm = T)),Txp_rate = c(NA))",
            "for (i in 1:(nrow(Txp_rate_plot_dt)-1)) {",
            "    days_at_month <- ifelse(i%%12 %in% c(1,3,5,7,8,10,0), 31,",
            "    ifelse(i%%12 == 2 & (i%/%12)%%4==0, 29,",
            "    ifelse(i%%12 == 2, 28, 30)))",
            "    dt$age_cur = dt$CAN_AGE_AT_LISTING + floor(i/12) + 2010 - as.numeric(substr(dt$CAN_LISTING_DT,1,4))",
            "    temp <- dt %>%",
            "            filter(start_month<=i,end_month>=i|is.na(end_month)) %>%",
            "            mutate(person_month = ifelse(is.na(end_month),1,",
            "                                         ifelse(start_month==end_month,(day_end_month-day_start_month)/days_at_month,",
            "                                                ifelse(start_month==i,(days_at_month+1-day_start_month)/days_at_month,",
            "                                                       ifelse(end_month==i,day_end_month/days_at_month,1)))))",
            "                                                                                                                      ",
            "    temp = temp %>% filter(age_cur >= 70)",
            "                                                                                                                      ",
            "    Txp_rate_plot_dt[i,'Txp_count'] <- nrow(dt %>% filter(CAN_REM_CD %in% c(4,15,18,19), end_month == i))",
            "    Txp_rate_plot_dt[i,'Deceased_Txp_count'] <- nrow(dt %>% filter(CAN_REM_CD %in% c(4), end_month == i))",
            "    Txp_rate_plot_dt[i,'Txp_free_death_count'] <- nrow(dt %>% filter(CAN_REM_CD==8, end_month == i))",
            "    Txp_rate_plot_dt[i,'New_waitlisted_count'] <- nrow(dt %>% filter(start_month == i))",
            "    Txp_rate_plot_dt[i,'Living_Txp_count'] <- nrow(dt %>% filter(CAN_REM_CD %in% c(15), end_month == i))",
            "    Txp_rate_plot_dt[i,'Person_month'] <- sum(temp$'person_month')",
            "                                                                                                         ",
            "    Txp_rate_plot_dt[i,'Txp_rate'] <- Txp_rate_plot_dt[i,'Txp_count']/Txp_rate_plot_dt[i,'Person_month']",
            "    Txp_rate_plot_dt[i,'Deceased_Txp_rate'] <- Txp_rate_plot_dt[i,'Deceased_Txp_count']/Txp_rate_plot_dt[i,'Person_month']",
            "    Txp_rate_plot_dt[i,'Txp_free_death_rate'] <- Txp_rate_plot_dt[i,'Txp_free_death_count']/Txp_rate_plot_dt[i,'Person_month']",
            "    Txp_rate_plot_dt[i,'Living_Txp_rate'] <- Txp_rate_plot_dt[i,'Living_Txp_count']/Txp_rate_plot_dt[i,'Person_month']",
            "}",sep = "\n")
    }
  })
  ## 4. Explorer plot ----
  output$dist_plot <- renderPlot({
    req(input$var_plot)
    var <- input$var_plot
    x   <- tx_ki[[var]]

    if (!input$by_group) {
      if (is.numeric(x)) {
        hist(
          x, main = paste("Histogram of", var),
          xlab = var, col = "#3E8ACC", border = "white"
        )
      } else {
        barplot(
          table(x), main = paste("Bar plot of", var),
          col = "#2ECC71", las = 2
        )
      }
    } else {
      req(input$grp_var)
      g <- input$grp_var
      if (is.numeric(x)) {
        tx_ki %>%
          ggplot(aes(.data[[g]], .data[[var]])) +
          geom_boxplot(fill = "#3E8ACC") +
          labs(x = g, y = var)
      } else {
        tx_ki %>%
          ggplot(aes(.data[[g]], fill = .data[[var]])) +
          geom_bar(position = "dodge") +
          labs(x = g, y = "Count")
      }
    }
  })

  observe({
    center_data = all_data %>% filter(time_period == input$period)
    hist_opo_txc = read.csv("data/Center_opo.csv")
    hist_opo_txc = hist_opo_txc %>% filter(Center.Code %in% center_data$CTR_CD)
    hist_opo_txc = hist_opo_txc[,-1]
    if (input$outcome == "Deceased Donor Transplant Rate"){
      hist_opo_txc$Rate = center_data[match(hist_opo_txc$Center.Code,center_data$CTR_CD),]$TMR_CadTxR_o
      center_data$Rate = as.numeric(center_data$TMR_CadTxR_c)
      legend_text = c("DDKT Rate")
    }
    else if (input$outcome == "Transplant Rate"){
      hist_opo_txc$Rate = center_data[match(hist_opo_txc$Center.Code,center_data$CTR_CD),]$TMR_TxR_o
      center_data$Rate = as.numeric(center_data$TMR_TxR_c)
      legend_text = c("Tx Rate")
    }
    else if (input$outcome == "Pre-transplant Mortality Rate"){
      hist_opo_txc$Rate = center_data[match(hist_opo_txc$Center.Code,center_data$CTR_CD),]$TMR_DthR_o
      center_data$Rate = as.numeric(center_data$TMR_DthR_c)
      legend_text = c("PreTx Dth Rate")
    }

    opo_map = readRDS("data/opo_map.rds")
    opo_map$Rate = hist_opo_txc[match(opo_map$opo_ctr_cd,hist_opo_txc$Served.OPO.Code),]$Rate
    opo_map$Rate = as.numeric(opo_map$Rate)
    geo_info = read_xls("data/csrs_final_tables_22_11_KI.xls")
    geo_info = geo_info[-1,] %>% select(CTR_CD,Latitude,Longitude)
    center_data = merge(center_data,geo_info,by = "CTR_CD")

    Site <- center_data
    #Site <- merge(Site,center_data[,c("CTR_CD","TMR_CadTxR_c")],all.x = TRUE,by="CTR_CD")
    Site$Latitude <- as.numeric(Site$Latitude)
    Site$Longitude <- as.numeric(Site$Longitude)
    # Site$TMR_CadTxR_c <- as.numeric(Site$TMR_CadTxR_c)
    # Site$TMR_DthR_c <- as.numeric(Site$TMR_DthR_c)

    output$map <- renderLeaflet({
      pal = colorBin("YlOrRd", Site$Rate, bins = quantile(Site$Rate))
      pal2 = colorBin("Greens", opo_map$Rate, bins = quantile(opo_map$Rate))
      vals$base <- leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addTiles() %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4.5) %>%
        addPolygons(data = opo_map,
                    color = 'black',
                    weight = 2,
                    smoothFactor = .3,
                    fillOpacity = .5,
                    fillColor = ~pal2(opo_map$Rate),
                    popup = ~paste0("<i>",
                                    opo_map$opo_ctr_cd,
                                    "</i>",
                                    "<br/>",
                                    paste0(legend_text," (OPO) (per person-year): "),
                                    round(opo_map$Rate,3))) %>%
        addCircleMarkers(data = Site, ~Longitude, ~Latitude,
                         stroke=FALSE, fillOpacity=0.8, color = ~pal(Site$Rate), popup = ~paste0("<i>",
                                                                                                         Site$ENTIRE_NAME,
                                                                                                         "</i>",
                                                                                                         "<br/>",
                                                                                                         paste0(legend_text," (Center) (per person-year): "),
                                                                                                         round(Site$Rate,3))) %>%
        # addMarkers(data = Site, ~Longitude, ~Latitude)%>%
        addLegend(pal = pal,
                  values = Site$Rate,
                  position = "bottomright",
                  title = paste0(legend_text," (OPO) (per person-year): ")) %>%
        addLegend(pal = pal2,
                  values = opo_map$Rate,
                  position = "bottomleft",
                  title = paste0(legend_text," (OPO) (per person-year): "))
      #   addCircles(data=Site, ~Longitude, ~Latitude, radius=radius,
      #              color="#ffa500", stroke=FALSE, fillOpacity=0.7)  %>%
      # addLegend(pal = pal,
      #           values = Site$TMR_CadTxR_c,
      #           position = "bottomright",
      #           title = "Transplant Rate (%)")
    })

    # reactive values to store map
    vals <- reactiveValues()

    observeEvent({
      input$map_zoom
      input$map_center
    }, {
      vals$current <- vals$base %>%
        setView(lng = input$map_center$lng,
                lat = input$map_center$lat,
                zoom = input$map_zoom)
    }
    )

    output$dl <- downloadHandler(
      filename = "map.png",
      content = function(file) {
        if (backend %in% c("webshot2", "webshot")) {
          # mapview::mapshot() will pick the available backend
          mapview::mapshot(
            vals$current, file = file,
            vwidth  = input$dimension[1] + 500,
            vheight = input$dimension[2] + 250,
            selfcontained = FALSE
          )
        } else {
          showNotification(
            "Screenshot backend unavailable. Please install {webshot2} (recommended) or {webshot}+PhantomJS.",
            type = "error"
          )
        }
      }
    )

  })



}

# -------------------- Run app ----------------------
shinyApp(ui, server)
