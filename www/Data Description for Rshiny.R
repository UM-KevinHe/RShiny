
library(summarytools)
library(htmltools)
library(dplyr)
library(stringr)

# tx_ki <- read.csv("data/tx_ki.csv")
#
# tx_ki <- read_sas("C:\\Custom Files\\UMich\\Course\\Kevin\\Data\\tx_ki.sas7bdat")
######### select vars for tx_ki

# selected_vars <- c(
#   # Donor Info
#   "DON_AGE", "DON_AGE_IN_MONTHS", "DON_HGT_CM", "DON_WGT_KG",
#   "DON_RACE", "DON_RACE_SRTR", "DON_ETHNICITY_SRTR",
#   "DON_HTN", "DON_HIST_HYPERTEN", "DON_HIST_DIAB", "DON_CREAT",
#   "DON_ANTI_HCV", "DON_CAD_DON_COD", "DON_DEATH_MECH", "DON_DEATH_CIRCUM",
#   "DON_CARDIAC_ARREST_AFTER_DEATH", "DON_NON_HR_BEAT", "DON_TY","DON_B1","DON_DR1",
#
#   # Patient Info
#   "CAN_AGE_AT_LISTING", "CAN_AGE_IN_MONTHS_AT_LISTING",
#   "CAN_GENDER", "CAN_RACE", "CAN_RACE_SRTR", "CAN_ETHNICITY_SRTR",
#   "CAN_ABO", "CAN_DIAB", "CAN_DIAB_TY", "CAN_LISTING_DT", "CAN_PREV_TX", "CAN_PREV_KI",
#
#   # Transplant Record
#   "REC_AGE_AT_TX", "REC_AGE_IN_MONTHS_AT_TX", "REC_TX_DT",
#   "REC_BMI", "REC_CREAT", "REC_DISCHRG_CREAT", "REC_POSTX_LOS", "REC_DISCHRG_DT",
#
#   # Outcome
#   "TFL_DEATH_DT", "PERS_SSA_DEATH_DT", "PERS_OPTN_DEATH_DT",
#   "REC_FAIL_DT", "TFL_LAFUDATE", "TFL_LASTATUS", "TFL_ENDTXFU", "DON_RECOV_DT",
#
#   "TFL_GRAFT_DT", "PERS_RETX"  # , "COMPOSITE_DEATH_DATE"
# )

tx_ki_selected <- tx_ki %>% select(all_of(selected_vars))

tx_ki_selected <- tx_ki_selected %>%
  filter(
  REC_TX_DT >= as.Date("2025-01-01"),
  REC_TX_DT < as.Date("2025-12-31")
)



# ==============================================================
# Custom summary report for a data.frame (no summarytools needed)
# ==============================================================
library(dplyr)
library(tidyr)
library(purrr)
library(htmltools)
library(kableExtra)
library(scales)


# -------- 0. vector-safe percent formatter --------------------------------
fmt_pct <- function(p) {
  sapply(p, function(x) {
    if (is.na(x))               return("-")
    if (x > 0     && x < 0.0001) return("<0.01%")
    if (x >= 0.9999 && x < 1)    return(">99.99%")
    percent(x, accuracy = 0.01)              # <- two-decimal percentages
  }, USE.NAMES = FALSE)
}


# -------- 1. numeric summary helper --------------------------------------
num_summary <- function(x) {
  tibble(
    mean = mean(x, na.rm = TRUE),
    sd   = sd(x,   na.rm = TRUE),
    min  = min(x,  na.rm = TRUE),
    med  = quantile(x, 0.5, na.rm = TRUE),
    max  = max(x,  na.rm = TRUE)
  ) |>
    mutate(across(everything(), \(v) round(v, 2)))
}

# -------- 2. categorical summary helper ----------------------------------
cat_summary <- function(x, top_n = 6) {
  tb <- sort(table(x), decreasing = TRUE)
  lv <- head(tb, top_n)
  paste0(
    names(lv), ": ", lv, " (",
    fmt_pct(lv / sum(tb)), ")",
    collapse = "<br>"
  )
}

# -------- 3. main summary builder ----------------------------------------
build_summary <- function(df, top_n_levels = 6) {
  tot <- nrow(df)

  map_dfr(names(df), function(v) {
    vec <- df[[v]]
    lbl  <- attr(vec, "label", exact = TRUE)
    lbl  <- if (is.null(lbl)) "" else lbl
    cls  <- class(vec)[1]

    miss_n <- sum(is.na(vec))
    miss_pct  <- miss_n  / tot

    var_disp <- sprintf(
      "%s<br><span style='color:gray;'>[%s]</span>",
      v, cls
    )

    if (is.numeric(vec)) {
      stats <- num_summary(vec)
      tibble(
        variable = var_disp,
        label    = lbl,
        missing  = sprintf("%d (%s)", miss_n, fmt_pct(miss_pct)),
        stats    = paste0(
          "Mean=", stats$mean, "; SD=", stats$sd,
          "<br>Min=", stats$min, "; Med=", stats$med,
          "; Max=", stats$max
        )
      )
    } else {
      lv_txt <- cat_summary(vec, top_n = top_n_levels)
      tibble(
        variable = var_disp,
        label    = lbl,
        missing  = sprintf("%d (%s)", miss_n, fmt_pct(miss_pct)),
        stats    = lv_txt
      )
    }
  })
}

# -------- 4. run and export ----------------------------------------------
summary_tbl <- build_summary(tx_ki_selected, top_n_levels = 6)

# ---- build the kable HTML string ----------------------------------------
html_table <- summary_tbl |>
  kable("html", escape = FALSE, align = c("l", "l", "c", "l"),
        col.names = c("Variable", "Label" , "Missing", "Stats / Top Levels")) |>
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover"))

# ========== ADD HERE : header block  =====================================
ds_name   <- "tx_ki selected variables (2025)"                     # dataset name
dim_line  <- sprintf("%d x %d",
                     nrow(tx_ki_selected),
                     ncol(tx_ki_selected))

header_block <- htmltools::tagList(
  htmltools::tags$h2("Dataset Summary"),
  htmltools::tags$h3(ds_name),
  htmltools::tags$p(
    htmltools::tags$strong("Dimensions: "),
    dim_line
  ),
  htmltools::tags$hr()
)
# ========================================================================

# # ---- read & filter summarytools CSS (add this block) --------------------
# st_css_file <- system.file("css", "summarytools.css", package = "summarytools")
#
# st_css <- readLines(st_css_file, warn = FALSE, encoding = "utf-8")
# st_css <- st_css[!grepl("\\.navbar", st_css)]
# st_css <- st_css[!grepl("\\.navbar", st_css, fixed = FALSE)]
# st_css <- st_css[!grepl("navbar", st_css)]
# st_css <- paste(st_css, collapse = "\n")
# # -------------------------------------------------------------------------

# ---- wrap into full HTML document ---------------------------------------
page <- htmltools::tagList(
  htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$title("tx_ki summary"),
      htmltools::tags$link(
        rel = "stylesheet",
        href = "https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css"
      )
      # ,
      # htmltools::tags$style(htmltools::HTML(st_css))

    ),
    htmltools::tags$body(
      style = "max-width: 1200px;
               margin: 0 auto;
               padding: 0 20px;",
      header_block,               # <—  place header before the table
      htmltools::HTML(html_table)
    )
  )
)


htmltools::save_html(page, file = "tx_ki_summary_custom.html")

#########################################################################
#########################################################################


# # ---------- 0. global options ----------
# st_options(
#   plain.ascii = FALSE,   # produce HTML
#   style       = "grid"
# )
#
# # ---------- 1. dfSummary ----------
# df <- dfSummary(tx_ki_selected)
#
# # ---------- 2. full HTML via view() ----------
# tmp_html <- tempfile(fileext = ".html")
# view(df, file = tmp_html, lang = "en", silent = TRUE)
#
# html_vec <- readLines(tmp_html, warn = FALSE, encoding = "UTF-8")
# html_txt <- paste(html_vec, collapse = "\n")                 # single string
#
# # ---- 3A. Non-zero 0.0 %  -->  <0.1 %  -----------------------------------
# #  pattern :  '>  count (0.0%)'  OR  '(0.0%)' in Missing column
# html_txt <- gsub(
#   "(>\\s*[1-9][0-9]*\\s*\\()0\\.0%",      # capture '(' as part of group-1
#   "\\1&lt;0.1%",
#   html_txt,
#   perl = TRUE
# )
#
# html <- gsub(
#   "(>\\s*[1-9][0-9]*\\s*\\()\\s*0\\.0%",   # <- added \\s* before 0.0%
#   "\\1&lt;0.1%",
#   html,
#   perl = TRUE
# )
#
# html <- gsub(
#   "(>\\s*[1-9][0-9]*\\s*<br>\\s*\\()\\s*0\\.0%",  # <- added \\s*
#   "\\1&lt;0.1%",
#   html,
#   perl = TRUE
# )
#
# # ---- 3B. 99.9 %  -->  >99.9 %   (*only* when count < n_total) ----------
# n_total <- nrow(tx_ki_selected)
#
#
# html_txt <- str_replace_all(
#   html_txt,
#
#   # pattern: ">" ... digits ... "(" ... optional <br> / spaces ... 100.0%
#   "(>\\s*)([0-9]+)([^0-9%]*?)\\s*100\\.0%",
#
#   function(m) {
#     # extract count inside the matched string
#     cnt <- as.numeric(str_extract(m, "[0-9]+"))
#
#     # only change if count < n_total
#     if (!is.na(cnt) && cnt < n_total) {
#       sub("100\\.0%", "&gt;99.9%", m, fixed = TRUE)
#     } else {
#       m   # leave genuine 100 % untouched
#     }
#   }
# )
#
# # ---- 4. Save final report -----------------------------------------------
# out_file <- "tx_ki_selected_summary1.html"
# writeLines(html_txt, out_file, useBytes = TRUE)
#
# message("HTML report written to: ", out_file)
#
#




