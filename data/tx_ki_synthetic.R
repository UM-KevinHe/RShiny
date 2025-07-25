set.seed(42)  # Set seed for reproducibility

# Get the column names from your original dataset
col_names <- colnames(tx_ki)
n <- nrow(tx_ki)

# Initialize an empty data frame with the same column names
synthetic_tx_ki <- data.frame(matrix(nrow = n, ncol = length(col_names)))
colnames(synthetic_tx_ki) <- col_names
# Generate synthetic values for each column based on pattern rules
for (col in col_names) {
  
  if (grepl("DT$|_DT$|_DATE$|_FUDATE$|_DIAL_DT$|_TXFER_DT$|_ADMISSION_DT$|_DISCHRG_DT$", col)) {
    # For date variables, generate random dates within a realistic range
    synthetic_tx_ki[[col]] <- sample(seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day"), n, replace = TRUE)
    
  } else if (grepl("ID$|_ID$|_ID_", col)) {
    # For ID fields, generate large random integers
    synthetic_tx_ki[[col]] <- sample(1e6:9e6, n, replace = TRUE)
    
  } else if (grepl("_CD$|_TY$|_STAT$|_CATEGORY$|_COD$|_SRC$|_CAUSE$", col)) {
    # For code or status fields, assign random capital letters
    synthetic_tx_ki[[col]] <- sample(LETTERS, n, replace = TRUE)
    
  } else if (grepl("AGE|YEARS|MONTHS|WGT|HGT|BMI|TIME|SCORE|KDPI|KDRI|PRA", col)) {
    # For continuous numeric variables like age, weight, scores
    synthetic_tx_ki[[col]] <- round(runif(n, 0, 100), 1)
    
  } else if (grepl("^ORG_TY$|_GENDER$|_RACE$|_ABO$|_ETHNICITY", col)) {
    # For categorical string fields like gender, race, blood type
    synthetic_tx_ki[[col]] <- sample(c("M", "F", "White", "Black", "Asian", "Other", "A", "B", "AB", "O", "Hispanic", "Non-Hispanic"), n, replace = TRUE)
    
  } else if (grepl("event|bin|_FAIL_|_REJ_|_CENSORED_|_IND_|_FLAG", col)) {
    # For binary indicator variables (0 or 1)
    synthetic_tx_ki[[col]] <- sample(0:1, n, replace = TRUE)
    
  } else {
    # Default: generate small integers (e.g., counts or ordinal categories)
    synthetic_tx_ki[[col]] <- sample(0:5, n, replace = TRUE)
  }
}

write.csv(synthetic_tx_ki, "data/tx_ki_synthetic.csv", row.names = FALSE)
