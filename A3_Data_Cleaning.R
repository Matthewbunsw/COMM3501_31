library(tidyverse)
library(summarytools)

# 1 Load Data
df <- read_csv("A3.csv")

# 2. Format Date Column
df$Date <- dmy(df$Date)

# 3. Data Quality Checks
#print(dfSummary(df))     # Summary report

# Drop 'ExternalRef' field as all values are null
df <- df %>%
  select(-ExternalRef)

# many key fields are missing exactly 1775 values
# Identify rows with missing in any key column (e.g., RecommendationId, RequestId, Date, etc.)
incomplete_rows <- df %>%
  filter(is.na(RecommendationId) | is.na(RequestId) | is.na(Date) | is.na(Time))

#nrow(incomplete_rows)
# returns 1775 incomplete missing, confirms data missing across columns 

# Drop rows where any of the key fields are missing
df <- df %>%
  filter(!is.na(RecommendationId) & !is.na(RequestId) & !is.na(Date) & !is.na(Time))


# 4.Identify categorical columns to convert to factors
categorical_vars <- c(
  "Alternative", "Underwriter", "Package", "Super", "RolloverTaxRebate",
  "CommissionStructure", "Gender", "SmokerStatus", "HomeState", "Occupation",
  "SelfEmployed", "Life", "TPD", "Trauma", "IP", "BE", "Severity",
  "PremiumFrequency"
)

# Convert to factors
df <- df %>%
  mutate(across(all_of(categorical_vars), as.factor))


# 5. Feature Engineering - Extracting data from 'CommissionStructure'
# drop records with missing commission structure (123 record, safe to drop as small proportion of dataset)
df <- df %>% filter(!is.na(CommissionStructure))
df <- df %>%
  mutate(
    # Normalize input to lowercase for consistent matching.
    CommissionStructure_clean = tolower(as.character(CommissionStructure)),
    
    # Fix common typo: replace patterns like "L2evel" with "level".
    CommissionStructure_clean = str_replace_all(CommissionStructure_clean, "l\\d*evel", "level"),
    
    # Determine commission type with additional cases.
    # Notice that the condition for entries starting with "initial (" is now re‑labeled as "Higher Initial".
    CommissionType = case_when(
      str_detect(CommissionStructure_clean, "no commission") ~ "Nil",
      str_detect(CommissionStructure_clean, "nil") ~ "Nil",
      str_detect(CommissionStructure_clean, "^\\d+\\.?\\d*%$") ~ "Upfront",
      str_detect(CommissionStructure_clean, "^initial\\s*\\(") ~ "Higher Initial",  # Updated condition for initial
      str_detect(CommissionStructure_clean, "year\\s?1 only") ~ "Year 1 Only",
      str_detect(CommissionStructure_clean, "level") ~ "Level",
      str_detect(CommissionStructure_clean, "up ?front") ~ "Upfront",
      str_detect(CommissionStructure_clean, "hybrid|h\\d+brid|h\\W+brid|h.*brid") ~ "Hybrid",
      str_detect(CommissionStructure_clean, "higher initial") ~ "Higher Initial",
      TRUE ~ "Other"
    ),
    
    # Extract upfront commission percentage.
    Commission_Upfront = str_extract(
      CommissionStructure_clean,
      paste0(
        "(?<=\\()\\d+\\.?\\d*",                # e.g., extracts '115' from "(115%/10%)"
        "|(?<=initial )\\d+\\.?\\d*",           # alternative pattern following 'initial'
        "|(?<=yr1 )\\d+\\.?\\d*",               
        "|(?<=year1 )\\d+\\.?\\d*",             
        "|(?<=only\\s?-\\s?c)\\d+\\.?\\d*",      
        "|(?<=^|\\s)(\\d+\\.?\\d*)(?=%)"        
      )
    ),
    
    # Extract renewal commission percentage.
    Commission_Renewal = str_extract(
      CommissionStructure_clean,
      "(?<=/\\s?)\\d+\\.?\\d*(?=%|\\))|(?<=renewal )\\d+\\.?\\d*|(?<=yr2 )\\d+\\.?\\d*|(?<=year2 )\\d+\\.?\\d*"
    ),
    
    # Convert the extracted strings to numeric values.
    Commission_Upfront = as.numeric(Commission_Upfront),
    Commission_Renewal = as.numeric(Commission_Renewal)
  ) %>%
  # Optionally remove the intermediate clean column.
  select(-CommissionStructure_clean)

# convert new Commission Type Column to a factor
df <- df %>%
  mutate(
    CommissionType = factor(
      CommissionType,
      levels = c("Nil", "Upfront", "Level", "Hybrid", "Higher Initial", "Year 1 Only")
    )
  )

# 6. Handling fields with few missing values
# summary of missing values
df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "MissingCount") %>%
  arrange(desc(MissingCount)) %>%
  print(n = Inf)

# numeric values use median imputation
# missing categorical values are replaced with "Unknown" factor level 
df <- df %>%
  mutate(
    # Numeric: Median imputation
    InsideSuperPremium = if_else(is.na(InsideSuperPremium), median(InsideSuperPremium, na.rm = TRUE), InsideSuperPremium),
    OutsideSuperPremium = if_else(is.na(OutsideSuperPremium), median(OutsideSuperPremium, na.rm = TRUE), OutsideSuperPremium),
    AgeNext = if_else(is.na(AgeNext), median(AgeNext, na.rm = TRUE), AgeNext),
    
    # Factor: Replace NA with "Unknown" using the updated function
    Super = fct_na_value_to_level(Super, level = "Unknown"),
    RolloverTaxRebate = fct_na_value_to_level(RolloverTaxRebate, level = "Unknown"),
    Gender = fct_na_value_to_level(Gender, level = "Unknown"),
    SmokerStatus = fct_na_value_to_level(SmokerStatus, level = "Unknown"),
    HomeState = fct_na_value_to_level(HomeState, level = "Unknown"),
    Occupation = fct_na_value_to_level(Occupation, level = "Unknown"),
    SelfEmployed = fct_na_value_to_level(SelfEmployed, level = "Unknown"),
    Life = fct_na_value_to_level(Life, level = "Unknown"),
    TPD = fct_na_value_to_level(TPD, level = "Unknown"),
    Trauma = fct_na_value_to_level(Trauma, level = "Unknown"),
    IP = fct_na_value_to_level(IP, level = "Unknown"),
    BE = fct_na_value_to_level(BE, level = "Unknown"),
    Severity = fct_na_value_to_level(Severity, level = "Unknown"),
    PremiumFrequency = fct_na_value_to_level(PremiumFrequency, level = "Unknown")
  )


# 7. Summary of cleaned data
df_clean <- df
print(dfSummary(df_clean))

# Export cleaned data to CSV
write_csv(df_clean, "A3_cleaned.csv")

# 8. Other possible considerations
# summarise missing values
df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Column", values_to = "MissingCount") %>%
  arrange(desc(MissingCount)) %>%
  print(n = Inf)

# 'Alternative' could be dropped (81.1% missing)

# 27 missing values of LifeId and AdviserId
  # not predictors, so these missing values are not critical in immediate access 
  # if we want to drop these rows:  
  # df <- df %>% filter(!is.na(LifeId) & !is.na(AdviserID))

# missing values in the fields extracted from 'CommissionStructure'
# Commision_Upfront, Commission_Renewal
  # these missing values represent no upfront or renewal commission 