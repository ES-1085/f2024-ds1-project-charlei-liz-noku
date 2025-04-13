Project memo
================
Liz, Charlie, Noku - Healthy Androscoggin - Smoke Trends Analysis

This document contains a detailed account of the data clean up for our
data and the design choices we are making for the plots.

## Data Clean Up Steps for Overall Data

### Step 1: Transforming PDFs documents into Excel tables

Our community partner data was presented as a collection of PDF
documents. Thus, our first step of data cleaning process involved
transforimg presented data into Excel files. To achieve that, we used
PDF to Excel Converter online
(<https://acrobat.adobe.com/link/acrobat/pdf-to-excel?x_api_client_id=adobe_com&x_api_client_location=pdf_to_excel>).

### Step 2: Data Cleaning within Excel

Next, we proceeded to clear formatting of the tables we saved from PDF
documents. After that, we conducted some simple data preparation,
including removal of empty rows and columns within each year’s dataframe
and change of column names to be consistent across years.

### Step 3: Downloading Excel data as CSV files

During this step we downloaded all CSV files representing smoke data for
year 2009, 2013, 2015, 2017, 2019, and 2021.

### Step 3: Data Cleaning with R

Initially, we tried to clean each `smoke[year].csv` data file
separately, which involved a lot of copy-pasting of the same code. Thus,
we decided to write a function to achieve better efficiency.

In this function, we clean and reshape survey data from a CSV file into
a tidy format. We begin by reading the file and renaming the columns for
clarity. Then, we assign a group_type (such as Age, Grade, or
Race/Ethnicity) based on patterns in the data and remove header rows.
Next, we create three separate data frames for total, female, and male
responses, converting percentages to numeric values and cleaning
confidence intervals. We then standardize the format across these data
frames, combine them into a single long-format dataset, and add the
survey year.

``` r
clean_data <- function(file_path, year) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)

  # Rename columns
  colnames(df) <- c(
    "category", "total_percent", "total_ci", "total_number",
    "female_percent", "female_ci", "female_number",
    "male_percent", "male_ci", "male_number"
  )

  # Add group_type
  df_clean <- df %>%
    mutate(group_type = case_when(
      str_detect(category, "----Age----") ~ "Age",
      str_detect(category, "----Grade----") ~ "Grade",
      str_detect(category, "----Race/Ethnicity----") ~ "Race/Ethnicity",
      TRUE ~ NA_character_
    )) %>%
    fill(group_type, .direction = "down") %>%
    filter(!str_detect(category, "----"))

  # Build long format using mutate + select
  total_df <- df_clean %>%
    mutate(
      gender = "total",
      percent = as.numeric(str_remove(total_percent, "%")),
      ci = ifelse(total_ci %in% c("^", "*"), NA, total_ci),
      number = as.numeric(total_number)
    ) %>%
    select(subgroup = category, group_type, gender, percent, ci, number)

  female_df <- df_clean %>%
    mutate(
      gender = "female",
      percent = as.numeric(str_remove(female_percent, "%")),
      ci = ifelse(female_ci %in% c("^", "*"), NA, female_ci),
      number = as.numeric(female_number)
    ) %>%
    select(subgroup = category, group_type, gender, percent, ci, number)

  male_df <- df_clean %>%
    mutate(
      gender = "male",
      percent = as.numeric(str_remove(male_percent, "%")),
      ci = ifelse(male_ci %in% c("^", "*"), NA, male_ci),
      number = as.numeric(male_number)
    ) %>%
    select(subgroup = category, group_type, gender, percent, ci, number)

  # Combine and add year
  df_long <- bind_rows(total_df, female_df, male_df) %>%
    mutate(year = year)

  return(df_long)
}
```

### Step 4: Call Function on All CSV Files

In this step, we use the function outlined above to clean each
`smoke[year].csv` file separately.

``` r
df_2009 <- clean_data("/cloud/project/data/smoke2009.csv", 2009)
df_2013 <- clean_data("/cloud/project/data/smoke2013.csv", 2013)
df_2015 <- clean_data("/cloud/project/data/smoke2015.csv", 2015)
df_2017 <- clean_data("/cloud/project/data/smoke2017.csv", 2017)
df_2019 <- clean_data("/cloud/project/data/smoke2019.csv", 2019)
df_2021 <- clean_data("/cloud/project/data/smoke2021.csv", 2021)
```

### Step 5: Combine Each Year’s Dataframe into a Single Dataframe

Lastly, we combine all dataframes we have recieved through the process
of cleaning into a single dataframe called `smoke_data`.

``` r
# Combine all dataframes together

smoke_data <- bind_rows(df_2009, df_2013, df_2015, df_2017, df_2019, df_2021)
```

## Plots

You will want to document choices you’ve made that were intentional for
your graphic, e.g. color you’ve chosen for the plot. Think of this
document as a code script someone can follow to reproduce the data
cleaning steps and graphics in your handout.

### ggsave example for saving plots

``` r
p1 <- starwars |>
  filter(mass < 1000, 
         species %in% c("Human", "Cerean", "Pau'an", "Droid", "Gungan")) |>
  ggplot() +
  geom_point(aes(x = mass, 
                 y = height, 
                 color = species)) +
  labs(x = "Weight (kg)", 
       y = "Height (m)",
       color = "Species",
       title = "Weight and Height of Select Starwars Species",
       caption = paste("This data comes from the starwars api: https://swapi.py43.com"))


ggsave("example-starwars.png", width = 4, height = 4)

ggsave("example-starwars-wide.png", width = 6, height = 4)
```

### Plot 1: \_\_\_\_\_\_\_\_\_

#### Data cleanup steps specific to plot 1

These data cleaning sections are optional and depend on if you have some
data cleaning steps specific to a particular plot

#### Final Plot 1

### Plot 2: \_\_\_\_\_\_\_\_\_

### Plot 3: \_\_\_\_\_\_\_\_\_\_\_

Add more plot sections as needed. Each project should have at least 3
plots, but talk to me if you have fewer than 3.

### Plot 4: \_\_\_\_\_\_\_\_\_\_\_
