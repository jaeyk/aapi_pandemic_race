# Calculate attrition rate
attr_rate <- function(pre, post) {
    out <- (pre - post)/pre
    round(out, 2)}

# Extract the non-wave part of the names from the column names

extract_name <- function(df, wave) {gsub(glue("{wave}_"), "", names(df)[str_detect(names(df), glue("{wave}"))])}

# Recode responses

recode_response <- function(df) {

    df <- 6 - df

    return(df)

}

# Recode party

recode_party <- function(df) {

    df[df == 1] <- "Independent"
    df[df == 2] <- "Democrat"
    df[df == 3] <- "Republican"

    return(df)
}

# Recode gender

recode_dummy <- function(df) {

    df <- if_else(df == 1, 1, 0)

    return(df)

}