# Add the columns present in df2 but missing in df2 to df1

add_miss_cols <- function(df1, df2) {

    diff.col.names <- setdiff(names(df2), names(df1))

    df1[diff.col.names] <- NA

    return(df1)

}

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

recode_party_w1 <- function(df) {

    df[df == 1] <- "Independent"
    df[df == 2] <- "Democrat"
    df[df == 3] <- "Republican"

    return(df)
}

recode_party_w23 <- function(df) {

    df[df == 1] <- "Republican"
    df[df == 2] <- "Independent"
    df[df == 3] <- "Democrat"
    df[df == 4] <- NA
    df[df == 5] <- NA

    return(df)
}

# Recode gender

recode_dummy <- function(df) {

    df <- if_else(df == 1, 1, 0)

    return(df)

}

# Replace NA with 0

replacena0 <- function(x) {
    x[which(is.na(x))] <- 0
    return(x)
}

# Normalize data
rescale01 <- function(x) {
    x <- scales::rescale(x, to = c(0, 1))
    return(x)
}

# Reverse code
reverse <- function(x) {

    out <- (max(x, na.rm = T) + 1) - x

    return(out)
}

## for bootstrapping 95% confidence intervals; Borrowed from Nick Camp's code from Jaren, Nick, and my shared project

theta <- function(x, xdata, na.rm = T) {
    mean(xdata[x], na.rm = na.rm)
}

ci.low <- function(x, na.rm = T) {
    mean(x, na.rm = na.rm) - quantile(bootstrap::bootstrap(1:length(x), 1000, theta, x, na.rm = na.rm)$thetastar, .025, na.rm = na.rm)
}

ci.high <- function(x, na.rm = T) {
    quantile(bootstrap::bootstrap(1:length(x), 1000, theta, x, na.rm = na.rm)$thetastar, .975, na.rm = na.rm) - mean(x, na.rm = na.rm)
}

interpret_estimate <- function(model){

    # Control
    intercept <- model$estimate[model$term == "(Intercept)"]
    control <- exp(intercept) / (1 + exp(intercept))

    # Likelihood
    model <- model %>% filter(term != "(Intercept)")

    model$likelihood <- (exp(model$estimate) / (1 - control + (control * exp(model$estimate))))

    return(model)
}

# Calculate group mean

group_mean <- function(x){
    out <- df %>%
        group_by(wave_fac) %>%
        summarise(mean = mean(get(var.list[x]), na.rm = TRUE),
                  ci_high = ci.high(get(var.list[x])),
                  ci_low = ci.low(get(var.list[x])))

    return(out)
}

# tidy t-test

tidy_test <- function(x) {
tidy(t.test(subset(df, prior == 1) %>% pull({{x}}), subset(df, prior == 0) %>% pull({{x}})))
}
# Calculate model outputs

cal_model_outputs <- function(x) {

    lm.out <- lm(X2020likelyvote ~ gendiscrim + apa.discrim.rona + usborn + DEM + GOP + age + male + edu + factor(wave), data = x)

    lm.robust.out <- estimatr::lm_robust(X2020likelyvote ~ gendiscrim + apa.discrim.rona + usborn + DEM + GOP + age + male + edu + factor(wave), data = x)

    plm.out <- plm(X2020likelyvote ~ gendiscrim + apa.discrim.rona + usborn + DEM + GOP + age + male + edu + factor(wave), data = x,
                   model = "fd", index = c("pid", "wave"))

    lme.out <- lmer(X2020likelyvote ~ gendiscrim + apa.discrim.rona + usborn + DEM + GOP + age + male + edu + factor(wave) + (1|pid), data = x)

    model.outs <- bind_rows(
        tidy(lm.out, conf.int = TRUE) %>%
            mutate(model = "Within estimator"),

        tidy(lm.robust.out, conf.int = TRUE) %>%
            mutate(model = "Robust SE"),

        tidy(plm.out, conf.int = TRUE) %>%
            mutate(model = "FD estimator"),

        broom.mixed::tidy(lme.out, conf.int = TRUE) %>%
            mutate(model = "Mixed model") %>%
            dplyr::select(!matches("effect|group"))
    )

    return(model.outs)
}

cal_glm <- function(x) {

    glm.out <- glm(biden ~ gendiscrim + apa.discrim.rona + usborn + GOP + DEM + age + male + edu + factor(wave), data = x, family = "binomial")

    tidy(glm.out, conf.int = TRUE) %>%
        interpret_estimate() %>%
        mutate(term = recode(term,
                             "age" = "Age",
                             "usborn" = "Born in US",
                             "male" = "Male",
                             "gendiscrim" = "General discrimination",
                             "apa.discrim.rona" = "COVID discrimination",
                             "edu" = "Education",
                             "factor(wave)3" = "Wave 3",
                             "GOP" = "Republican",
                             "DEM" = "Democrat",
                             "apa.discrim.rona:linkedfate" = "COVID discrimination:Linked fate"))
}