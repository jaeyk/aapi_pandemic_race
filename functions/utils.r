
# Credit to Achim here:
# http://stackoverflow.com/questions/27367974/
# different-robust-standard-errors-of-logit-regression-in-stata-and-r
# for the code in line 14 and 15

robustse <- function(x, coef = c("logit", "odd.ratio", "probs")) {
    suppressMessages(suppressWarnings(library(lmtest)))
    suppressMessages(suppressWarnings(library(sandwich)))

    sandwich1 <- function(object, ...) sandwich(object) *
        nobs(object) / (nobs(object) - 1)
    # Function calculates SE's
    mod1 <- coeftest(x, vcov = sandwich1)
    # apply the function over the variance-covariance matrix

    if (coef == "logit") {
        return(mod1) # return logit with robust SE's
    } else if (coef == "odd.ratio") {
        mod1[, 1] <- exp(mod1[, 1]) # return odd ratios with robust SE's
        mod1[, 2] <- mod1[, 1] * mod1[, 2]
        return(mod1)
    } else {
        mod1[, 1] <- (mod1[, 1]/4) # return probabilites with robust SE's
        mod1[, 2] <- mod1[, 2]/4
        return(mod1)
    }
}

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

recode_affect_w12 <- function(df) {

    df[df == 2] <- 3
    df[df == 4] <- 2
    df[df == 1] <- 4

    return(df)
}

recode_party_w23 <- function(df) {

    df[df == 1] <- "Republican"
    df[df == 2] <- "Independent"
    df[df == 3] <- "Democrat"
    df[df == 4] <- "Other"
    df[df == 5] <- "Other"

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

    if (!is.numeric(x)) {

        x <- as.numeric(x)
    }

    x <- scales::rescale(x, to = c(0, 1))

    return(x)
}

# Reverse code
reverse <- function(x) {

    if (!is.numeric(x)) {

        x <- as.numeric(x)
    }

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

sub_group_mean <- function(df, x){

    out <- df %>%
        group_by(wave_fac) %>%
        summarise(mean = mean(get(var.list[x]), na.rm = TRUE),
                  ci_high = ci.high(get(var.list[x])),
                  ci_low = ci.low(get(var.list[x])))

    return(out)
}

plot_two_disc <- function(df) {

    df$proxy <- rep(subset(df, wave == 1)$gendiscrim, 3)

    plot_w12 <- df %>%
        filter(wave %in% c(1,2)) %>%
        group_by(proxy) %>%
        summarise(mean = mean(apa.discrim.rona, na.rm = TRUE),
                  ci_high = ci.high(apa.discrim.rona),
                  ci_low = ci.low(apa.discrim.rona)) %>%
        mutate(group = 2)

    plot_w13 <- df %>%
        filter(wave %in% c(1,3)) %>%
        group_by(proxy) %>%
        summarise(mean = mean(apa.discrim.rona, na.rm = TRUE),
                  ci_high = ci.high(apa.discrim.rona),
                  ci_low = ci.low(apa.discrim.rona)) %>%
        mutate(group = 3)

    plot_w123 <- bind_rows(plot_w12, plot_w13)

    plot_w123 %>%
        ggplot(aes(x = factor(proxy), y = mean,
                   ymax = mean + ci_high,
                   ymin = mean - ci_low,
                   col = factor(group))) +
        geom_pointrange() +
        ggrepel::geom_text_repel(aes(label = round(mean, 2))) +
        labs(x = "Response to general discrimination Q in Wave 1",
             y = "Average response to COVID-19 discrimination Q in Wave 2 and 3",
             col = "Wave") +
        geom_hline(yintercept = 0.5,  linetype = 'dotted', col = 'red', size = 1)
}

plot_two_disc_sub <- function(df) {

    df$proxy <- rep(subset(df, wave == 1)$gendiscrim, 3)

    plot_w12 <- df %>%
        filter(wave %in% c(1,2)) %>%
        group_by(proxy, edu) %>%
        summarise(mean = mean(apa.discrim.rona, na.rm = TRUE),
                  ci_high = ci.high(apa.discrim.rona),
                  ci_low = ci.low(apa.discrim.rona)) %>%
        mutate(group = 2)

    plot_w13 <- df %>%
        filter(wave %in% c(1,3)) %>%
        group_by(proxy, edu) %>%
        summarise(mean = mean(apa.discrim.rona, na.rm = TRUE),
                  ci_high = ci.high(apa.discrim.rona),
                  ci_low = ci.low(apa.discrim.rona)) %>%
        mutate(group = 3)

    plot_w123 <- bind_rows(plot_w12, plot_w13)

    plot_w123 %>%
        ggplot(aes(x = factor(proxy), y = mean,
                   ymax = mean + ci_high,
                   ymin = mean - ci_low)) +
        geom_pointrange() +
        ggrepel::geom_text_repel(aes(label = round(mean, 2))) +
        labs(x = "Response to general discrimination Q in Wave 1",
             y = "Average response to COVID-19 discrimination Q") +
        geom_hline(yintercept = 0.5,  linetype = 'dotted', col = 'red', size = 1) +
        facet_grid(edu~group)
}

# tidy t-test

tidy_test <- function(x) {
tidy(t.test(subset(df, prior == 1) %>% pull({{x}}), subset(df, prior == 0) %>% pull({{x}})))
}
# Calculate model outputs

cal_model_outputs <- function(x) {

    lm.out <- lm(likely_vote ~ gendiscrim + apa.discrim.rona + usborn + edu + DEM + GOP + age + male + wave_fac + korean + japanese, data = x)

    robust.out <- estimatr::lm_robust(likely_vote ~ gendiscrim + apa.discrim.rona + usborn + edu + DEM + GOP + age + male + wave_fac + korean + japanese, data = x)

    model.outs <- bind_rows(
        tidy(lm.out, conf.int = TRUE) %>%
            mutate(model = "Within estimator"),

        tidy(robust.out, conf.int = TRUE) %>%
            mutate(model = "Within estimator (with robust standard errors)")
    )

    return(model.outs)
}

cal_glm <- function(x) {

    glm.out <- glm(biden ~ gendiscrim + apa.discrim.rona + usborn + edu + income + DEM + GOP + age + male + factor(wave) + korean + japanese, data = x, family = "binomial")

    glm.tidy <- tidy(glm.out, conf.int = TRUE) %>%
    interpret_estimate() %>%
    mutate(model = "Within estimator")

    robust.out <- robustse(glm.out, coef = "logit")

    robust.tidy <- tidy(robust.out, conf.int = TRUE) %>%
        interpret_estimate() %>%
        mutate(model = "Within estimator (with robust standard errors)")


    model.outs <- bind_rows(glm.tidy, robust.tidy)

    model.outs <- model.outs %>%
        mutate(term = factor(term, levels = c("gendiscrim","apa.discrim.rona","usborn","edu","income","DEM","GOP","age","male","factor(wave)3","korean","japanese"))) %>%
        mutate(term = fct_rev(term)) %>%
        mutate(term = recode(term,
                             "gendiscrim" = "General discrimination",
                             "apa.discrim.rona" = "COVID-19 discrimination",
                             "usborn" = "US born",
                             "edu" = "Education",
                             "income" = "Income",
                             "DEM" = "Democratic Party",
                             "GOP" = "Republican Party",
                             "age" = "Age",
                             "male" = "Male",
                             "factor(wave)3" = "Wave3",
                             "korean" = "Korean",
                             "japanese" = "Japanese"))

    return(model.outs)
}

# run sensitivity analysis

run_sense <- function(model) {

    sense.out <- sensemakr(model = model,
                           treatment = "apa.discrim.rona",
                           benchmark_covariates = "DEM1",
                           kd = 1:3)

    out <- sense.out %>%
        summary()
}