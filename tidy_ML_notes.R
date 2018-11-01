
# LCW: List Column Workflow -----------------------------------------------

# Three basic steps
## 1. Make a list column with nest()
## 2. Work with list column with map()
## 3. Simplify list columns with unnest() and map_*()
library(tidyverse)
data(gapminder, package = "dslabs")
datacamp_names <- c("country", "year", "infant_mortality", "life_expectancy", 
                    "fertility", "population", "gdpPercap")
setdiff(names(gapminder), datacamp_names)
setdiff(datacamp_names, names(gapminder))
d <- as_tibble(gapminder) %>% 
    inner_join(read_csv("data/gap_countries.csv"), 
               by = "country") %>% 
    mutate(country = as.factor(country)) %>% 
    select(-continent, -region) %>% 
    mutate(gdpPercap = as.integer(gdp/population)) %>% 
    select(-gdp) %>% 
    arrange(country)
    
    
d
# can group the tibble bny country and then nest those as a list column
gap_nested <- d %>% 
    group_by(country) %>% 
    nest()
head(gap_nested)

# apply functions with map()
pop_nested <- gap_nested %>% 
    mutate(pop_mean = map(data, ~ mean(.x$population, na.rm = TRUE))) %>% 
    unnest(pop_mean) # this is necessary to change pop_mean from a list column
head(pop_nested)

# do this more easily if you know the type of the output
pop_nested <- gap_nested %>% 
    mutate(pop_mean = map_dbl(data, ~ mean(.x$population, na.rm = TRUE)))
    # no longer need to unnest(pop_mean) as map_dbl() creates a vector
head(pop_nested)

# build models within the tibble
gap_models <- gap_nested %>% 
    mutate(model = map(data, ~ lm(life_expectancy ~ year, data = .x)))

# Work with list columns --------------------------------------------------

# use the broom, Metrics, and rsample libraries
# broom::tidy(), glance(), and augment() used a lot

# if we want the coefficients for the nested models, use tidy()
library(broom)
model_coef <- gap_models %>% 
    mutate(coef = map(model, ~ tidy(.x))) %>% 
    unnest(coef)

head(model_coef)
# visualise the distribution of the coefficient for year
model_coef %>% 
    filter(term == "year") %>% 
    ggplot(aes(x = estimate)) + 
    geom_histogram()

# add statistics for model performance with glance()
model_perf <- gap_models %>% 
    mutate(fits = map(model, ~ glance(.x))) %>% 
    unnest(fits)
head(model_perf)    

# can then use whatever statistic we want to assess which perform well or not
model_perf %>% 
    ggplot(aes(x = AIC)) + 
    geom_histogram()

best_models <- model_perf %>% 
    top_n(n = 4, wt = -AIC) # note use of - to get the lowest AIC, i.e. best models
best_models
worst_models <- model_perf %>% 
    top_n(n = 4, wt = AIC) 
worst_models


# use augment() to add fitted values and other stats to the original data
best_augmented <- best_models %>% 
    mutate(augmented = map(model, ~ augment(.x))) %>% 
    unnest(augmented)
best_augmented %>% 
    select(country, life_expectancy, .fitted, everything())
worst_augmented <- worst_models %>% 
    mutate(augmented = map(model, ~ augment(.x))) %>% 
    unnest(augmented)
worst_augmented %>% 
    select(country, life_expectancy, .fitted, everything())

bind_rows(best_augmented, worst_augmented) %>% 
    ggplot(aes(x = year)) + 
    geom_point(aes(y = life_expectancy)) + 
    geom_line(aes(y = .fitted, col = "red")) + 
    facet_wrap(~ country, scales = "free_y")

# can do something similar with multiple linear regression
gap_fullmodel <- gap_nested %>% 
    mutate(model = map(data, ~ lm(formula = life_expectancy ~ ., 
                                 data = .x)))
fullmodel_perf <- gap_fullmodel %>% 
    mutate(fit = map(model, ~ glance(.x))) %>% 
    unnest(fit)

fullmodel_perf %>% 
    filter(country %in% worst_models$country) %>% 
    select(country, AIC) %>% 
    rename(multi_AIC = AIC) %>% 
    inner_join(worst_models, by = "country") %>% 
    select(country, single_AIC = AIC, multi_AIC)
