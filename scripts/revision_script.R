
# Setup -------------------------------------------------------------------

library(tidyverse)

lm_df <- readr::read_csv(here::here("data/lm_data.csv"))

# Part 1 ------------------------------------------------------------------

model1 <- lm(productivity ~ coffee, data = lm_df)

set.seed(3424)

n <- 200

x <- rnorm(n, mean = 5, sd = 2) 
A <-  2*x + 3 + 2*rnorm(n) 
B <- -1.5*x + 20 + 3*rnorm(n)
C <- 1*x +10 + rnorm(n)
D <- 18 + 4*rnorm(n) 


df <- data.frame(x, A, B, C, D)

df_long <- df |> 
  tidyr::pivot_longer(
    cols = -x,
    names_to = "y_var",
    values_to = "y_val"
  )

## Plots
lm_mat <- ggplot(df_long, aes(x=x, y=y_val)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 10, 2))+
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  coord_cartesian(ylim = c(0, 30)) +
  labs(x = "x", y = "y") +
  facet_wrap(~y_var) +
  theme_bw()

lm_mat

mods <- df_long |> 
  dplyr::nest_by(y_var) |> 
  dplyr::mutate(model = list(lm(y_val ~ x, data = data)))

mods_sum <- mods |> 
  dplyr::summarise(
    b0 = summary(model)$coefficients[1],
    b1 = summary(model)$coefficients[2]
  )

order <- c(2,4,1,3)

mods_sum <- mods_sum[order,]

# Part 2 ------------------------------------------------------------------

model2 <- lm(productivity ~ coffee + wellbeing, data = lm_df)

## Custom print function
## Give correct b number (e.g. 0 for b0, 1 for b1 etc.)
print_b <- function(model, coef, conf.int = FALSE, ci.type = "t"){
  mod_out <- broom::tidy(model, conf.int = TRUE) |> dplyr::slice(coef+1)
  cis <- ""
  
  if(conf.int){
    if(ci.type == "t"){
      cis <- paste0(", 95% CI [", round(mod_out$conf.low, 2), ", ",
                    round(mod_out$conf.high, 2), "]")
    } else if(ci.type == "norm"){
      cis <- paste0(", 95% CI [", round(mod_out$estimate - mod_out$std.error*1.96, 2), ", ",
                    round(mod_out$estimate + mod_out$std.error*1.96, 2), "]")
    }
  }
  
  paste0("*b* = ", mod_out$estimate |> round(2),
         ", SE(*b*) = ", mod_out$std.error |> round(2),
         cis,
         ", *t* = ", mod_out$statistic |> round(2),
         ", *p* ", ifelse(mod_out$p.value >= .001, "= ", ""), papaja::print_p(mod_out$p.value)
  )  
}


# Part 3 ------------------------------------------------------------------


model3 <- lm(productivity ~ coffee + wellbeing + use_ai, data = lm_df)

m3_coeffs <- broom::tidy(model3) |> pull(estimate) |> round(2)

m2m3_aov <- anova(model2, model3)

