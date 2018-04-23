diamonds <- diamonds
library(tidyverse)
library(ruler)

# Observation is not an outlier based on z-score if its absolute value of default z-score is lower then some threshold (popular choice is 3).
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}

# Observation is not an outlier based on MAD if its absolute value of z-score with median as center and MAD as normalization unit is lower then some threshold (popular choice is 3).
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}
#Tukey's fences is a technique used in box plots. The non-outlier range is defined with \([Q_1 - k(Q_3 - Q_1),~ Q_3 + k(Q_3 - Q_1)]\),
# where \(Q_1\) and \(Q_3\) are the lower and upper quartiles respectively, \(k\) - some nonnegative constant (popular choice is 1.5).
# Observation is not an outlier based on Tukey's fences if its value lies in non-outlier range.
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}
#All previous approaches were created for univariate numerical data. To detect outliers in multivariate case one can use Mahalanobis distance to reduce to univariate case and then apply known techniques.
# Observation is not an outlier based on Mahalanobis distance if its distance is not an outlier.
maha_dist <- . %>% select_if(is.numeric) %>%
  mahalanobis(center = colMeans(.), cov = cov(.))

isnt_out_maha <- function(tbl, isnt_out_f, ...) {
  tbl %>% maha_dist() %>% isnt_out_f(...)
}


#Combination

isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)


diamonds %>% transmute_if(is.numeric, isnt_out_funs)

diamonds %>%
  transmute(maha = maha_dist(.)) %>%
  transmute_at(vars(maha = maha), isnt_out_funs)

data_tbl <- diamonds %>%
  unite(col = "group", cut, color, clarity)

compute_group_non_outliers <- . %>%
  # Compute per group mean values of columns
  group_by(group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  # Detect outliers among groups
  mutate_if(is.numeric, isnt_out_funs) %>%
  # Remove unnecessary columns
  select_if(Negate(is.numeric))

data_tbl %>% compute_group_non_outliers()


row_packs_isnt_out <- row_packs(
  # Non-outliers based on some column
  column = . %>% transmute_if(is.numeric, isnt_out_funs),
  # Non-outliers based on Mahalanobis distance
  maha = . %>% transmute(maha = maha_dist(.)) %>%
    transmute_at(vars(maha = maha), isnt_out_funs)
)

group_packs_isnt_out <- group_packs(
  # Non-outliers based on grouping
  group = compute_group_non_outliers,
  .group_vars = "group"
)

# Don't remove obeyers to compute total number of applied rules
full_report <- data_tbl %>%
  expose(row_packs_isnt_out, group_packs_isnt_out,
         .remove_obeyers = FALSE) %>%
  get_report()

used_rules <- full_report %>%
  distinct(pack, rule)

breaker_report <- full_report %>%
  filter(!(value %in% TRUE))

group_breakers <- breaker_report %>%
  # Filter group packs
  filter(pack == "group") %>%
  # Expand rows by matching group with its rows
  select(-id) %>%
  left_join(
    y = data_tbl %>% transmute(var = group, id = 1:n()),
    by = "var"
  ) %>%
  select(pack, rule, var, id, value)

outliers <- bind_rows(
  breaker_report %>% filter(pack != "group"),
  group_breakers
) %>%
  select(pack, rule, id)

# Not all group based definitions resulted with outliers
outliers %>%
  count(pack, rule) %>%
  filter(pack == "group") %>%
  print(n = Inf)

outliers %>%
  count(pack, rule, sort = TRUE)

outlier_score <- outliers %>%
  group_by(id) %>%
  # nrow(used_rules) equals total number of applied methods
  summarise(score = n() / nrow(used_rules))

# Top 10 outliers
outlier_score %>% arrange(desc(score)) %>% slice(1:10)

diam_tbl <- diamonds %>%
  mutate(id = 1:n()) %>%
  left_join(y = outlier_score, by = "id") %>%
  mutate(
    score = coalesce(score, 0),
    is_out = if_else(score > 0.2, "Outlier", "Not outlier")
  )

# Total number of outliers
sum(diam_tbl$score > 0.2)


theme_set(theme_bw())

plot_outliers <- function(tbl, x, y, facet_var) {
  tbl %>%
    arrange(is_out) %>%
    ggplot(aes_string(x, y, colour = "is_out")) +
    geom_point() +
    facet_wrap(facets = facet_var) +
    scale_colour_manual(values = c("#AAAAAA", "#004080")) +
    guides(colour = guide_legend(title = NULL,
                                 override.aes = list(size = 4))) +
    labs(title = paste0("Strong outliers illustration by ", facet_var)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14))
}

diam_tbl %>% plot_outliers("carat", "price", facet_var = "cut")

diam_tbl %>% plot_outliers("x", "depth", facet_var = "color")

diam_tbl %>% plot_outliers("price", "table", facet_var = "clarity")

#Based on those plots we see the complicated nature of "strong outliers". They are not necessary located "on the edge" of two-dimensional scatter plots, but most extreme cases are tagged as outliers.
#Also one interesting observation: most outliers are concentrated in the combination of "Fair" cut, "J" colour and "I1" clarity which are worst options among their features. The reason of this effect is group-based definitions of non-outliers which tagged certain groups more than others:
  

breaker_report %>%
  filter(pack == "group") %>%
  count(var, sort = TRUE) %>%
  print(n = 10)

#Conclusions
#???	Using only basic outlier detection methods one can achieve insightful results by combining them. Observations which are tagged as outlier by more than some threshold number of methods might be named as "strong outliers". Those should be considered as outliers based on the whole data rather then on separate features.
#???	With ruler combining results of several outlier detection methods is straightforward due to the format of tidy data validation report.
#???	Suggested "strong outlier" observations in diamonds dataset are not only those with extreme numerical values but also ones based on quality of diamonds. This is achieved without prior knowledge of "diamond quality" notion.

