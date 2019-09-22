# gghpcc
## install
if(!require(ggcorrr))
  devtools::install_github("houyunhuang/ggcorrr")
devtools::install_github("houyunhuang/gghpcc")

## examples
library(ggcorrr)
library(ggmantel)
library(dplyr)
library(vegan)
data(varespec)
data(varechem)
## group by rows
spec_df <- varespec %>% 
  mutate(grp = rep(LETTERS[1:3], 8))

env_df <- varechem %>% 
  mutate(grp = rep(LETTERS[1:3], 8))

df_grp <- fortify_mantel(spec_df, env_df, 
               spec_group = "grp",
               env_group = "grp",
               mantel_fun = "mantel",
               pair_test = TRUE,
               env_corr_params = list(type = "upper", 
                                      show_diag = FALSE,
                                      corr_test = TRUE, 
                                      cluster = TRUE))
ggmantel(df_grp)

# four group
df4 <- fortify_mantel(varespec, varechem, 
                     spec_select = list(spec01 = 22:25, 
                                        spec02 = 1:4, 
                                        spec03 = 38:43, 
                                        spec04 = 15:20),
                     mantel_fun = "mantel",
                     env_corr_params = list(type = "upper", 
                                            show_diag = FALSE,
                                            corr_test = TRUE, 
                                            cluster = TRUE))
ggmantel(df4) # 四组

# three group
df3 <- fortify_mantel(varespec, varechem, 
                      spec_select = list(spec01 = 22:25, 
                                         spec02 = 1:4, 
                                         spec03 = 38:43),
                      mantel_fun = "mantel",
                      env_corr_params = list(type = "upper", 
                                             show_diag = FALSE,
                                             corr_test = TRUE, 
                                             cluster = TRUE))
ggmantel(df3)

# two group
df2 <- fortify_mantel(varespec, varechem, 
                      spec_select = list(spec01 = 22:25, 
                                         spec02 = 1:4),
                      mantel_fun = "mantel",
                      env_corr_params = list(type = "upper", 
                                             show_diag = FALSE,
                                             corr_test = TRUE, 
                                             cluster = TRUE),
                      get_link_params = list())
ggmantel(df2)

spec <- list(spec01 = varespec[ , 22:25],
             spec02 = varespec[ , 1:4])
df <- ggmantel::fortify_mantel(spec, varechem, process = FALSE,
                               mdist_params = list(method = "euclidean"))
corr_df <- fortify_corr(varechem, type = "upper", show_diag = FALSE,
                        corr_test = TRUE, cluster = TRUERUE)
mantel_df <- ggmantel::get_link_data(df, corr_df, type = "upper", grp_hjust = c(0, -1.5, -1, 0))
ggmantel::ggmantel(mantel_df, corr_df, type = "upper", corr_type = "square", curvature = 0.05)
# 1 group
df1 <- fortify_mantel(varespec, varechem, 
                      spec_select = list(spec01 = 22:25),
                      mantel_fun = "mantel",
                      env_corr_params = list(type = "lower", 
                                             show_diag = FALSE,
                                             corr_test = TRUE, 
                                             cluster = TRUE))
ggmantel(df1, curvature = 0.05)

