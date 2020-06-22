library("tidyverse")
library("recommenderlab")
set.seed(1234)
dataset <- read_csv("http://deepyeti.ucsd.edu/jianmo/amazon/categoryFilesSmall/Kindle_Store.csv", col_names= c("User","Item","Rating","TimeStamp"), col_types = list(col_character(), col_character(),col_double(),col_double())) %>% data.frame()
b <- dataset[,c(1:3)]
b_dist <- distinct(b)
r <- as(b_dist,"realRatingMatrix")
r_df <- as(r, "data.frame")
r_df <- subset(md, rating <= 5)
r_df <- subset(md, rating > 0)
rr <- as(r_df, "realRatingMatrix")
min_n_item <- quantile(rowCounts(rr),0.999)
min_n_users <- quantile(colCounts(rr),0.999)
rr_subset <- rr[rowCounts(rr) > min_n_item, colCounts(rr) > min_n_users]
rr_subset_df <- as(rr_subset, "data.frame")
rr_subset_df_m <- rr_subset_df %>% pivot_wider(names_from = item, values_from = rating)
all_na <- function(x) any(!is.na(x))
rr_subset_df_m <- rr_subset_df_m %>% select_if(all_na)
rr_subset_df_m[is.na(rr_subset_df_m)] = mean(as.matrix(rr_subset_df_m[, 2:1818]), na.rm = TRUE)
A <- as.matrix(rr_subset_df_m[, 2:1818])
x <- svd(A)
U <- x$u
sig <- x$d
V <- x$v
