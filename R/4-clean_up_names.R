# this script cleans up the names to use a nicer format
# It was added a year after the initial NAPS_wrangling scripts
library(plyr)
library(tidyverse)

source("~/NAPS_project/NAPS_wrangling/R/0-setup_project.R")
load(file = paste0(wd$output, "NAPS_fine.rda"))
load(file = paste0(wd$output, "NAPS_coarse.rda"))


#-------------------------------------------------
# Function: mutate_using_mdl
# Replaces values below the mdl with 0.5*mdl
#-------------------------------------------------
mutate_using_mdl <- function(df, col, colmdl){
  col <- as.character(col)
  colmdl <- as.character(colmdl)
  df %>% mutate(!!rlang::ensym(col) := ifelse(!is.na(!!rlang::ensym(col)) & (!!rlang::ensym(col)  < !!rlang::ensym(colmdl)) ,
                                              0.5*!!rlang::ensym(colmdl), !!rlang::ensym(col) ))
}


final_fine_names <- read.csv("header_files/final_fine_names.csv")

NAPS_renamed <- vector("list", length = length(NAPS_fine))
names(NAPS_renamed) <- names(NAPS_fine)

for(i in 1:length(NAPS_fine)){
  NAPS_fine[[i]] <- NAPS_fine[[i]] %>% 
         select(-contains("flag"))
  df <- data.frame(idx = 1:nrow(NAPS_fine[[i]]))
  col <- 1
  for(j in 1:ncol(NAPS_fine[[i]])){
    if(names(NAPS_fine[[i]])[j] %in% final_fine_names$old_name){
      df$new_col <- NAPS_fine[[i]][, j]
      names(df)[col+1] <- final_fine_names[which(final_fine_names$old_name == names(NAPS_fine[[i]])[j]), "new_name2"]
      col <- col+1
    }
  }
  NAPS_renamed[[i]] <- df %>% select(-1) %>% 
    mutate(date = as.Date(date))
  gc()
}

NAPS_renamed_df <- ldply(NAPS_renamed, data.frame)
df_nonmdl_names <- grep("mdl", names(NAPS_renamed_df), invert = TRUE, value = TRUE)
df_mdl_names <- grep("mdl", names(NAPS_renamed_df), invert = FALSE, value = TRUE) 

for(i in df_nonmdl_names){
  if(paste0(i, "_mdl") %in% names(NAPS_renamed_df)){
    NAPS_renamed_df <- mutate_using_mdl(NAPS_renamed_df, i, paste0(i, "_mdl"))
  }
}


NAPS_renamed <- split(NAPS_renamed_df, ".id", drop = TRUE)
save(file = paste0(wd$output, "NAPS_renamed.rda"), NAPS_renamed)

