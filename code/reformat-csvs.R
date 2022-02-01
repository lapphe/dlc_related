#Hannah Lapp
#Format and clean up dlc output files: top three columns merged to column headers
#saved as new cav files with "ready" at end of file name


#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(purrr)

#Functions----------------------------------------------------------------------

#fix column headers
fix_colnames <- function(df){
ind <- as.list(df[1,])
bodypart <- as.list(df[2,])
coor <- as.list(df[3,])
combined <- paste(ind, bodypart, coor, sep = '-')
colnames(df) <- combined
df <- df[-(1:3),]
colnames(df)[1] <- "frame"
rownames(df) <- 1:nrow(df)
return(df)
}

#open csv file
read_file <- function(file) { 
  open1 <- paste(folder, "/", file, sep = "")
  df2 <- read.csv(open1)
  return(df2)}
#Load data (edit these!!!)------------------------------------------------------
setwd("C:/Users/psyc-hel488/Documents/LBN crossover study/dlc-huddle/practice-files")

#full path to directory with dlc output:
folder <- "C:/Users/psyc-hel488/Documents/LBN crossover study/dlc-huddle/practice-files"

#where to save your new cleaned up csvs:
save_path <- "C:/Users/psyc-hel488/Documents/LBN crossover study/dlc-huddle/reformatted_csvs/"

#for a specific csv, enter path here:
df <- read.csv("./B23_huddle1_27_03_2021__14_47DLC_dlcrnetms5_pup-huddle-testSep8shuffle1_200000_el_filtered.csv")

#convert all csv in a folder----------------------------------------------------

files <- as.list(dir(folder))
converted_files <-  map(files, read_file)
reformatted <-  map(converted_files, fix_colnames)
save_names <- files %>% 
  str_replace(".csv", "ready.csv")
save_names <- paste(save_path, save_names, sep= "")
names(reformatted) <- save_names
num_files <- as.numeric(length(reformatted))

#save your files
for (i in 1:num_files) {
  df <- reformatted[[i]]
  zebra <- save_names[[i]]
  write.csv(df, zebra)
}
