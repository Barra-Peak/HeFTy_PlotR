#### EXTRACT AND PLOT PATHS FROM HeFTy OUTPUT FILES
## BARRA PEAK
## Jan. 03, 2025

## This script plots HeFTy inverse model tT paths saved as .txt files (one of 
  ## two default HeFTy saving options).
## The script can process models produced in HeFTy v1 or v2 but some options (e.g.,
  ## plotting path goodness-of-fit using a continuous color scale) are only possible with v2 outputs.
## See README for full information.

### REQUIRED TO RUN THE SCRIPT AS WRITTEN - IMPORTANT!
## 1. HeFTy inverse path results file exported from HeFTy as a .txt file
  ## this is read into the script as "file"
## 2. .xlsx or .xls file of individual or binned thermochronologic dates and uncertainty used in HeFty modeling
  ## this is used to make histogram plots of the predicted vs. observed dates
  ## if comparison of corrected dates is chosen for HeFTy path acceptance criteria, dates in data 
    ## file should be corrected
  ## if comparison of UNCORRECTED dates is chosen, dates in data file should be uncorrected (raw 
    ## dates)
## Example input files stored in the GitHub repository

##_________________________________________________________________________________________________
#### LOAD PACKAGES ----
## No modification necessary; however, packages must be already installed in R environment.
## if packages are not installed you will be prompted to do so
library(openxlsx) # read in and export microsoft excel files
library(tidyverse) # easier data manipulation
library(ggplot2) # easier plotting
library(viridis)
##_________________________________________________________________________________________________
#### USER CHOICES AND INPUTS - CHANGE PRIOR TO RUNNING SCRIPT----

### NOTE HEFTY VERSION USED
## comment out unwanted options, uncomment desired option:
#version = "Hv1"
version = "Hv2"

### PLOTTING OPTIONS ----
### ACCEPTANCE CRITERIA USED IN PLOT COLORSCHEMES
## 0.5 and 0.05 are the default goodness-of-fit thresholds used to designate good and acceptable paths in HeFty
## the user can change these in HeFty or here to see how paths are designated using a different criteria
## if criteria are stricter than criteria used in HeFTy, some paths will be designated "rejected" in final plots
gf <- 0.5 # goodness of fit threshold for "good" paths
af <- 0.05 # goodness of fit threshold for "acceptable" paths


### tT PATH DEPICTION OPTIONS
## comment out unwanted options, uncomment desired option:
plot_type <- c("HeFTy v1") # plot replicates discrete acceptance criteria used in HeFTy v1
#plot_type <- c("Min.GOF") # plot uses continuous color scale based on the minimum fit to a single data point for each path (e.g., Peak et al. 2021, Fig 3)
#plot_type <- c("HeFTy v2") # plot replicates discrete acceptance criteria used in HeFTy v2
#plot_type <- c("Com.GOF") # plot by acceptance criteria used in HeFty v2 continuous GOF scale

## Color scale for discrete color scale plots
## default is same color scheme as plots generated in HeFTy
## changes to color options for continuous color scale options require changes to the plotting script below
hefty.cols = c("Good"="magenta", "Acceptable" = "green", "Rejected" = "gray")

### CHOOSE WHETHER OR NOT TO PLOT HISTOGRAMS
## comment out unwanted options, uncomment desired option:
plot_hist <- c("yes")
#plot_hist <- c("no")

### SET TIME AND TEMPERATURE AXES FOR PLOTS
axes <- data.frame("Time" = c(1400,0), "Temp" = c(1000,0))

### IMPORT FILES ----
file <- read_lines(file.choose()) # choose HeFTy output txt file to open using popup window
#file <- read_lines("your file path here") # import using file path instead of file picker popup

if(plot_hist == "yes"){
  data_file <- read.xlsx(file.choose(), check.names = T) # choose data input txt file to open using popup window
  #data_file <- read.xlsx("your file path here", check.names = T) # import using file path instead of file picker popup
}

### SAVING PLOTS ----
## if using R studio, plots don't show up in the plot viewer because they're generated in a loop
setwd("~/Desktop") # set where you want to save the plots
Name <- "Example" # sample/model name to use in plot title and file name

##_________________________________________________________________________________________________________

#### PLOTTING SCRIPT - NO USER CHANGES REQUIRED ----

#### Isolate Constraints ----
## Constraints are bracketed by entries "Constraints" and "Inversion Completed"
## Constraints are numbered
cons_bounds1 <- which(str_detect(file, "^Constraints")) # find first index of bounds
cons_bounds2 <- which(str_detect(file, "^Inversion completed")) # find second index of bounds
cons <- file[(cons_bounds1[1]+2):(cons_bounds2[1]-1)] # subset paths
cons <- matrix(cons) # convert to matrix for easier parsing

cons_parse <- strsplit(cons, '\t')
cons.df <- data.frame()
for (i in 1:length(cons_parse)){
  ci <- cons_parse[[i]]
  df <- data.frame("Num" = rep(ci[1], length.out = 4), 
                   "Time" = c(as.numeric(ci[3]), as.numeric(ci[3]), as.numeric(ci[2]), as.numeric(ci[2])), 
                   "Temp" = c(as.numeric(ci[5]), as.numeric(ci[4]), as.numeric(ci[4]), as.numeric(ci[5])),
                   "Type" = "Constraint")
  cons.df <- rbind(cons.df, df) # combine data frames
}

cons.df$Type <- as.factor(cons.df$Type)

#### Isolate Paths ----
## Paths are bracketed by entries that begin with "Individual Paths"
## followed by 3 lines of header

path_bounds <- which(str_detect(file, "^Individual paths")) #find index of bounds
path_data <- file[(path_bounds[1]+4):length(file)] # subset paths
path_data <- matrix(path_data) # convert to matrix for easier parsing
# each path is 2 lines
path_data_parse <- data.frame(t(sapply(strsplit(path_data, '\t'), c))) # make into data frame

#### Subset longform data ----
row_odd <- seq_len(nrow(path_data_parse)) %% 2 # create odd and even row index
time.rows <- path_data_parse[row_odd == 1, ] # extract odd rows which contain time steps, bin predictions
temp.rows <- path_data_parse[row_odd == 0, ] # extract even rows which contain temp steps, GOF
num_path <- nrow(time.rows) # total number of paths
num_dates <- sum(str_detect(sapply(strsplit(file[(path_bounds[1]+3)], '\t'), c), "^GOF"), na.rm = TRUE) # find the number of modeled dates
temp.rows$X2 <- as.numeric(temp.rows$X2)
time.rows$v1 <- rep("", num_path)
time.rows$minGOF <- rep("", num_path) # isolate minimum GOF value
temp.rows$v2 <- rep("", num_path)

## add HeFTy defualt path designations
gof.cols <- which(str_detect(sapply(strsplit(file[(path_bounds[1]+3)], '\t'), c), "^GOF")) # index of colums for individual GOF values
for(i in 1:num_path){
  ## HeFTy v1
  no <- as.numeric(temp.rows[i, gof.cols]) <= af
  good <- as.numeric(temp.rows[i, gof.cols]) >= 1/(num_dates + 1) & mean(as.numeric(temp.rows[i, gof.cols])) >= gf
  time.rows$minGOF[i] <- min(as.numeric(temp.rows[i, gof.cols])) # isolate minimum GOF value for all bins
  if(any(no) == T){
    time.rows$v1[i] <- "Rejected" # HeFTy v1 would have rejected this path
  } else if(all(good) == T){
    time.rows$v1[i] <- "Good" # HeFTy v1 would have designated this a good fit path
  } else if(all(no) == F && all(good) == F){
    time.rows$v1[i] <- "Acceptable" # HeFTy v1 would have designated this an acceptable fit path
  }
  
  if(version == "Hv2"){
    if(temp.rows[i,2] < af){
      temp.rows$v2[i] <- "Rejected" # HeFTy v2 would have rejected this path
    } else if(temp.rows[i,2] >= gf){
      temp.rows$v2[i] <- "Good" # HeFTy v2 would have designated this a good fit path
    } else{
      temp.rows$v2[i] <- "Acceptable" # HeFty v2 would have designated this an acceptable fit path
    }
  } else if(version == "Hv1"){
    temp.rows$v2[i] <- NA
  }
  
}

## add extra column if models were generated in HeFTy v1
if(version == "Hv1"){
  temp.rows <- temp.rows %>%
    add_column(Com.GOF = NA, .after = "X1")
  time.rows <- time.rows %>%
    add_column(Com.GOF = NA, .after = "X1")
}

## pivot longer to put in plotting format
path.start <- which(time.rows[1, ] == "Time (Ma)", arr.ind = TRUE)[2] #find start of path time/temp steps
piv <- names(time.rows[ , (path.start + 1):(ncol(time.rows)-2)]) # column names to pivot
time.long <- time.rows %>% pivot_longer(cols = all_of(piv), values_to = "Time") # all_of() silences the warning message 
temp.long <- temp.rows %>% pivot_longer(cols = all_of(piv), values_to = "Temp")

## create columns headers 
time.header <- c("Path", "Delete") # "Delete" is an empty column retained from the original HeFTy file
temp.header <- c("Path1", "Com.GOF") # "Path1" is a duplicate of "Path" and will be deleted when combined
for (i in 1:num_dates){
  time.header <- c(time.header, paste("Date", i, sep = ".")) # save preds.df column names for later use
  temp.header <- c(temp.header, paste("GOF", i, sep = ".")) # save paths.df column names for later use
}
time.header <- c(time.header, "Delete2", "v1","minGOF","name1", "Time") # "Delete" columns are duplicates between the two vectors
temp.header <- c(temp.header, "Delete3", "v2","name2", "Temp")
colnames(time.long) <- time.header
colnames(temp.long) <- temp.header

## combine time and temp dataframes to create one dataframe with all information for plotting
com.paths <- cbind(time.long,temp.long)
com.paths.clean = subset(com.paths, select = -c(Delete, Delete2, Delete3, name1, name2, Path1))
paths.df.1 <- com.paths.clean %>% select(Path, v1, v2, minGOF, Time, Temp, everything())
paths.df <- paths.df.1 %>% mutate_at(4:ncol(paths.df.1), as.numeric) # convert numeric values

#### PLOTTING ----

### Create color scales ----
## custom color scales for individual plots
# # Minimum GOF continuous color scale limits
# min.limits <- c(plyr :: round_any(min(paths.df$minGOF), 0.1, f = floor), plyr :: round_any(max(paths.df$minGOF), 0.1, f = ceiling))
# min.breaks <-  seq(min.limits[1], min.limits[2], by = 0.1)
# 
# # Combined GOF continuous color scale limits
# con.limits <- c(0, plyr :: round_any(max(paths.df$Com.GOF), 0.1, f = ceiling))
# con.breaks <-  seq(0, con.limits[2], by = 0.1)

## plot labels with number of paths and modeled dates
label = paste(paste("n paths =", num_path, sep = " "), paste("n dates =", num_dates, sep = " "), sep = "\n")

### PLOTS ----
if(plot_type == "HeFTy v1"){ 
  ## order paths by good or acceptable
  df <- paths.df %>% 
    mutate(v1 = factor(v1, levels = c("Rejected", "Acceptable", "Good"))) 

  ## plot
  tT.Plot.v1 <- ggplot() +
    geom_path(group_by(df, Path), mapping = aes(x = Time, y = Temp, group = interaction(Path, v1), color = v1), linewidth = 0.5, , show.legend = TRUE) + 
    scale_color_manual(name = paste(plot_type,"Path Fit", sep = "\n"), values = hefty.cols, drop = FALSE) +
    scale_x_reverse(name = "Time (Ma)") +
    scale_y_reverse(name = "Temperature (°C)") +
    coord_cartesian(xlim = axes$Time , ylim = axes$Temp) +
    
    annotate("text", x = axes$Time[2] + 3*(axes$Time[1]-axes$Time[2])/10, y = axes$Temp[1], label = label, hjust = -0.08, size = 3) +
  
    labs(title = paste(Name, version, sep = " ")) +
    geom_polygon(data = cons.df[cons.df$Type %in% "Constraint", ], mapping = aes(x = Time, y = Temp, group = Num), colour = "black", linewidth = 1, fill = "transparent") +
    theme(panel.background = element_rect(fill = "transparent", color = "transparent", linewidth = 1),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
          axis.ticks = element_line(colour = "black", linewidth = 0.5),
          axis.ticks.length = unit(-0.2,"cm"),
          panel.grid.major = element_line(colour = "grey70", linewidth = 0.5, linetype = 'dashed'),
          plot.margin = grid::unit(c(0,0,0,0), "mm"))

  ## SAVE PLOT
  ggsave(paste(Name,version, plot_type, "discrete.tT","pdf", sep = "."), plot = tT.Plot.v1, width = 6.5, height = 6)
  
} else if(plot_type == "Min.GOF"){
  ## order paths by good or acceptable
  df <- paths.df %>%
    ungroup() %>%
    arrange(minGOF) %>%
    mutate(Path.fac = factor(Path, levels = unique(Path)))
    
  
  ## plot
  tT.Plot.min <- ggplot() +
    geom_path(group_by(df, Path), mapping = aes(x = Time, y = Temp, group = Path.fac, color = minGOF), linewidth = 0.5) + 
    #scale_color_viridis(name = "Minimum GOF", limits = min.limits, breaks = min.breaks, option = "A") + # individual scale
    scale_color_viridis(name = "Minimum GOF", limits = c(0, 1), option = "A") + # default comparable scale
    scale_x_reverse(name = "Time (Ma)") +
    scale_y_reverse(name = "Temperature (°C)") +
    coord_cartesian(xlim = axes$Time , ylim = axes$Temp) +
    
    annotate("text", x = axes$Time[2] + 3*(axes$Time[1]-axes$Time[2])/10, y = axes$Temp[1], label = label, hjust = -0.08, size = 3) +
    
    labs(title = paste(Name, version, sep = " ")) +
    geom_polygon(data = cons.df[cons.df$Type %in% "Constraint", ], mapping = aes(x = Time, y = Temp, group = Num), colour = "black", linewidth = 1, fill = "transparent") +
    theme(panel.background = element_rect(fill = "transparent", color = "transparent", linewidth = 1),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
          axis.ticks = element_line(colour = "black", linewidth = 0.5),
          axis.ticks.length = unit(-0.2,"cm"),
          panel.grid.major = element_line(colour = "grey70", linewidth = 0.5, linetype = 'dashed'),
          plot.margin = grid::unit(c(0,0,0,0), "mm"))
  
  ## SAVE PLOT
  ggsave(paste(Name,version, plot_type,"tT.pdf", sep = "."), plot = tT.Plot.min, width = 6.5, height = 6)
  
} else if(plot_type == "HeFTy v2"){
  ## order paths  by discrete order
  df <- paths.df %>%
    mutate(v2 = factor(v2, levels = c("Rejected", "Acceptable", "Good")))
  
  ## plot
  tT.Plot.v2 <- ggplot() +
    geom_path(group_by(df, Path), mapping = aes(x = Time, y = Temp, group = interaction(Path, v2), color = v2), linewidth = 0.5, , show.legend = TRUE) + 
    scale_color_manual(name = paste(plot_type,"Path Fit", sep = "\n"), values = hefty.cols, drop = FALSE) +
    scale_x_reverse(name = "Time (Ma)") +
    scale_y_reverse(name = "Temperature (°C)") +
    coord_cartesian(xlim = axes$Time , ylim = axes$Temp) +
    
    annotate("text", x = axes$Time[2] + 3*(axes$Time[1]-axes$Time[2])/10, y = axes$Temp[1], label = label, hjust = -0.08, size = 3) +
    
    labs(title = paste(Name, version, sep = " ")) +
    geom_polygon(data = cons.df[cons.df$Type %in% "Constraint", ], mapping = aes(x = Time, y = Temp, group = Num), colour = "black", linewidth = 1, fill = "transparent") +
    theme(panel.background = element_rect(fill = "transparent", color = "transparent", linewidth = 1),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
          axis.ticks = element_line(colour = "black", linewidth = 0.5),
          axis.ticks.length = unit(-0.2,"cm"),
          panel.grid.major = element_line(colour = "grey70", linewidth = 0.5, linetype = 'dashed'),
          plot.margin = grid::unit(c(0,0,0,0), "mm"))
  
  ## SAVE PLOT
  ggsave(paste(Name,"version", plot_type, "discrete.tT", "pdf", sep = "."), plot = tT.Plot.v2, width = 6.5, height = 6)
  
} else if(plot_type == "Com.GOF"){
  ## order paths by good or acceptable
  df <- paths.df %>%
    ungroup() %>%
    arrange(Com.GOF) %>%
    mutate(Path.fac = factor(Path, levels = unique(Path)))
  
  ## plot
  tT.Plot.comb <- ggplot() +
    geom_path(group_by(df, Path), mapping = aes(x = Time, y = Temp, group = Path.fac, color = Com.GOF), linewidth = 0.5) + 
    #scale_color_viridis(name = "Combined GOF", limits = con.limits, breaks = con.breaks) + # individual scale
    scale_color_viridis(name = "Combined GOF", limits = c(0, 1)) + # default comparable scale
    scale_x_reverse(name = "Time (Ma)") +
    scale_y_reverse(name = "Temperature (°C)") +
    coord_cartesian(xlim = axes$Time , ylim = axes$Temp) +
    
    annotate("text", x = axes$Time[2] + 3*(axes$Time[1]-axes$Time[2])/10, y = axes$Temp[1], label = label, hjust = -0.08, size = 3) +
    
    labs(title = paste(Name, version, sep = " ")) +
    geom_polygon(data = cons.df[cons.df$Type %in% "Constraint", ], mapping = aes(x = Time, y = Temp, group = Num), colour = "black", linewidth = 1, fill = "transparent") +
    theme(panel.background = element_rect(fill = "transparent", color = "transparent", linewidth = 1),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
          axis.ticks = element_line(colour = "black", linewidth = 0.5),
          axis.ticks.length = unit(-0.2,"cm"),
          panel.grid.major = element_line(colour = "grey70", linewidth = 0.5, linetype = 'dashed'),
          plot.margin = grid::unit(c(0,0,0,0), "mm"))
  
  ## SAVE PLOT
  ggsave(paste(Name,version, plot_type,"tT.pdf", sep = "."), plot = tT.Plot.comb, width = 6.5, height = 6)
  
}

#### PLOT INDIVIDUAL PATH PREDICTIONS AS HISTOGRAMS ----

### create headers dataframe of bin predictions for each path ----
if(plot_hist == "yes"){
  preds.df.header <- cbind("Path", "v1")
  for (i in 1:num_dates){
    preds.df.header <- cbind(preds.df.header, paste("Date", i, sep = ".")) # save preds.df column names for later use
  }
  preds.df <- time.rows[ , 1:(num_dates + 2)]
  ## convert dates to numeric class
  for(i in 3:(num_dates+2)){
    preds.df[ , i] <- as.numeric(preds.df[ , i])
  }
  colnames(preds.df) <- preds.df.header
  preds.df$v1 <- time.rows$v1 # add HeFTy v1 designations
  preds.df <- add_column(preds.df, v2 = temp.rows$v2, .after = "v1") # add HeFTy v2 designations
  preds.df <- add_column(preds.df, Com.GOF = temp.rows$X2, .after = "v2") # add HeFty v2 combined GOF
  preds.df <- add_column(preds.df, Min.GOF = time.rows$minGOF, .after = "Com.GOF") # add minimum GOF for an individual date
  
  disc.levels <- c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4", "0.4-0.5",
                   "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1") # list of full discretized GOF from 0 to 1 for plotting "continuous" plots
  long.pred <- preds.df %>% pivot_longer(!c(Path, v1, v2, Com.GOF, Min.GOF)) %>%
    mutate(Date = 0) %>%
    mutate(Unc = 0) %>%
    mutate(Label = "0") %>%
    mutate(Com.GOF.Discr = cut(as.numeric(Com.GOF), breaks = seq(0, 1, 0.1), labels = disc.levels, right = FALSE)) %>%
    mutate(Min.GOF.Discr = cut(as.numeric(Min.GOF), breaks = seq(0, 1, 0.1), labels = disc.levels, right = FALSE))
  
  for(i in 1:nrow(data_file)){
    long.pred <- long.pred %>%
      mutate(Date = case_when(name == data_file$Date.ID[i] ~ data_file$Date[i],
                              .default = Date)) %>%
      mutate(Unc = case_when(name == data_file$Date.ID[i] ~ data_file$Unc[i],
                             .default = Unc)) %>%
      mutate(Label = case_when(name == data_file$Date.ID[i] ~ paste(data_file$Type[i], round(data_file$Date[i], 0), "±", data_file$Unc[i], sep = " "),
                               .default = Label))
  }
  
  ## Create n = text
  grob <- grid::grobTree(grid::textGrob(paste("n =", num_path, sep = " "), x = 0.7,  y = 0.95, hjust=0,
                                        gp = grid::gpar(col="black", fontsize = 7, fontface = "italic")))
  
  ## PLOT SIMPLE HISTOGRAMS OF TOTAL COUNTS
  pred.plot <- ggplot(long.pred) +
    geom_histogram(aes(x = value), bins = 50, fill = "gray", color = "gray", alpha = 0.9) +
    geom_vline(aes(xintercept = Date), color = "black", linewidth = 0.5) + # add vertical line for observed date
    geom_vline(aes(xintercept = Date + Unc), linetype = "dashed", color = "black", linewidth = 0.25) +
    geom_vline(aes(xintercept = Date - Unc), linetype = "dashed", color = "black", linewidth = 0.25) + # add vertical lines for observed date uncertainty
    labs(x = "Predicted Date (Ma)", y = "Count", title = paste(Name, version, "Date Predictions", sep = " ")) +
    annotation_custom(grob) + 
    theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
          axis.ticks = element_line(colour = "black", linewidth = 0.5),
          axis.ticks.length = unit(0.1,"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=15),
          legend.position = 'bottom') +
    facet_wrap(~Label, scales = "free")
  
  ## save plot
  ggsave(paste(Name, version, "simple.histogram","pdf", sep = "."), plot = pred.plot, width = 6.5, height = 6)
  
  ## Plot all date predictions stacked by default path designation
  if(plot_type == "HeFTy v1"){
    pred.plot.stack.1 <- long.pred %>%
      mutate(v1 = factor(v1, levels = c("Rejected", "Acceptable", "Good"))) %>%
      ggplot() +
      geom_histogram(aes(x = value, fill = v1, color = after_scale(fill)), position = "stack", bins = 50, alpha = 0.9, show.legend = TRUE) +
      geom_vline(aes(xintercept = Date), color = "black", linewidth = 0.5) + # add vertical line for observed date
      geom_vline(aes(xintercept = Date + Unc), linetype = "dashed", color = "black", linewidth = 0.25) +
      geom_vline(aes(xintercept = Date - Unc), linetype = "dashed", color = "black", linewidth = 0.25) + # add vertical lines for observed date uncertainty
      scale_fill_manual(paste(plot_type,"Path Fit", sep = "\n"), values = hefty.cols, drop = FALSE) +
      labs(x = "Predicted Date (Ma)", y = "Count", title = paste(Name, version, "Date Predictions", sep = " ")) +
      annotation_custom(grob) +
      theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
            panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
            axis.ticks = element_line(colour = "black", linewidth = 0.5),
            axis.ticks.length = unit(0.1,"cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size=15),
            legend.position = 'right') +
      facet_wrap(~Label, scales = "free")
    
    ## save plot
    ggsave(paste(Name, version, plot_type, "discrete.histogram","pdf", sep = "."), plot = pred.plot.stack.1, width = 6.5, height = 6)
    
  } else if(plot_type == "HeFTy v2"){
    pred.plot.stack.2 <- long.pred %>%
      mutate(v2 = factor(v2, levels = c("Rejected", "Acceptable", "Good")))  %>%
      ggplot() +
      geom_histogram(aes(x = value, fill = v2, color = after_scale(fill)), position = "stack", bins = 50, alpha = 0.9, show.legend = TRUE) +
      geom_vline(aes(xintercept = Date), color = "black", linewidth = 0.5) + # add vertical line for observed date
      geom_vline(aes(xintercept = Date + Unc), linetype = "dashed", color = "black", linewidth = 0.25) +
      geom_vline(aes(xintercept = Date - Unc), linetype = "dashed", color = "black", linewidth = 0.25) + # add vertical lines for observed date uncertainty
      scale_fill_manual(paste(plot_type,"Path Fit", sep = "\n"), values = hefty.cols, drop = FALSE) +
      labs(x = "Predicted Date (Ma)", y = "Count", title = paste(Name, version, "Date Predictions", sep = " ")) +
      annotation_custom(grob) +
      theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
            panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
            axis.ticks = element_line(colour = "black", linewidth = 0.5),
            axis.ticks.length = unit(0.1,"cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size=15),
            legend.position = 'right') +
      facet_wrap(~Label, scales = "free")
    
    ## save plot
    ggsave(paste(Name, version, plot_type, "discrete.histogram","pdf", sep = "."), plot = pred.plot.stack.2, width = 6.5, height = 6)
    
  } else if(plot_type == "Min.GOF"){
    pred.plot.stack.min <- ggplot(long.pred) +
      geom_histogram(aes(x = value, fill = Min.GOF.Discr, color = after_scale(fill)), position = "stack", bins = 50, alpha = 0.9, show.legend = TRUE) + # show.legend = TRUE necessary to show full legend
      geom_vline(aes(xintercept = Date), color = "black", linewidth = 0.5) + # add vertical line for observed date
      geom_vline(aes(xintercept = Date + Unc), linetype = "dashed", color = "black", linewidth = 0.25) +
      geom_vline(aes(xintercept = Date - Unc), linetype = "dashed", color = "black", linewidth = 0.25) + # add vertical lines for observed date uncertainty
      scale_fill_viridis_d(name = paste("Minimum", "GOF", sep = "\n"), limits = disc.levels, drop = FALSE, option = "A") +
      labs(x = "Predicted Date (Ma)", y = "Count", title = paste(Name, version, "Date Predictions", sep = " ")) +
      annotation_custom(grob) +
      theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
            panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
            axis.ticks = element_line(colour = "black", linewidth = 0.5),
            axis.ticks.length = unit(0.1,"cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size=15),
            legend.position = 'right') +
      guides(fill = guide_legend(reverse = TRUE)) +
      facet_wrap(~Label, scales = "free")
    
    ## save plot
    ggsave(paste(Name, version, plot_type, "histogram","pdf", sep = "."), plot = pred.plot.stack.min, width = 6.5, height = 6)
    
  } else if(plot_type == "Com.GOF"){
    pred.plot.stack.com <- ggplot(long.pred) +
      geom_histogram(aes(x = value, fill = Com.GOF.Discr, color = after_scale(fill)), position = "stack", bins = 50, alpha = 0.9, show.legend = TRUE) +
      geom_vline(aes(xintercept = Date), color = "black", linewidth = 0.5) + # add vertical line for observed date
      geom_vline(aes(xintercept = Date + Unc), linetype = "dashed", color = "black", linewidth = 0.25) +
      geom_vline(aes(xintercept = Date - Unc), linetype = "dashed", color = "black", linewidth = 0.25) + # add vertical lines for observed date uncertainty
      scale_fill_viridis_d(name = paste("Combined", "GOF", sep = "\n"), limits = disc.levels, drop = FALSE, option = "D") +
      labs(x = "Predicted Date (Ma)", y = "Count", title = paste(Name, version, "Date Predictions", sep = " ")) +
      annotation_custom(grob) +
      theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
            panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
            axis.ticks = element_line(colour = "black", linewidth = 0.5),
            axis.ticks.length = unit(0.1,"cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size=15),
            legend.position = 'right') +
      guides(fill = guide_legend(reverse = TRUE)) +
      facet_wrap(~Label, scales = "free")
    
    ## save plot
    ggsave(paste(Name, version, plot_type, "histogram","pdf", sep = "."), plot = pred.plot.stack.com, width = 6.5, height = 6)
  }
}