#! /usr/bin/Rscript

args = commandArgs(trailingOnly=TRUE)

if (length(args) != 5) {
  stop("Please provide the 5 arguments for the script", 
       call.=FALSE)
}


#' @title Feature significance analysis.
#'
#' @description Script for feature significance analysis. 
#' It must derive from the giExtract python package.
#'
#' @param context Path to csv file with meta information. 
#' This table must have only 3 columns Name, slide and Group
#' 
#' @param cnnfeature Path to csv file with cnn features to analyse. 
#' This table must be an output of giExtract. 
#' It should contain a column with matching name as the first column in -c
#' 
#' @param mergeColumn The column name of the matching column in -c and -f above to merge the table. 
#' It is ideally to change the context name to Name as the giExtract will use this
#' 
#' @param slideColumn The column name of the slide name in meta file define by -c
#' 
#' @param groupColumm The column name of the Group name in meta file define by -c
#'
#' @return Saves files to the current working directory.
#'
#' @keywords plotting
#'
#'
gifeature <- function(context, cnnfeature, mergeColumn="Name", 
                      slideColumn="slide", groupColumn="Group"){
  # Path to context file
  # Path to cnnfeature file, must be from giExtract
  # Column name with matching IDs to merge the two tables
  # mergeColumn must be the same name as in the two files
  
  obj <- read.csv(context)
  nncol <- ncol(obj)
  
  if(nncol != 3)(
    stop("ERROR: The context file must have only three columns, tiles, slide, group")
  )
  
  # Read the features
  predict <- read.csv(cnnfeature)
  
  # Merge the data
  final <- merge(obj, predict, by = mergeColumn)
  
  # Fix the column name for easy processing
  names(final)[which( colnames(final)==groupColumn)] <- "Group"
  names(final)[which( colnames(final)==slideColumn)] <- "slide"
  
  # Filter missing Groups
  final <- final[!final$Group %in% "Unknown", ]
  final <- final[!is.na(final$Group), ]
  
  # Create a hold
  fina2 <- final[3:ncol(predict)]
  
  get <- data.frame("id"=names(fina2[-1]),
                    "colsum"=colSums(fina2[-1]))
  
  # Remove features with low values and zeros
  get <- get[get$colsum <= 0.001, ]
  final <- final[!names(final) %in% get$id ]
  
  
  # Prepare the feature names to search
  y_class <- sapply(fina2, class)
  featu <- y_class[y_class %in%  "numeric"]
  featu <- names(featu)
  
  
  # Prepare the Discovery and test data
  samp <- unique(final$slide)
  if(length(samp) < 2)(
    stop("ERROR: The feature file seems to have only one data point.")
  )
  
  
  set.seed(2389)
  disco <- sample(samp, 60,
                  replace = FALSE,
                  prob = NULL)
  
  dis_dat <- final[final$slide %in% disco, ]
  dis_dat <- dis_dat[3:ncol(dis_dat)]
  test_dat <- final[!final$slide %in% disco, ]
  test_dat <- test_dat[3:ncol(test_dat)]
  
  # Perform feature identification in the discovery data
  feat_len <- length(featu)
  uniGroup <- unique(dis_dat$Group)
  
  ### Function for medina difference
  # Function to calculate median difference
  # Function to calculate median difference for multiple columns
  meandiff <- function(formula, data) {
    # Extract variable and groups from the formula
    vars <- all.vars(formula)
    group_var <- vars[length(vars)]
    
    # Extract columns to calculate median difference
    columns <- names(data)[!names(data) %in% group_var]
    
    # Use tapply to calculate medians for each group and each column
    medians <- lapply(data[columns], function(col) {
      tapply(col, data[[group_var]], median)
    })
    
    # Calculate the median difference for each column
    median_differences <- lapply(medians, function(med) diff(med))
    
    # Return the result
    return(median_differences)
  }
  
  #########
  ## Do feature selection per group
  library(tidyverse)
  
  dis_dat2 <- dis_dat
  test_dat2 <- test_dat
  
  # On the discovery data
  ffout_Ttest <- lapply(uniGroup, function(gg){
    dis_dat2$Group <- ifelse(dis_dat$Group %in% gg, "a", "z")
    
    res <- dis_dat2 %>%
      select_if(is.numeric) %>%
      map_df(~ broom::tidy(wilcox.test(. ~ dis_dat2$Group)), .id = 'var')
    
    res <- as.data.frame(res)
    
    # Run median difference
    res2 <- as.data.frame(meandiff(~ Group, data = dis_dat2))
    res2 <- as.data.frame(t(res2))
    
    res$estimate <- res2$z
    res$Group <- gg
    
    return(res)
  })
  
  # Merge it for use
  ffout_Ttest <- do.call(rbind, ffout_Ttest)
  ffout_Ttest$Type <- "Discovery"
  
  
  ## Apply the same function on the test data
  # On the test data
  tescase_Ttest <- lapply(uniGroup, function(gg){
    test_dat2$Group <- ifelse(test_dat$Group %in% gg, "a", "z")
    
    res <- test_dat2 %>%
      select_if(is.numeric) %>%
      map_df(~ broom::tidy(wilcox.test(. ~ test_dat2$Group)), .id = 'var')
    
    res <- as.data.frame(res)
    
    # Run median difference
    res2 <- as.data.frame(meandiff(~ Group, data = test_dat2))
    res2 <- as.data.frame(t(res2))
    
    res$estimate <- res2$z
    res$Group <- gg
    
    return(res)
  })
  
  # Bind the the test data run
  tescase_Ttest <- do.call(rbind, tescase_Ttest)
  tescase_Ttest$Type <- "Test"
  
  # Bind the two datasets for easy processing Add the file here to Rdata
  ffout2 <- rbind(ffout_Ttest, tescase_Ttest)
  # write.csv(ffout2, file = "ttest_select.csv", row.names = F)
  
  # Extract features significant at p.value <= 0.01
  ffout2_sig <- ffout2[ffout2$p.value <= 0.01, ]
  ffout2_sig$keep <- paste(ffout2_sig$var, ffout2_sig$Group, sep = "_")
  dtyu <- as.data.frame(table(ffout2_sig$keep))
  
  # Filter to features only occurring in one group given sig p-value
  dtyu <- dtyu[dtyu$Freq == 2, ]
  ffout2_sig <- ffout2_sig[ffout2_sig$keep %in% dtyu$Var1, ]
  
  iddd <- unique(ffout2_sig$keep)
  outpp <- vector("character", length(iddd))
  outpp2 <- vector("character", length(iddd))
  
  
  # Check that the two matching significant rows is same discovery and test
  for(i in 1:length(iddd)){
    dad <- ffout2_sig[ffout2_sig$keep %in% iddd[i], ][["Type"]]
    
    outpp[i] <- ifelse(dad[1] == dad[2], "remove", "keep")
  }
  # Add to the big data
  ffout2_sig$filter <- outpp
  
  
  # Check that the sign of test statistics is matching same discovery and test
  for(i in 1:length(iddd)){
    dad <- ffout2_sig[ffout2_sig$keep %in% iddd[i], ][["estimate"]]
    
    outpp2[i] <- ifelse(sign(dad[1]) == sign(dad[2]), "keep", "remove")
  }
  # Add to the big data
  ffout2_sig$filter2 <- outpp2
  
  
  # Now filter to make sure the rule above is meet
  ffout2_sig <- ffout2_sig[ffout2_sig$filter2 %in% "keep", ]
  
  # Write the data to file +++++ Add the file here to Rdata
  # write.csv(ffout2_sig, file = "ffout2_sig_second_filter.csv", row.names = F)
  
  # Reduce to Discovery for the next analysis
  ffout2_sig <- ffout2_sig[ffout2_sig$Type %in% "Discovery", ]
  fiil <- as.data.frame(table(ffout2_sig$var))
  
  # Filter to those occurring once in the data
  fiil <- fiil[fiil$Freq == 1, ]
  
  ffout2_sig <- ffout2_sig[ffout2_sig$var %in% fiil$Var1, ]
  
  # Final selection function
  # Here we selected the top 5
  final_sec <- lapply(unique(ffout2_sig$Group), function(kk){
    gdat <- ffout2_sig[ffout2_sig$Group %in% kk, ]
    
    neg <- gdat[gdat$estimate < 0, ]
    neg <- neg[order(neg$p.value), ][1:5, ]
    neg <- neg[c("var", "Group")]
    neg$Side <- "down"
    
    pos <- gdat[gdat$estimate > 0, ]
    pos <- pos[order(pos$p.value), ][1:5, ]
    
    pos <- pos[c("var", "Group")]
    pos$Side <- "up"
    
    return(rbind(pos, neg))
  })
  
  final_sec <- do.call(rbind, final_sec)
  final_sec$NatCom <- paste(final_sec$Group, final_sec$Side, sep = "_")
  
  # Save this file and use to build a final Python system 
  write.csv(final_sec, file = "NatCom_path.csv", row.names = F)
  
  # Get your associated data and file
  final_data <- final[c("Name", "slide", "Group", final_sec$var)]
  fdisc <- final_data[final_data$slide %in% disco, ]
  fdisc$data <- "Discovery"
  ftest <- final_data[!final_data$slide %in% disco, ]
  ftest$data <- "Test"
  final_data <- rbind(fdisc, ftest)
  
  final_data_long <-  reshape2::melt(final_data,
                                     id.vars=c("Name", "slide", "Group", "data"),
                                     variable.name="NatCom",
                                     value.name="measurment" )
  
  write.csv(final_data, file = "selected_feature_Values.csv", row.names = T)
  
  ## This is the Nature-Communication Function
  ## Also Implemented in Python
  NatCom <- function(sig, data){
    
    output <- data[!names(data) %in% sig$var]
    
    gx <- unique(sig$NatCom)
    
    i <- gx[1]
    for(i in gx){
      gf <- sig[sig$NatCom %in% i,][["var"]]
      df <- data[gf]
      output[i] <- rowSums(df)
    }
    return(output)
  }
  
  # Do the Sum
  final_sum <- NatCom(sig = final_sec, data = final_data)
  f_disc <- final_sum[final_sum$slide %in% disco, ]
  f_disc$data <- "Discovery"
  f_test <- final_sum[!final_sum$slide %in% disco, ]
  f_test$data <- "Test"
  final_data <- rbind(f_disc, f_test)
  
  final_sum <- rbind(f_disc, f_test)
  write.csv(final_sum, file = "quntifcation.csv", row.names = T)

  print("Feature selection is complete and saved to current working directory below.")
  getwd()
  print("You should see three csv files: NatCom_path.csv, selected_feature_Values and  quntifcation")
}


## Run the code 
gifeature(context=args[1], cnnfeature = args[2],
          mergeColumn = args[3], slideColumn = args[4], 
          groupColumn = args[5])

