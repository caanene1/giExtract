library(purrr)

# Prepare the context, Before python run
files <- list.files("/Users/chineduanene/Desktop/nat_com/CNESCC_JPF_sf8/cubes",
                    pattern = ".jpg")


obj <- data.frame("Name" = files)
obj$slide <- substr(obj$Name, 1, 9)
obj$Group <- substr(obj$Name, 11, 15)
unique(obj$Group)

# repeat as per the issues
obj$Group <- gsub("B STE", "STEM", obj$Group)
obj <- obj[obj$Group %in% c("DIFF", "MET", "IMM", "STEM"),]
write.csv(obj, file = "context-NatCom.csv", row.names = F)



# Run the python code and come back


### Begin downstream analysis #############
### Read files
# Read in the extracted feature files

colls <- c("DIFF_up", "IMM_up",
           "MET_up",  "STEM_up")

# Plot the Boxplot
library(ggpubr)
ggboxplot(final_sum, x = "Group", y = colls,
          color = "data",
          #fill = "Group",
          xlab = "Group",
          ylab = "Feature Level",
          combine = F,
          palette = c("black", "grey"),
          notch = TRUE,
          ggtheme = theme_minimal() )


########### Now extract the representative figures
########### Only considering the discovery data makes sense
loc <- "/Users/chineduanene/Documents/OneDrive/NatCom/Data/CNESCC_JPF_sf8-PWF/cubes"
base <- "/Users/chineduanene/Desktop/Images/"
oldSe <- getwd()
setwd(loc)
savdat <- final_sum[final_sum$data %in% "Discovery", ]
for(i in colls){
  sav1 <- savdat[order(savdat[[i]]),][1:50,]
  sav2 <- savdat[rev(order(savdat[[i]])),][1:50,]
  
  sav <- rbind(sav1, sav2)[c("Name", i)]
  outfolder <- paste(base, i, sep = "")
  dir.create(outfolder)
  file.copy(sav$Name, outfolder)
  write.csv(sav, file = paste(outfolder, "/meta.csv", sep=""), row.names = F)
}
setwd(oldSe)

# Do Statistics on the sum for significance
# May need to come back and show per data (Discovery or Test)
# Create the target
# If bring in external quantification data use below
final_sum <- check
final_selb <- check[check$data %in% "Discovery", ]
uniGroup <- unique(final_selb$Group)

### Get the discovery and the test split
disco <- final_sum[final_sum$data %in% "Discovery", ]
testco <- final_sum[final_sum$data %in% "Test", ]

## Get columns for the analysis
disco <- disco[c("Group", "MET_up", "IMM_up", "STEM_up", "DIFF_up", "data")]
names(disco) <- c("Group", "MET", "IMM", "STEM", "DIFF", "data")

## Get columns for the analysis
testco <- testco[c("Group", "MET_up", "IMM_up", "STEM_up", "DIFF_up", "data")]
names(testco) <- c("Group", "MET", "IMM", "STEM", "DIFF", "data")

statsout <- lapply(list(disco, testco), function(xx){
  quant <- xx
  quant2 <- quant[-6]
  dataid <- unique(quant$data)
  
  out_STAT <- lapply(uniGroup, function(gg){
    quant2$Group <- ifelse(quant$Group %in% gg, "a", "z")
    
    res <- quant2 %>%
      select_if(is.numeric) %>%
      map_df(~ broom::tidy(wilcox.test(. ~ quant2$Group)), .id = 'var')
    
    res <- as.data.frame(res)
    
    res$Group <- gg
    res <- res[res$var == res$Group, ]
    res <- res[-1]
    
    return(res)
  })
  out_STAT <- do.call(rbind, out_STAT)
  out_STAT$split <- dataid
  
  return(out_STAT)
})
## Bind the data
statsout <- do.call(rbind, statsout)

###
write.csv(statsout, file = "statistics.csv", row.names = T)
# Hand filter externally

getwd()
