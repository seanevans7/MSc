########### Correcting/Adding Mdepth_hunting ########### 


# SealIDS = c(1,2,4,5,seq(13,25,1),31,47,seq(49,53,1),57,58,seq(62,70,1),109,110,112,113)
# 
# save_bsm_seg_df = "bsm_seg_df"
Add_Mdepth_hunting <- function(divessummary_df_,bsm_) {

  # bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealIDS,"_bsm_seg_df.rds",sep = "")))
  durr <- bsm_ %>% 
    group_by(num) %>%
    filter(foraging == "hunting") %>%
    summarize("dur"=max(dur))

  jack <- data_frame(j1 = seq(1,NROW(durr),1),j2 = 0)
  jack$j1 <- NaN
  jack$j2 <- NaN

  bsm_ <- bsm_ %>% group_by(num)
  for (i in 1:NROW(durr)){
    print(i)
    j1 <- as.integer(durr$num[i])
    j2 <- last(bsm_[which(bsm_$num == durr$num[i] & bsm_$dur == durr$dur[i] & bsm_$foraging == 'hunting'),"mean_depth"]) %>% as.numeric() 
    jack$j1[i] <- j1
    jack$j2[i] <- j2
  }
  colnames(jack) <- c('num','Mdepth_hunting')
  
  divessummary_df_$Mdepth_hunting <- NULL
  divessummary_df_<-divessummary_df_ %>% 
    left_join(jack,by='num')
  
  divessummary_df_
}
# bsm_seg_df %>% 
#   group_by(num) %>%
#   filter(foraging == "hunting") %>%
#   summarize("hunting_time" = sum(dur),"Mvdist_err_hunting"= mean(vdist)/2, "Mdepth_hunting"=mean(mean_depth))


# # Try with divessummary ---------------------------------------------------
# 
# sealID = SealIDS[1]
# save_divessummary = paste("Plots & Dive Tables/Seal",sealID, sep = "")
# divessummary <- read.csv(file.path(save_divessummary,"divessummary.csv"),sep=',')
# divessummary$Mdepth_hunting <- NULL
# divessummary<-divessummary %>% 
#   left_join(jack,by='num')
