save_bsm_seg_df = "bsm_seg_df"
bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(1,"_bsm_seg_df.rds",sep = "")))

(bsm_seg_df %>% filter(bs_npoints=='6_set') %>% NROW())
NROW(bsm_seg_df)

new_df <- data.frame("sealID" = rep(0, (length(SealIDS))), "6_set" = 0, "total" = 0)

for (i in 1:length(SealIDS)) {
  
  sealID = SealIDS[i]
  print(sealID)
  save_bsm_seg_df = "bsm_seg_df"
  bsm_seg_df <- read_rds(file.path(save_bsm_seg_df,paste(sealID,"_bsm_seg_df.rds",sep = "")))
  
  new_df$sealID[i] <- sealID
  new_df$X6_set[i] <- (bsm_seg_df %>% filter(bs_npoints=='6_set') %>% NROW())
  new_df$total[i] <- NROW(bsm_seg_df)
}
saveRDS(new_df,file.path(save_bsm_seg_df,"bs_points.rds"),compress = TRUE)
saveRDS(new_df,file.path(save_bsm_seg_df,"bs_points.rds"),compress = TRUE)
write.csv(new_df,file.path(save_bsm_seg_df,"bs_points.csv"))

sum(new_df$X6_set)/sum(new_df$total)*100

