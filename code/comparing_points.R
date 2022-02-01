# comparing body labels across scorers
library(tidyverse)
library("stringr")
library(raster)


setwd("~/deeplabcut")

format_df<- function(df){
  
  #df<-read.csv("CollectedData_Vidya.csv")  
  
  headers<- df[1:3,] 
  df2<- setNames(df, (paste(headers[1,], headers[2,], headers[3,], sep = "_")))
  df<- df2[-c(1:3),]
  
  frame_num <- str_extract(df$individuals_bodyparts_coords, pattern = "img[0-9]+")
  
  df$frame = frame_num
  df<- df[,-c(1)]
  
  df <- df %>%
    dplyr::select(frame, everything())
  
  width<- ncol(df)
  df_long<- pivot_longer(df, col = 2:all_of(width), names_to = "body_part", values_to = "coordinate")
  
  df_long$X_Y<- ifelse(grepl("_x", df_long$body_part, fixed = TRUE) == TRUE, "X", "Y")
  
  body_label <-
    ifelse(
      grepl("_x", df_long$body_part, fixed = TRUE), 
      strsplit(df_long$body_part, split='_x', fixed = TRUE),
      strsplit(df_long$body_part, split='_y', fixed = TRUE)
    )
  body_part2<- unlist(body_label)
  
  df_long$body_part<-body_part2
  
  df_wide<- pivot_wider(df_long, names_from = X_Y, values_from = coordinate)
  df_wide$X<- as.numeric(df_wide$X)
  df_wide$Y<- as.numeric(df_wide$Y)
  
  final<-df_wide[!(is.na(df_wide$X) | df_wide$X==""), ]
  
  new_df <<- final
  
}


#plot some stuff

# list of frames for later
#frame_names <- unique(new_df$frame)


plot_labels<-function(img, frame_width, frame_height){
  
  #baby df with just the first frame labels
  frame1_p<- paige_df%>% filter(frame == img)%>%mutate(scorer = "paige")
  
  frame1_t<- taran_df%>%filter(frame == img)%>%mutate(scorer = "taran")
  
  frame1_v<- vidya_df%>%filter(frame == img)%>%mutate(scorer = "vidya")
  
  p_t<-full_join(frame1_p, frame1_t, by.x = "frame", by.y = "body_part")
  p_t_v<-full_join(p_t, frame1_v, by.x = "frame", by.y = "body_part")
  #plot points
  
  
  
  #need to scale picture/crop
  
  
  frame_file<-paste(img, ".png", sep = "")
  background<-  png::readPNG(frame_file)
  
  
  library(grid)
  library(png)
  
  #z<-ggplot( NULL, aes(x= X, y= Y)) +
  #  ggtitle(img) +
  #  scale_fill_continuous(guide = FALSE) +
  #  annotation_custom(grid::rasterGrob(background, 
  #                               width = unit(1,"npc"), 
  #                               height = unit(1,"npc")), 
  #                    -Inf, Inf, -Inf, Inf) +
  #  scale_y_reverse('Pixel position', expand = c(0,0), limits = c(frame_height,0)) +
  #  scale_x_continuous('Pixel position', limits = c(0,frame_width), expand = c(0,0))+
  #  geom_point(data=frame1_p, color = "red")+
  #  geom_point(data=frame1_t, color = "blue")+
  #  geom_point(data=frame1_v, color = "green")+
  #  annotate(geom= "text", x=80, y=50, label="P = red \n V = green \n T = blue",
  #              color="white", size = 5)
  
  
  z<-p_t_v%>%
    ggplot(aes(x = X, y = Y, label = body_part, color = scorer))+
    ggtitle(img) +
    scale_fill_continuous(guide = FALSE) +
    annotation_custom(grid::rasterGrob(background, 
                                       width = unit(1,"npc"), 
                                       height = unit(1,"npc")), 
                      -Inf, Inf, -Inf, Inf) +
    scale_y_reverse('Pixel position', expand = c(0,0), limits = c(frame_height,0)) +
    scale_x_continuous('Pixel position', limits = c(0,frame_width), expand = c(0,0))+
    geom_point(size =3, alpha = .7)
  #  annotate(geom= "text", x=80, y=50, label="P = red \n V = green \n T = blue",
  #          color="white", size = 5)+
  #geom_text_repel(size = 3, alpha = .8, color = "yellow", box.padding = 2)
  
  
  z
  
  x2<-".png"
  filename2<- paste(img, "_labeled", x2, sep = '')
  ggsave(filename = filename2, 
         z,
         height = 12,
         width = 18,
         dpi = 100, path = "group_labels")
}

#---------------------------------------------
# run

paige<- read.csv("CollectedData_Paige (1).csv")
taran<- read.csv("CollectedData_Taran (1).csv")
vidya<-read.csv("CollectedData_Vidya (1).csv")

#format all three dfs
paige_df<-format_df(paige)
taran_df<-format_df(taran)
vidya_df<- format_df(vidya)

frame_list<- unique(vidya_df$frame)

for (i in frame_list){
  plot_labels(img = i, frame_width = 850, frame_height = 510)
}


