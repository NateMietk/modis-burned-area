# peak fsr function
library(tidyverse)
library(strucchange)
library(foreach)
library(doParallel)

peak_fsr <- function(dd, res){

  if(nrow(dd)>20){
    bps <- breakpoints(dd$pixels ~ 1)
    
    if(length(na.omit(bps$breakpoints)) >0){
      df <- data.frame(event_day = 0, cum_pixels = 0)
      for(i in 1:length(bps$breakpoints)){
        df[i+1,1] <- dd$event_day[bps$breakpoints[i]] 
        df[i+1,2] <- dd$cum_pixels[bps$breakpoints[i]]
      }
      df[length(bps$breakpoints)+2,1]<- max(dd$event_day)
      df[length(bps$breakpoints)+2,2]<- max(dd$cum_pixels)
      
      df<- df %>%
        mutate(prior_pixels = lag(cum_pixels),
               prior_days = lag(event_day),
               pixel_gain = cum_pixels - prior_pixels,
               day_gain = event_day - prior_days,
               fsr = pixel_gain/day_gain)
      
      p_fsr <- max(df$fsr, na.rm=T)
      pix <- df[df$fsr == p_fsr, "pixel_gain"][2]
      days <- df[df$fsr == p_fsr, "day_gain"][2]
      
      maxrow <- which(df$fsr == max(df$fsr, na.rm=T))
      
      sp <- df[maxrow-1, "event_day"]
      if(sp==0) {sp <- sp+1}
      ep <- df[maxrow,"event_day" ]
      
      if(res == "pix") return(pix)
      if(res == "days") return(days)
      if(res == "fsr") return(p_fsr)
      if(res == "start_pgp") return(sp)
      if(res == "end_pgp") return(ep)
  }
  else{
    return(NA)   
  }
    }else{
     return(NA)
  }
}

daily <- read_csv("data/daily_06_10.csv") %>%
  filter(duration > 20)

cc<- detectCores()-1
registerDoParallel(cc)

l_id <- length(unique(daily$id))
ids <- unique(daily$id)
ddd <- foreach(i = 1:l_id, .combine = "rbind") %dopar%{
  dd <- filter(daily, id == ids[i])

  fsr_df <- data.frame(id = ids[i], 
                       pgp_fsr=peak_fsr(dd, "fsr"),
                       pgp_duration = peak_fsr(dd, "days"),
                       pgp_ba_pix = peak_fsr(dd, "pix"),
                       pgp_start = peak_fsr(dd, "start_pgp"),
                       pgp_end = peak_fsr(dd, "end_pgp")
                       )
  
  system(paste("echo",round(i/l_id,2)*100, "%", ids[i]))
  return(fsr_df)
}


ddd %>% 
  na.omit() %>%
  write_csv("data/peak_growth_periods.csv")
