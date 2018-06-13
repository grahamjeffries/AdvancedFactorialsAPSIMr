# This final step reads in the simulated data and produces maps for Optimum N rate
# and Net retrun by soil type

# Load libraries ##################################################################

library(tidyverse)
library(lubridate)
library(rgdal)

# Set theme for plotting 
theme_set(theme_minimal() + theme(plot.background = element_blank(),
                                  axis.text.x = element_text(angle = 90),
                                  axis.ticks = element_line(colour = "black"),
                                  panel.grid = element_blank(),
                                  legend.position = "bottom"))

# Read simulated data 
factorials <- readRDS("factorials.rds") %>%
  separate(sim, c("scenarioID1","soilName","MUSYM")) %>%
  mutate(year = year(Date))

# Pre-processing ##################################################################

# Vizualize yield response to N
factorials %>%
  group_by(myPD,myHybrid,myNrate,soilName) %>%
  summarise(y = mean(corn_buac),
            ysd = sd(corn_buac)) %>%
  ggplot(aes(x = myNrate/1.12, y = y))+
  geom_ribbon(aes(ymin = y - ysd, ymax = y + ysd, fill= soilName), alpha = 0.3) + 
  geom_point(aes(colour = soilName, shape = soilName)) +
  geom_line(aes(colour = soilName)) +
  facet_grid(myPD~myHybrid) +
  labs(y = "Net return ($/ac)",
       x = "Nrate (lb/ac)")

# Vizualize Net return response to N
factorials %>%
  group_by(myPD,myHybrid,myNrate,soilName) %>%
  summarise(y = mean(NetReturn),
            ysd = sd(NetReturn)) %>%
  ggplot(aes(x = myNrate/1.12, y = y))+
  geom_ribbon(aes(ymin = y - ysd, ymax = y + ysd, fill= soilName), alpha = 0.3) + 
  geom_point(aes(colour = soilName, shape = soilName)) +
  geom_line(aes(colour = soilName)) +
  facet_grid(myPD~myHybrid) +
  labs(y = "Net return ($/ac)",
       x = "Nrate (lb/ac)")

# Get N rate at which Net return is maximized by fitting a smoothing spline
# This is done for every combination of factors and yield

factorials %>%
  arrange(myPD,myHybrid,MUSYM,year) %>%
  group_by(myPD,myHybrid,MUSYM,year) %>%
  do(fit = data.frame(spline(.$myNrate,
                             .$NetReturn,
                             xout = 0:280))) %>%
  group_by(myPD,myHybrid,MUSYM,year) %>%
  unnest() %>%
  mutate(maxy = max(y)) %>%
  filter(y == maxy) %>%
  group_by(myPD,myHybrid,MUSYM) %>%
  summarise(optN = mean(x),
            optN_sd = sd(x),
            NetRet = mean(y),
            NetRet_sd = sd(y))-> optN

# Create fina figures #############################################################

# Read shapefile 
shapefile <- readOGR("soils/ISU Nashua Farm_shapefile", "x")

# Convert to dataframe and join with simulated data 
shapefile_df <- fortify(shapefile,  region = "MUSYM") %>%
  right_join(optN, by = c("id" = "MUSYM"))

# Plot of Mean Optimum N rate 
shapefile_df %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill =  optN/1.12),
               alpha = 0.5, color = "black")  +
  scale_fill_gradientn(colours = c("blue","red")) +
  scale_y_continuous(breaks = c(42.930,42.932,42.934)) + 
  facet_grid(myPD~myHybrid) +
  labs(y = "", x = "", fill = "Optimum rate (lb N/ac)") -> fig1a

# Plot of Optimum N rate CV
shapefile_df %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill =  100*optN_sd/optN/1.12),
               alpha = 0.5, color = "black")  +
  scale_fill_gradientn(colours = c("blue","red")) +
  scale_y_continuous(breaks = c(42.930,42.932,42.934)) + 
  facet_grid(myPD~myHybrid) +
  labs(y = "", x = "", fill = "CV (%)") -> fig1b

# Save fig 1
fig1 <- ggarrange(fig1a,fig1b)
annotate_figure(fig1,
                top = text_grob("N rate with highest Net return", color = "black", face = "bold", size = 14))
ggsave("fig1.jpeg",width = 10,height = 8)

# Plot of Mean return to N at optimum 
shapefile_df %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill =  NetRet/1.12),
               alpha = 0.5, color = "black")  +
  scale_fill_gradientn(colours = c("blue","red")) +
  scale_y_continuous(breaks = c(42.930,42.932,42.934)) + 
  facet_grid(myPD~myHybrid) +
  labs(y = "", x = "", fill = "Net Return  ($/ac)") -> fig2a

# Plot of Net return to N at optimum CV
shapefile_df %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group, fill =  100*NetRet_sd/NetRet/1.12),
               alpha = 0.5, color = "black")  +
  scale_fill_gradientn(colours = c("blue","red")) +
  scale_y_continuous(breaks = c(42.930,42.932,42.934)) + 
  facet_grid(myPD~myHybrid) +
  labs(y = "", x = "", fill = "CV (%)") -> fig2b

# Save fig 2
fig2 <- ggarrange(fig2a,fig2b)
annotate_figure(fig2,
                top = text_grob("Net return to N at optimum rate", color = "black", face = "bold", size = 14))

ggsave("fig2.jpeg",width = 10,height = 8)
