#######################################################################
## FIGURES FOR PAPER
#########################################################################
# update names and hydro categories

seasonal_diurnal_trends = seasonal_diurnal_trends%>%
  mutate(hydro_category = ifelse(hydro_category == "Runoff", "Atmospheric", hydro_category))


seasonal_diurnal_trends = seasonal_diurnal_trends %>% mutate(site_id_display = recode(site_id,
                                                                            "government_E323" = "Government",
                                                                            "LB_upper_E017" = "LB Upper",
                                                                            "LB_lower_E126" = "LB Lower",
                                                                            "mckorkle_E118" = "McKorkle",
                                                                            "chamberlain_E032" = "Chamberlain",
                                                                            "buckhorn_E016" = "Buckhorn", 
                                                                            "BC_E109" = "Big Creek", 
                                                                            "grouse_E062" = "Grouse", 
                                                                            "lake_E035" = "Lake", 
                                                                            "logan_E123" = "Logan", 
                                                                            "smith_E208" = "Smith", 
                                                                            "warren_E162" = "Warren"))




##################################################################
# FIGURE 2: RUNOFF FIGURE FOR METHODS 

snowmelt_methods = Snowmelt_runoff %>% filter(site_id_display =="Grouse" & Water_Year == 2021)%>%
  ggplot(aes(DOY, cumulative_runoff*1000))+
  geom_point(color = "steelblue", size = 3)+
  # geom_vline(xintercept = 178, linetype = "dashed", color = "grey20", lwd = 1)+
  geom_abline(slope = 15, intercept = -1700, color = "black", linetype = "dotdash", lwd = 1) +
  annotate("text", x = 95, y = 50, label = "Snowmelt rate (total runoff at 90%/melt days)", 
           angle = 70, color = "black", size = 3, hjust = 0) +
  
  # Partial vertical dashed line
  annotate("point", x = 178, y = 890, 
            color = "grey20", size = 5) +
  annotate("segment", x = 178, xend = 178, y = 890, yend = 0,linetype = "dotted", lwd = 1, color = "grey20")+
  
  # Partial horizontal dashed line
  # annotate("segment", x = 0, xend = 320, y = 979, yend = 979, 
  #          linetype = "dashed", color = "grey20", lwd = 1) +
  
  # Add text annotations next to the lines
  annotate("text", x = 45, y = 880, label = "Snowmelt timing (90% runoff)", 
           hjust = 0, vjust = -0.5, size = 3) +
  annotate("text", x = 330, y = 985, label = "Total annual snowmelt runoff (mm)", 
           hjust = 1, vjust = -0.5, size = 3) +
  theme_classic()+
  geom_hline(yintercept = 979, linetype = "dashed", color = "grey20", lwd = 1)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  xlab("Day of year")+
  ylab("Cumulative snowmelt runoff (mm)")+
  theme(
    axis.title.x = element_text(size = 9, color = "black"),
    axis.title.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"))

snowmelt_methods
ggsave(plot = snowmelt_methods, filename = "FIGURES/snowmelt_methods.tif", dpi = 500,height = 3.5, width = 6, units = "in" )



######################################################################
## FIGURE 3 
#####################################################################

## all sites
all_sites_boxplots = ggplot(diurnal_metrics, aes(factor(year), diurnal_AmpRatio, fill = factor(year)))+
  geom_boxplot()+
  ylim(0,0.5)+
  xlab("Year")+
  ylab(expression(AR[d]))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11, color = "black"),
    title = element_text(size = 11, color = "black"), 
    strip.text = element_text(size = 11, color = "black"),
    legend.position = "none",
    legend.text = element_text(size = 10, color = "black"))+
  facet_wrap(~site_id_display, ncol = 3)+
  scale_fill_manual(values = c("2021" = "red"))



all_sites_boxplots


ggsave(plot = all_sites_boxplots , filename = "FIGURES/all_sites_boxplots .tif", dpi = 500,height = 11, width = 10, units = "in" )




#######################################
## Figure 4: PL AND AR
#######################################
# the name of the r object "poly_time_series" is an artifact of an initial
# iteration that fit polynomial models. GAM Smoothers are used in the below code,
# but naming retained.

## rename sites

poly_time_series = poly_time_series %>% mutate(site_id_display = recode(site_id,
                                                                        "government_E323" = "Government",
                                                                        "LB_upper_E017" = "LB Upper",
                                                                        "LB_lower_E126" = "LB Lower",
                                                                        "mckorkle_E118" = "McKorkle",
                                                                        "chamberlain_E032" = "Chamberlain",
                                                                        "buckhorn_E016" = "Buckhorn", 
                                                                        "BC_E109" = "Big Creek", 
                                                                        "grouse_E062" = "Grouse", 
                                                                        "lake_E035" = "Lake", 
                                                                        "logan_E123" = "Logan", 
                                                                        "smith_E208" = "Smith", 
                                                                        "warren_E162" = "Warren"))


p_diel = ggplot(poly_time_series, aes(DOY)) +
  
  geom_point(aes(y = diurnal_phase_lag, shape = "Diel phase Lag"), size = .5, color = "black") +
  geom_point(aes(y = diurnal_AmpRatio * 10, shape = "Diel amplitude ratio"),
             size = 0.5, color = "black") +
  
  geom_smooth(aes(y = diurnal_AmpRatio* 10, color = "Diel amplitude ratio"), method = "gam",  se = FALSE, size = 1.5)+
  geom_smooth(aes(y = diurnal_phase_lag, color = "Diel phase Lag"), method = "gam",se = FALSE, size = 1.5)+
  
  facet_wrap(~site_id_display, nrow = 4, ncol = 3) +
  theme_bw() +
  ylab("Diel phase lag (hours)") +
  xlab("Day of year") +
  scale_color_manual(name = "", values = c("Diel phase Lag" = "hotpink3",
                                           "Diel amplitude ratio" = "springgreen4"))+
  scale_y_continuous(
    limits = c(0, 5),
    sec.axis = sec_axis(
      ~ . / 10,
      name = bquote("Diel amplitude ratio (" * AR[d] * ")"),
      breaks = seq(0, 0.5, by = 0.1)))+
  
  
  scale_shape_manual(name = "",
                     values = c("Diel phase Lag" = 1,
                                "Diel amplitude ratio" = 2)) +
  
  guides(shape = guide_legend(override.aes = list(size = c(3,3))),
         color = guide_legend(override.aes = list(linetype = c(1,1), size = c(3,3))),
  ) +
  
  
  theme(
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, color = "black"),
    title = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.text = element_text( size = 10, color = "black"))

p_diel

ggsave(plot = p_diel, filename = "FIGURES/diel_pl_ar.tif", dpi = 500,height = 7 , width = 8, units = "in" )



########################################################################
## Figure 5: Three example streams with differing patterns in wet and dry years
########################################################################

diurnal_ts_metrics = diurnal_ts_metrics %>% mutate(site_id_display = recode(site_id,
                                                                            "government_E323" = "Government",
                                                                            "LB_upper_E017" = "LB Upper",
                                                                            "LB_lower_E126" = "LB Lower",
                                                                            "mckorkle_E118" = "McKorkle",
                                                                            "chamberlain_E032" = "Chamberlain",
                                                                            "buckhorn_E016" = "Buckhorn", 
                                                                            "BC_E109" = "Big Creek", 
                                                                            "grouse_E062" = "Grouse", 
                                                                            "lake_E035" = "Lake", 
                                                                            "logan_E123" = "Logan", 
                                                                            "smith_E208" = "Smith", 
                                                                            "warren_E162" = "Warren"))

library(patchwork)

# Shared color scale with legend title
color_scale <- scale_color_manual(
  values = c(
    "Chamberlain" = "salmon3",
    "Logan" = "steelblue",
    "Warren" = "darkolivegreen3"
  ),
  name = "Stream" # This sets the legend title
)

# Base theme with no legend
base_theme <- theme_bw() +
  theme(
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    title = element_text(size = 11, color = "black"),
    legend.position = "none" # turn off legend in each plot
  )



# Legend OFF in the rest
ar_2021 <- diurnal_metrics %>%
  filter(year == "2021", DOY %in% 175:250, site_id_display %in% c("Chamberlain", "Warren", "Logan")) %>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(site_id_display))) +
  geom_smooth(se = FALSE, lwd = 1.5, method = "gam") +
  color_scale +  # reuse color scale but hide legend via theme
  base_theme +
  xlim(180,250)+
  ylab(expression(AR[d])) +
  xlab("Day of year")+
  ggtitle("(a) 2021 (dry year)")



ar_2022<- diurnal_metrics %>%
  filter(year == "2022", DOY %in% 175:250, site_id_display %in% c("Chamberlain", "Warren", "Logan")) %>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(site_id_display))) +
  geom_smooth(se = FALSE, lwd = 1.5, method = "gam") +
  color_scale +
  xlim(180,250)+
  base_theme +
  ylab("")+
  xlab("Day of year")+
  ggtitle("(b) 2022 (wet year)")

ar_example = ( ar_2021 + ar_2022 ) +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")


ggsave(plot = ar_example, filename = "FIGURES/AR_time_series.tif", dpi = 500,height = 3.5 , width = 7, units = "in" )







###########################################################################
## Figure 6: All streams comparing a wet and dry year
##########################################################################
diurnal_metrics<- diurnal_metrics%>%
  mutate(hydro_category = case_when(
    site_id %in% c("grouse_E062","LB_upper_E017", "lake_E035", "warren_E162") ~ "Variable",
    site_id %in% c("BC_E109", "buckhorn_E016", "chamberlain_E032", "mckorkle_E118") ~ "Atmospheric",
    site_id %in% c("government_E323", "LB_lower_E126", "smith_E208",  "logan_E123") ~ "Groundwater"))


ar_21_23 = diurnal_metrics %>%
  filter(year %in% c("2021", "2022") & DOY %in% 175:250) %>%
  ggplot(aes(DOY, diurnal_AmpRatio, color = factor(year))) +
  geom_smooth(se = TRUE, lwd = 1.5, method = "gam") +
  ylab(expression(AR[d])) +
  xlab("Day of year")+
  facet_wrap(~hydro_category)+
  scale_color_manual(values = c("2021" = "orangered", "2022" = "slateblue"), name = "Year")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    title = element_text(size = 11, color = "black"),
    legend.position = "bottom")

ar_21_23

ggsave(plot = ar_21_23, filename = "FIGURES/AR_21_23_all_streams.tif", dpi = 500,height = 4, width = 8, units = "in" )






##################################################
## FIGURE 7: max temp vs. amp ratio
##################################################

mt1 = ggplot(seasonal_diurnal_trends, aes(avg_AmpRatio, MaxTempC, color = factor(site_id_display)))+
  geom_point()+
  theme_bw()+
  ylab("Max summer stream temperature (C)")+
  xlab(bquote("Average diel amplitude ratio (" * AR[d]*")"))+
  labs(color = "Stream")+
  scale_color_manual(values = c(
    "Chamberlain" = "salmon4",
    "Big Creek" = "salmon1",
    "McKorkle" = "tomato3",
    "Buckhorn" = "tomato1",
    "Logan" = "turquoise4",
    "Government" = "steelblue3",
    "LB Lower" = "slateblue3", 
    "Smith" = "skyblue2", 
    "Grouse" = "springgreen3",
    "LB Upper" = "seagreen",
    "Lake" = "darkseagreen3",
    "Warren" = "darkolivegreen3" 
  ))+
  theme(
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    title = element_text(size = 10, color = "black"),
    legend.position = "right")



ggsave(plot = mt1, filename = "FIGURES/maxtemp_ar.tif", dpi = 500,height = 5, width = 6, units = "in" )







#############################################################################
### Figure 8: snowmelt vs. spei for cliamte section of paper
#############################################################################
annual_climate$total_runoff_avg = annual_climate$total_runoff_avg*1000

annual_climate$total_runoff_max = annual_climate$total_runoff_max*1000

annual_climate$melt_rate_avg = annual_climate$melt_rate_avg*1000

annual_climate$melt_rate_max = annual_climate$melt_rate_max*1000



# tot runoff 
tot_runoff= ggplot(annual_climate, aes(year, total_runoff_avg)) +
  geom_boxplot(color = "black")+
  theme_bw()+
  ggtitle("(a) Total snowmelt runoff")+
  theme(
    axis.title.y.left = element_text(size = 11),
    legend.position = "none", 
    axis.text.x = element_text(size = 10, color = "black", hjust = 1, angle = 45), 
    axis.title.x = element_text(size = 11, color = "black"), 
    plot.title  = element_text(size = 11, color = "black"),
    axis.text.y.left = element_text(size = 10, color = "black"))+
  ylab("Total snowmelt runoff (mm)")+
  xlab("Year")

tot_runoff


### snowmelt rate

snowmelt_rate = ggplot(annual_climate, aes(year, melt_rate_avg)) +
  geom_boxplot(color = "black")+
  theme_bw()+
  ggtitle("(b) Snowmelt rate")+
  theme(
    axis.title.y.left = element_text(size = 11),
    legend.position = "none", 
    axis.text.x = element_text(size = 10, color = "black", hjust = 1, angle = 45), 
    axis.title.x = element_text(size = 11, color = "black"), 
    plot.title  = element_text(size = 11, color = "black"),
    axis.text.y.left = element_text(size = 10, color = "black"))+
  ylab("Snowmelt rate (mm/day)")+
  xlab("Year")

snowmelt_rate

# Snowmelt timing 
snowmelt_time = ggplot(annual_climate, aes(year, DOY_90_pct_runoff)) +
  geom_boxplot(color = "black")+
  theme_bw()+
  ggtitle("(c) Snowmelt timing")+
  theme(
    axis.title.y.left = element_text(size = 11),
    legend.position = "none", 
    axis.text.x = element_text(size = 10, color = "black", hjust = 1, angle = 45), 
    axis.title.x = element_text(size = 11, color = "black"),
    plot.title  = element_text(size = 11, color = "black"),
    axis.text.y.left = element_text(size = 10, color = "black"))+
  ylab("Snowmelt timing (day of year)")+
  xlab("Year")

snowmelt_time

# SPEI 
june_spei= ggplot(annual_climate, aes(year, June_SPEI_3month)) +
  geom_boxplot(color = "black")+
  theme_bw()+
  ggtitle("(d) Spring SPEI")+
  theme(
    axis.title.y.left = element_text(size = 11),
    legend.position = "none", 
    axis.text.x = element_text(size = 10, color = "black", hjust = 1, angle = 45), 
    axis.title.x = element_text(size = 11, color = "black"), 
    plot.title  = element_text(size = 11, color = "black"),
    
    axis.text.y.left = element_text(size = 10, color = "black"))+
  ylab("Spring SPEI")+
  xlab("Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

june_spei


climate_all = grid.arrange(tot_runoff, snowmelt_rate, snowmelt_time, june_spei, ncol = 2)

ggsave(plot = climate_all, filename =FIGURES/snowmelt_spei_all.tif',dpi = 500,height = 6 , width = 8, units = "in"  )







####################################################################
## FIGURE 9: Mixed Effects Model Results
####################################################################

# total runoff
p1 = ggplot(seasonal_diurnal_trends,aes(total_runoff_avg, avg_AmpRatio, group=site_id_display, col=site_id_display)) + 
  facet_grid(~hydro_category) +
  geom_line(aes(y=fit_m1), size=0.8) +
  geom_point(alpha = 0.3) + 
  theme_bw()+
  ggtitle("a")+
  ylab(expression(AR[d]))+
  xlab("Total snowmelt runoff (mm)")+
  labs(color = "Stream")+
  scale_color_manual(values = c(
      "Chamberlain" = "salmon4",
      "Big Creek" = "salmon1",
      "McKorkle" = "tomato3",
      "Buckhorn" = "tomato1",
      "Logan" = "turquoise4",
      "Government" = "steelblue3",
      "LB Lower" = "slateblue3", 
      "Smith" = "skyblue2", 
      "Grouse" = "springgreen3",
      "LB Upper" = "seagreen",
      "Lake" = "darkseagreen3",
      "Warren" = "darkolivegreen3" 
      ))+
  scale_x_continuous(breaks = c(500, 1000, 1500))+
  theme(
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none")




# melt rate
p2 = ggplot(seasonal_diurnal_trends,aes(melt_rate_to_90, avg_AmpRatio, group=site_id_display, col=site_id_display)) + 
  facet_grid(~hydro_category) +
  geom_line(aes(y=fit_m2), size=0.8) +
  geom_point(alpha = 0.3) + 
  theme_bw()+
  ggtitle("b")+
  ylab("")+
  xlab("Average snowmelt rate (mm/day)")+
  labs(color = "Stream")+
  scale_color_manual(values = c(
    "Chamberlain" = "salmon4",
    "Big Creek" = "salmon1",
    "McKorkle" = "tomato3",
    "Buckhorn" = "tomato1",
    "Logan" = "turquoise4",
    "Government" = "steelblue3",
    "LB Lower" = "slateblue3", 
    "Smith" = "skyblue2", 
    "Grouse" = "springgreen3",
    "LB Upper" = "seagreen",
    "Lake" = "darkseagreen3",
    "Warren" = "darkolivegreen3" 
  ))+
  scale_x_continuous(breaks = c(2, 5,  10, 15))+
  theme(
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none")





# spei
p3 = ggplot(seasonal_diurnal_trends,aes(June_SPEI_3month, avg_AmpRatio, group=site_id_display, col=site_id_display)) + 
  facet_grid(~hydro_category) +
  geom_line(aes(y=fit_m3), size=0.8) +
  geom_point(alpha = 0.3) + 
  theme_bw()+
  ggtitle("c")+
  ylab(expression(AR[d]))+
  xlab("Spring SPEI")+
  labs(color = "Stream")+
  scale_color_manual(values = c(
    "Chamberlain" = "salmon4",
    "Big Creek" = "salmon1",
    "McKorkle" = "tomato3",
    "Buckhorn" = "tomato1",
    "Logan" = "turquoise4",
    "Government" = "steelblue3",
    "LB Lower" = "slateblue3", 
    "Smith" = "skyblue2", 
    "Grouse" = "springgreen3",
    "LB Upper" = "seagreen",
    "Lake" = "darkseagreen3",
    "Warren" = "darkolivegreen3" 
  ))+
  theme(
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none")



# timing
p4 = ggplot(seasonal_diurnal_trends,aes(DOY_90_pct_runoff, avg_AmpRatio, group=site_id_display, col=site_id_display)) + 
  facet_grid(~hydro_category) +
  geom_smooth(aes(y=fit_m4), size=0.8, se = FALSE, method = "lm") +
  geom_point(alpha = 0.3) + 
  theme_bw()+
  ggtitle("d")+
  ylab("")+
  xlab("Snowmelt timing (day of year)")+
  labs(color = "Stream")+
  scale_color_manual(values = c(
    "Chamberlain" = "salmon4",
    "Big Creek" = "salmon1",
    "McKorkle" = "tomato3",
    "Buckhorn" = "tomato1",
    "Logan" = "turquoise4",
    "Government" = "steelblue3",
    "LB Lower" = "slateblue3", 
    "Smith" = "skyblue2", 
    "Grouse" = "springgreen3",
    "LB Upper" = "seagreen",
    "Lake" = "darkseagreen3",
    "Warren" = "darkolivegreen3" 
  ))+
  scale_x_continuous(breaks = c(160, 180,200))+
  theme(
    axis.title.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    title = element_text(size = 11, color = "black"),
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    legend.position = "none")


combo = grid.arrange(p1, p2, p3, p4, ncol = 2)

ggsave(plot = combo, filename = "FIGURES/model_results_ar.tif", dpi = 500,height = 6 , width = 8.5, units = "in" )




##################################################################
## APPENDIX
##################################################################

### Comparing Little Buckhorn 2024 overlap
July_air_compare = LB_air_compare %>%
  filter(DOY == 200) %>%
  ggplot(aes(x = TimeofDay)) +
  geom_point(aes(y = scale(AirTempC_RAWS), color = "RAWS", shape = "RAWS"), size = 2) +
  geom_smooth(aes(y = scale(AirTempC_RAWS), color = "RAWS"), se = FALSE) +
  geom_point(aes(y = scale(AirTempC_logger), color = "Streamside Logger", shape = "Streamside Logger"), size = 2) +
  geom_smooth(aes(y = scale(AirTempC_logger), color = "Streamside Logger"), se = FALSE) +
  scale_color_manual(name = "", values = c("RAWS" = "purple", "Streamside Logger" = "seagreen3")) +
  scale_shape_manual(name = "", values = c("RAWS" = 17, "Streamside Logger" = 16)) +
  ggtitle("July 18, 2024") +
  ylab("Scaled air temperature (°C)") +
  xlab("Time of day (hour)") +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    title = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.text = element_text( size = 10, color = "black"))


ggsave(plot = July_air_compare, filename ='Stream_Temp/FIGURES/appendix_july_air_compare.tif',dpi = 500,height = 6 , width = 6, units = "in"  )


## Seasonal
seasonal_air_compare = LB_air_compare %>% filter(DOY %in% 175:250)%>%
  ggplot( aes(x = DOY)) +
  #geom_smooth(aes(y = scale(AirTempC_RAWS), color = "RAWS"), se = FALSE, span = .1) +
  geom_point(aes(y = scale(AirTempC_logger), color = "Streamside Logger",shape = "Streamside Logger"), size = 2) +
  geom_point(aes(y = scale(AirTempC_RAWS), color = "RAWS", shape = "RAWS"), size = 2) +
  #geom_smooth(aes(y = scale(AirTempC_logger), color = "Streamside Logger"), se = FALSE, span = .1) +
  scale_color_manual(name = "", values = c("RAWS" = "purple", "Streamside Logger" = "seagreen3")) +
  scale_shape_manual(name = "", values = c("RAWS" = 17, "Streamside Logger" = 16)) +
  ggtitle("") +
  ylab("Scaled air temperature (°C)") +
  xlab("Day of Year") +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    title = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.text = element_text( size = 10, color = "black"))


ggsave(plot = seasonal_air_compare, filename ='Stream_Temp/FIGURES/appendix_seasonal_air_compare.tif',dpi = 500,height = 6 , width = 8, units = "in"  )

## Compare all sites


appendix_all_sites = diurnal_dynamic_metrics %>% filter(DOY %in% 175:250)%>%
  ggplot( aes(DOY, scale(Amp_AT), group = site_id, color = station_type))+
  #geom_smooth(se = FALSE, span = 0.5)+
  facet_wrap(~year)+
  #geom_point()+
  geom_line(size = 0.75)+
  ylab("Scaled daily air temp amplitude")+
  xlab("Day of year")+
  scale_color_manual(name = "",values = c("RAWS" = "purple", "Streamside" = "seagreen3"))+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 11, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    title = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.text = element_text( size = 10, color = "black"))

ggsave(plot = appendix_all_sites, filename ='Stream_Temp/FIGURES/appendix_all_sites.tif',dpi = 500,height = 6 , width = 8, units = "in"  )



