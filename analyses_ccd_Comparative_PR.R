# Classic_conjoint_analyses_bycountry ####

#this script is for the analyses related to the visual conjoint design, 
#when considering a country at a time or pooled all together

## Library calls ####
pacman::p_load(
  cregg, dplyr, ggpubr, cowplot,
  stringr,MASS, cjoint, corrplot, 
  dplyr,forcats, ggplot2, gt, 
  gtools, gtsummary, margins, 
  scales, openxlsx, patchwork, rio, texreg,
  tools,lme4, ggeffects, wesanderson,
  officer, flextable, writexl,
  tidyr
)

## Functions ####



set_categories_and_levels_visual_bycountry = function(effects){
  #Here I define a function to set categories and levels in a neat and presentable 
  #fashion in the mm dataset resulting from the cj function. The
  #functio
  
  effects <- effects %>%
    mutate(feature = gsub("ccd_", "", feature) %>% # Remove "vcd_"
             tools::toTitleCase())   
  
  return(effects)
}


draw_plot_pooled = function(effects,
                            estimator=c("mm", 
                                        "amce",
                                        "mm_differences", 
                                        "amce_differences"), 
                            leftlim=999,
                            rightlim=999,
                            x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  
  
  estimator=match.arg(estimator)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
    leftlim=ifelse(estimator!="mm", -0.3, 0.2)
    rightlim=ifelse(estimator!="mm", 0.3, 0.7)
  }
  
  if(x_intercept == 999)
  {
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  
  
  y_POOL=0
  
  size_pool=2
  
  alpha_plot = 1
  size_plot=1
  country_coeff=0.5
  fatten_plot=1
  
  text_size=3
  
  v=list()
  for(attribute in unique(effects$feature))
  {
    
    data_POOL= effects |> filter(ccd_country=="POOL" & feature == attribute)
    
    these_labels = rev(unique(effects[effects$feature==attribute, ]$level))
    
    p = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_point(data=data_POOL,
                 aes(x=estimate, 
                     y=level),
                 size = size_pool,
                 position = position_nudge(y = y_POOL),
                 show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level),
                     alpha = alpha_plot,
                     size = 2*size_plot,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level),
                     alpha = 0.8*alpha_plot,
                     size = size_plot,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level),
                     alpha = 0.6*alpha_plot,
                     size = 1/2*size_plot,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_text(data=data_POOL,
                aes(x = estimate, y = level, 
                    label = scales::percent(estimate, accuracy = 0.1)),  
                hjust = 0, vjust=0, 
                size = text_size,  
                position = position_nudge(y = y_POOL+1/8#, x=1/9
                )) +
      ylab(attribute)+
      xlab("")+
      scale_y_discrete(limits = these_labels)+
      theme_minimal(base_size = 10) 
    
    v[[attribute]] = p
  }
  
  return(v)
}

draw_plot_comparative = function(effects,
                                 estimator=c("mm", "amce",
                                             "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                                 leftlim=999, #the left limit of the plot
                                 rightlim=999,#the right limit of the plot
                                 x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  
  
  estimator=match.arg(estimator)
  
  v = list()
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
    leftlim=ifelse(estimator!="mm", -0.3, 0.2)
    rightlim=ifelse(estimator!="mm", 0.3, 0.7)
  }
  
  if(x_intercept == 999)
  {
    intercept = 0.5
  }
  
  
  increment = 1/6
  y_CZ=3/2*increment
  y_FR = 1*increment/2
  y_IT=-1*increment/2
  y_SW=-3/2*increment
  
  size_countries=2
  
  alpha_countries = 1
  
  country_coeff = 1
  alpha_plot = 1
  size_plot=0.7
  
  text_size=3
  
  v=list()
  for(attribute in unique(effects$feature))
  {
    
    # data_IT= effects |> filter(BY=="IT - CZ" & feature == attribute)
    # data_FR= effects |> filter(BY=="FR - CZ" & feature == attribute)
    # data_SW= effects |> filter(BY=="SW - CZ" & feature == attribute)
    
    data_IT = effects |> filter(BY=="IT" & feature == attribute)
    data_FR = effects |> filter(BY=="FR" & feature == attribute)
    data_SW = effects |> filter(BY=="SW" & feature == attribute)
    data_CZ = effects |> filter(BY=="CZ" & feature == attribute)
    
    # data_CZ = data_IT
    # data_CZ$estimate = 0
    # data_CZ$std.error = NA
    # data_CZ$lower = NA
    # data_CZ$upper = NA
    
    # CZ is the reference category!
    
    these_labels = rev(unique(effects[effects$feature==attribute, ]$level))
    
    p = ggplot()+
      #geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_point(data=data_CZ,
                 aes(x=estimate, 
                     y=level, col = "CZ", shape = "CZ"),
                 size = size_countries+1,
                 position = position_nudge(y = y_CZ),
                 show.legend = T)+
      geom_point(data=data_IT,
                 aes(x=estimate, 
                     y=level, col = "IT", shape = "IT"),
                 size = size_countries,
                 position = position_nudge(y = y_IT),
                 show.legend = T)+
      geom_point(data=data_FR,
                 aes(x=estimate, 
                     y=level, col = "FR", shape = "FR"),
                 size = size_countries,
                 position = position_nudge(y = y_FR),
                 show.legend = T)+
      geom_point(data=data_SW,
                 aes(x=estimate, 
                     y=level, col = "SW", shape = "SW"),
                 size = size_countries,
                 position = position_nudge(y = y_SW),
                 show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "IT"),
                     alpha = alpha_plot,
                     size = 2*size_plot*country_coeff,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "IT"),
                     alpha = 0.8*alpha_plot,
                     size = size_plot*country_coeff,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "IT"),
                     alpha = 0.6*alpha_plot,
                     size = 1/2*size_plot*country_coeff,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "FR"),
                     alpha = alpha_plot,
                     size = 2*size_plot*country_coeff,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "FR"),
                     alpha = 0.8*alpha_plot,
                     size = size_plot*country_coeff,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "FR"),
                     alpha = 0.6*alpha_plot,
                     size = 1/2*size_plot*country_coeff,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "SW"),
                     alpha = alpha_plot,
                     size = 2*size_plot*country_coeff,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "SW"),
                     alpha = 0.8*alpha_plot,
                     size = size_plot*country_coeff,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "SW"),
                     alpha = 0.6*alpha_plot,
                     size = 1/2*size_plot*country_coeff,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "CZ"),
                     alpha = alpha_plot,
                     size = 2*size_plot*country_coeff,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "CZ"),
                     alpha = 0.8*alpha_plot,
                     size = size_plot*country_coeff,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "CZ"),
                     alpha = 0.6*alpha_plot,
                     size = 1/2*size_plot*country_coeff,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      ylab(attribute)+
      xlab("")+
      scale_y_discrete(limits = these_labels)+
      scale_color_manual(
        values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[4]
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW"
        )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW"
        )
      ) +
      theme_minimal(base_size = 10)+
      theme(
        panel.grid.major.x = element_line(color = "gray80"),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.background = element_rect(fill = "white", color = NA)  # ensure white background
      )# +  # Adjust font size for better readability
    # theme(
    #   legend.position = "right",  # You can change this to "top", "bottom", etc.
    #   axis.text.y = element_text(size = 10),
    #   axis.title.y = element_text(size = 12),
    #   plot.background = element_rect(fill = "white", color = NA)  # White background
    # )
    
    v[[attribute]] = p
  }
  
  
  # 
  # if(estimator =="mm")
  # {
  #   leftlim_gender=0.1
  #   rightlim_gender=0.9
  # }
  # 
  # if(estimator =="amce")
  # {
  #   leftlim_gender=-0.4
  #   rightlim_gender=0.4
  # }
  # 
  # v[["Gender"]] = v[["Gender"]] + scale_x_continuous(limits = c(leftlim_gender, rightlim_gender), 
  #                                                    breaks = round(seq(leftlim_gender, 
  #                                                                       rightlim_gender,
  #                                                                       length.out = 7), 
  #                                                                   digits=3))
  # 
  # if(estimator =="mm")
  # {
  #   leftlim_transport=0.3
  #   rightlim_transport=0.7
  # }
  # 
  # if(estimator =="amce")
  # {
  #   leftlim_transport=-0.2
  #   rightlim_transport=0.2
  # }
  # 
  # v[["Transport"]] = v[["Transport"]] + scale_x_continuous(limits = c(leftlim_transport, rightlim_transport), 
  #                                                    breaks = round(seq(leftlim_transport, 
  #                                                                       rightlim_transport,
  #                                                                       length.out = 7), 
  #                                                                   digits=3))
  # 
  return(v)
}


full_interaction_effects_bycountry = function(data,
                                              formula,
                                              type_of_interaction,
                                              leftlim=0.33,
                                              rightlim=0.67,
                                              height_pic=8,
                                              width_pic=8){
  
  effects <- data |>
    cj(formula, 
       id = ~respid,
       estimate = "mm",
       by=~ccd_country)
  
  effects_90 <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~ccd_country,
       alpha = 0.1,
       estimate = "mm"
    )
  
  effects_99 <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~ccd_country,
       alpha = 0.01,
       estimate = "mm"
    )
  
  effects$lower90 = effects_90$lower
  effects$upper90 = effects_90$upper
  effects$lower99 = effects_99$lower
  effectsy$upper99 = effects_99$upper
  
  
  effects_pooled <- readRDS(paste0(
    gdrive_code,
    input_pooled_data,
    "mm_clustSE_",
    type_of_interaction,
    ".rds")
  )
  effects_pooled$ccd_country = "POOL"
  effects_pooled$BY = "POOL"
  
  effects=rbind(effects, effects_pooled)
  
  data_POOL= effects |> filter(ccd_country=="POOL")
  
  effects$level = factor(effects$level, 
                         levels = data_POOL[order(data_POOL$estimate,
                                                  decreasing = T), ]$level)
  
  data_IT= effects |> filter(ccd_country=="IT")
  data_FR= effects |> filter(ccd_country=="FR")
  data_SW= effects |> filter(ccd_country=="SW")
  data_CZ= effects |> filter(ccd_country=="CZ")
  data_POOL= effects |> filter(ccd_country=="POOL")
  
  
  increment = 1/6
  y_CZ=2*increment
  y_FR = increment
  y_POOL=0
  y_IT=-1*increment
  y_SW=-2*increment
  
  size_countries=1.5
  size_pool=2
  
  alpha_countries = 0.5
  
  fatten_countries = 1.5
  fatten_pool = 4.5
  
  alpha_plot = 1
  size_plot=1
  country_coeff = 0.8
  fatten_plot=1
  
  text_size=3.5
  
  p=ggplot()+
    geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
    geom_point(data=data_IT,
               aes(x=estimate, 
                   y=level, col = "IT", shape = "IT"),
               size = size_countries,
               position = position_nudge(y = y_IT),
               show.legend = T)+
    geom_point(data=data_FR,
               aes(x=estimate, 
                   y=level, col = "FR", shape = "FR"),
               size = size_countries,
               position = position_nudge(y = y_FR),
               show.legend = T)+
    geom_point(data=data_CZ,
               aes(x=estimate, 
                   y=level, col = "CZ", shape = "CZ"),
               size = size_countries,
               position = position_nudge(y = y_CZ),
               show.legend = T)+
    geom_point(data=data_SW,
               aes(x=estimate, 
                   y=level, col = "SW", shape = "SW"),
               size = size_countries,
               position = position_nudge(y = y_SW),
               show.legend = T)+
    geom_point(data=data_POOL,
               aes(x=estimate, 
                   y=level, col = "POOL", shape = "POOL"),
               size = size_countries,
               position = position_nudge(y = y_POOL),
               show.legend = T)+
    geom_linerange(data=data_IT,
                   aes(x=estimate, xmin=lower90, xmax=upper90, 
                       y=level, col = "IT"),
                   alpha = alpha_plot,
                   size = 2*size_plot*country_coeff,
                   position = position_nudge(y = y_IT),
                   show.legend = T)+
    geom_linerange(data=data_IT,
                   aes(x=estimate, xmin=lower, xmax=upper, 
                       y=level, col = "IT"),
                   alpha = 0.8*alpha_plot,
                   size = size_plot*country_coeff,
                   position = position_nudge(y = y_IT),
                   show.legend = T)+
    geom_linerange(data=data_IT,
                   aes(x=estimate, xmin=lower99, xmax=upper99, 
                       y=level, col = "IT"),
                   alpha = 0.6*alpha_plot,
                   size = 1/2*size_plot*country_coeff,
                   position = position_nudge(y = y_IT),
                   show.legend = T)+
    geom_linerange(data=data_FR,
                   aes(x=estimate, xmin=lower90, xmax=upper90, 
                       y=level, col = "FR"),
                   alpha = alpha_plot,
                   size = 2*size_plot*country_coeff,
                   position = position_nudge(y = y_FR),
                   show.legend = T)+
    geom_linerange(data=data_FR,
                   aes(x=estimate, xmin=lower, xmax=upper, 
                       y=level, col = "FR"),
                   alpha = 0.8*alpha_plot,
                   size = size_plot*country_coeff,
                   position = position_nudge(y = y_FR),
                   show.legend = T)+
    geom_linerange(data=data_FR,
                   aes(x=estimate, xmin=lower99, xmax=upper99, 
                       y=level, col = "FR"),
                   alpha = 0.6*alpha_plot,
                   size = 1/2*size_plot*country_coeff,
                   position = position_nudge(y = y_FR),
                   show.legend = T)+
    geom_linerange(data=data_CZ,
                   aes(x=estimate, xmin=lower90, xmax=upper90, 
                       y=level, col = "CZ"),
                   alpha = alpha_plot,
                   size = 2*size_plot*country_coeff,
                   position = position_nudge(y = y_CZ),
                   show.legend = T)+
    geom_linerange(data=data_CZ,
                   aes(x=estimate, xmin=lower, xmax=upper, 
                       y=level, col = "CZ"),
                   alpha = 0.8*alpha_plot,
                   size = size_plot*country_coeff,
                   position = position_nudge(y = y_CZ),
                   show.legend = T)+
    geom_linerange(data=data_CZ,
                   aes(x=estimate, xmin=lower99, xmax=upper99, 
                       y=level, col = "CZ"),
                   alpha = 0.6*alpha_plot,
                   size = 1/2*size_plot*country_coeff,
                   position = position_nudge(y = y_CZ),
                   show.legend = T)+
    geom_linerange(data=data_SW,
                   aes(x=estimate, xmin=lower90, xmax=upper90, 
                       y=level, col = "SW"),
                   alpha = alpha_plot,
                   size = 2*size_plot*country_coeff,
                   position = position_nudge(y = y_SW),
                   show.legend = T)+
    geom_linerange(data=data_SW,
                   aes(x=estimate, xmin=lower, xmax=upper, 
                       y=level, col = "SW"),
                   alpha = 0.8*alpha_plot,
                   size = size_plot*country_coeff,
                   position = position_nudge(y = y_SW),
                   show.legend = T)+
    geom_linerange(data=data_SW,
                   aes(x=estimate, xmin=lower99, xmax=upper99, 
                       y=level, col = "SW"),
                   alpha = 0.6*alpha_plot,
                   size = 1/2*size_plot*country_coeff,
                   position = position_nudge(y = y_SW),
                   show.legend = T)+
    geom_linerange(data=data_POOL,
                   aes(x=estimate, xmin=lower90, xmax=upper90, 
                       y=level, col = "POOL"),
                   alpha = alpha_plot,
                   size = 2*size_plot,
                   position = position_nudge(y = y_POOL),
                   show.legend = T)+
    geom_linerange(data=data_POOL,
                   aes(x=estimate, xmin=lower, xmax=upper, 
                       y=level, col = "POOL"),
                   alpha = 0.8*alpha_plot,
                   size = size_plot,
                   position = position_nudge(y = y_POOL),
                   show.legend = T)+
    geom_linerange(data=data_POOL,
                   aes(x=estimate, xmin=lower99, xmax=upper99, 
                       y=level, col = "POOL"),
                   alpha = 0.6*alpha_plot,
                   size = 1/2*size_plot,
                   position = position_nudge(y = y_POOL),
                   show.legend = T)+
    geom_text(data=data_POOL,
              aes(x = estimate, y = level, 
                  label = scales::percent(estimate, accuracy = 0.1)),  
              hjust = -2, vjust=0, 
              size = text_size,  
              position = position_nudge(y = y_POOL+1/32#, x=1/9
              ),
              color = "black") +
    ylab("")+
    xlab("Marginal Means")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, rightlim,
                                          length.out = 9), 
                                      digits=3),
                       labels = label_percent(accuracy = 0.1)  # Ensure x-axis is also in %
    )+
    #scale_y_discrete(limits = rev(these_labels))+
    scale_color_manual(
      values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                 "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                 "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                 "SW" = wesanderson::wes_palettes$Darjeeling1[4],
                 "POOL" = 'black'
      ),
      name = "Country",
      limits = c("CZ", "FR", "IT", "SW", "POOL"
      )
    ) +
    scale_shape_manual(
      values = c("IT" = 19, 
                 "FR" = 17, 
                 "SW" = 15, 
                 "CZ" = 18,
                 "POOL" = 16
      ),
      name = "Country",
      limits = c("CZ", "FR", "IT", "SW", "POOL"
      )
    ) +
    theme_minimal(base_size = 10) +  # Adjust font size for better readability
    theme(
      legend.position = "right",  # You can change this to "top", "bottom", etc.
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA)  # White background
    )
  
  
  ggsave(paste0(output_wd, subdir,"interacted_", type_of_interaction, ".tif"), 
         p, 
         height = height_pic, 
         width = width_pic,
         dpi = 600,
         create.dir = T)
  
  saveRDS(p, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, ".rds"))
  
  #saveRDS(effects, file = paste0(output_wd, subdir,"interacted_", type_of_interaction, "_data.rds"))
  
  return(p)
  
}


full_analysis_bycountry = function(data,
                                   formula, #the conjoint formula
                                   estimator=c("mm","amce"), #marginal means and amces
                                   subdir, #the subdirectory where the plots will be saved
                                   continuous=F,#to change if we are dealing with continuous outcome
                                   leftlim=999,
                                   rightlim=999
){
  
  
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork
  
  #browser()
  
  estimator=match.arg(estimator)
  
  effects_pooled <- readRDS(paste0(
    gdrive_code,
    input_pooled_data,
    estimator,
    "_clustSE",
    ".rds")
  )
  
  effects_bycountry <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~ccd_country,
       estimate = estimator
    )
  
  effects_90 <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~ccd_country,
       alpha = 0.1,
       estimate = estimator
    )
  
  effects_99 <- data |>
    cj(formula, 
       id = ~respid, 
       by = ~ccd_country,
       alpha = 0.01,
       estimate = estimator
    )
  
  effects_bycountry$lower90 = effects_90$lower
  effects_bycountry$upper90 = effects_90$upper
  effects_bycountry$lower99 = effects_99$lower
  effects_bycountry$upper99 = effects_99$upper
  
  
  effects_pooled = set_categories_and_levels_visual_bycountry(effects_pooled)
  
  effects_bycountry = set_categories_and_levels_visual_bycountry(effects_bycountry)
  
  
  effects_pooled$BY = "POOL"
  
  effects_pooled$ccd_country = "POOL"
  
  effects = rbind(effects_bycountry, effects_pooled)
  
  
  midpoint = ifelse(estimator == "amce", 0, 0.5)
  
  pooled_plot = draw_plot_pooled(effects_pooled,
                                 estimator=estimator,
                                 leftlim = midpoint-0.15,
                                 rightlim = midpoint+0.15)
  
  
  comparative_plot = draw_plot_comparative(effects_bycountry,
                                           estimator=estimator,
                                           leftlim = midpoint-0.15,
                                           rightlim = midpoint+0.15)
  
  
  return_list = list(plot_pooled = pooled_plot, plot_comparative = 
                       comparative_plot, 
                     effects = effects)
  
  if (!dir.exists(paste0(output_wd, subdir))) {
    dir.create(paste0(output_wd, subdir), recursive = T)
  }
  
  saveRDS(effects, paste0(output_wd, subdir, "main_effects.rds"))
  
  effects$CI = paste0("(", round(effects$lower, digits = 3), "; ", round(effects$upper, digits = 3), ")")
  effects$CI_90 = paste0("(", round(effects$lower90, digits = 3), "; ", round(effects$upper90, digits = 3), ")")
  effects$CI_99 = paste0("(", round(effects$lower99, digits = 3), "; ", round(effects$upper99, digits = 3), ")")
  
  effects$estimate = round(effects$estimate, digits = 3)
  effects$std.error = round(effects$std.error, digits = 3)
  
  
  effects |> 
    select(BY, feature, level, estimate, std.error, CI, CI_90, CI_99) |>
    rename(
      Country = BY,
      Attribute = feature,
      `Attribute level` = level,
      Estimate = estimate,
      `Std. Error` = std.error,
      `95% Confidence Interval` = CI,
      `90% Confidence Interval` = CI_90,
      `99% Confidence Interval` = CI_99
    ) |>
    mutate(across(where(is.numeric), ~ signif(., digits = 3))) |>
    write_xlsx(paste0(output_wd, subdir, estimator, "_bycountry.xlsx"))
  
  
  return(return_list)
}


full_subgroup_analysis = function(data,
                                  formula,
                                  estimator =c("mm","mm_differences"),
                                  subdir, #the subdirectory where plots will be saved
                                  leftlim=999, #the left limit of the plot
                                  rightlim=999,#the right limit of the plot
                                  x_intercept=999, #the vertical line to signal the difference from the insignificance
                                  subgroup_variable, #the name of the variable on which subgroup analysis is conducted
                                  subgroup_name, #the grouping name in natural language in natural language (eg. età, titolo di studio)
                                  subgroup1, #the name of the first subgroup (variable level)
                                  subgroup2 #the name of the second subgroup (variable level)
){
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
    leftlim=ifelse(estimator!="mm", -1, 0)
    rightlim=1
  }
  if(x_intercept==999)
  {
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  
  
  countries = c("CZ", "FR", "IT", "SW")
  
  data$temp_subgroup = data[[subgroup_variable]]
  
  
  if(estimator == "mm")
    effects_pooled = readRDS(paste0(
      gdrive_code,
      input_pooled_data,
      "mm_clustSE_subgroup__",
      subgroup_variable,
      ".rds")
    )
  
  if(estimator == "mm_differences")
    effects_pooled = readRDS(paste0(
      gdrive_code,
      input_pooled_data,
      "mm_clustSE_subgroup__",
      subgroup_variable,
      "_diff.rds")
    )
  
  
  effects_pooled$ccd_country = "POOL"
  
  effects_pooled = set_categories_and_levels_visual_bycountry(effects_pooled)
  
  names(effects_pooled)[12] = "temp_subgroup"
  
  for(country_code in countries)
  {
    effects_country <- data |>
      filter(country_code == country) |>
      cj(formula, 
         id = ~respid,
         by = ~temp_subgroup,
         estimate = estimator,
         alpha=0.01)
    
    effects_country$ccd_country = country_code
    
    effects_country = set_categories_and_levels_visual_bycountry(effects_country)
    
    
    effects_pooled = rbind(effects_pooled, 
                           effects_country)
  }
  
  
  v=list()
  
  effects_pooled = effects_pooled[effects_pooled$level != "Non-Binary", ] #not enough power!
  
  effects1 = effects_pooled |>
    filter(temp_subgroup == subgroup1)
  
  effects2 = effects_pooled |>
    filter(temp_subgroup == subgroup2)
  
  increment = 1/6
  # y_CZ1=2*increment
  # y_FR1 = increment
  y_POOL1=-1/6
  # y_IT1=-1*increment
  # y_SW1=-2*increment
  
  # y_CZ2=y_CZ1+1/12
  # y_FR2 = y_FR1+1/12
  y_POOL2=+1/6
  # y_IT2= y_IT1+1/12
  # y_SW2=y_SW1+1/12
  
  
  size_countries=0.3
  size_pool=1
  
  alpha_countries = 0.5
  
  fatten_countries = 1.5
  fatten_pool = 4.5
  
  text_size=3.5
  
  effects = effects_pooled
  
  if(estimator=="mm")
  {
    for(attribute in unique(effects$feature))
    {
      data_IT1 = effects1 |> filter(ccd_country=="IT" & feature == attribute)
      data_FR1 = effects1 |> filter(ccd_country=="FR" & feature == attribute)
      data_SW1 = effects1 |> filter(ccd_country=="SW" & feature == attribute)
      data_CZ1 = effects1 |> filter(ccd_country=="CZ" & feature == attribute)
      data_POOL1 = effects1 |> filter(ccd_country=="POOL" & feature == attribute)
      
      data_IT2 = effects2 |> filter(ccd_country=="IT" & feature == attribute)
      data_FR2 = effects2 |> filter(ccd_country=="FR" & feature == attribute)
      data_SW2 = effects2 |> filter(ccd_country=="SW" & feature == attribute)
      data_CZ2 = effects2 |> filter(ccd_country=="CZ" & feature == attribute)
      data_POOL2 = effects2 |> filter(ccd_country=="POOL" & feature == attribute)
      
      p = ggplot()+
        geom_vline(aes(xintercept=0.5), col="black", alpha=1/4)+
        geom_pointrange(data=data_POOL1,
                        aes(x=estimate, xmin=lower, xmax=upper,
                            y=level, col = "POOL"),
                        shape = 19,
                        alpha = 1,
                        #size=size_pool,
                        fatten = fatten_pool,
                        position = position_nudge(y = y_POOL1),
                        show.legend = T)+
       geom_pointrange(data=data_POOL2,
                        aes(x=estimate, xmin=lower, xmax=upper,
                            y=level, col = "POOL"),
                        shape = 17,
                        alpha = 1,
                        #size=size_pool,
                        fatten = fatten_pool,
                        position = position_nudge(y = y_POOL2),
                        show.legend = T)+
        geom_text(data=data_POOL1,
                  aes(x = estimate, y = level, 
                      label = scales::percent(estimate, accuracy = 0.1)),  
                  hjust = -0.5, 
                  vjust=0.1, 
                  size = text_size,  
                  position = position_nudge(y = y_POOL1+1/16#, x=1/9
                  ),
                  color = "black") +
        geom_text(data=data_POOL2,
                  aes(x = estimate, y = level, 
                      label = scales::percent(estimate, accuracy = 0.1)),  
                  hjust = -0.5, 
                  vjust=0.1, 
                  size = text_size,  
                  position = position_nudge(y = y_POOL2+1/16#, x=1/9
                  ),
                  color = "black") +
        ylab(attribute)+
        xlab("")+
        scale_x_continuous(limits = c(leftlim, rightlim), 
                           breaks = round(seq(leftlim, rightlim,
                                              length.out = 9), 
                                          digits=3),
                           labels = label_percent(accuracy = 0.1)  # Ensure x-axis is also in %
        )+
        #scale_y_discrete(limits = rev(these_labels))+
        scale_color_manual(
          values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                     "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                     "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                     "SW" = wesanderson::wes_palettes$Darjeeling1[4],
                     "POOL" = 'black'
          ),
          name = "Country",
          limits = c("CZ", "FR", "IT", "SW", "POOL"
          )
        ) +
        theme_minimal(base_size = 10) +  # Adjust font size for better readability
        theme(
          legend.position = "none",  # You can change this to "top", "bottom", etc.
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA)  # White background
        )
      
      v[[attribute]] = p
    }
    
  }
  
  if(estimator == "mm_differences")
  {
    for(attribute in unique(effects$feature))
    {
      data_POOL = effects |> filter(ccd_country=="POOL" & feature == attribute)
      
      p = ggplot()+
        geom_vline(aes(xintercept=0), col="black", alpha=1/4)+
        geom_pointrange(data=data_POOL,
                        aes(x=estimate, 
                            xmin=lower, 
                            xmax=upper,
                            y=level),
                        col = "black",
                        shape = 19
                        #alpha = 1,
                        #size=size_pool,
                        #fatten = fatten_pool,
                        #position = position_nudge(y = y_POOL1),
                        #show.legend = T
        )+
        geom_text(data=data_POOL,
                  aes(x = estimate, y = level, 
                      label = scales::percent(estimate, accuracy = 0.1)),  
                  hjust = -0.5, 
                  vjust=0.1, 
                  size = text_size,  
                  position = position_nudge(y = 1/16#, x=1/9
                  ),
                  color = "black") +
        ylab(attribute)+
        xlab("")+
        scale_x_continuous(limits = c(leftlim, rightlim), 
                           breaks = round(seq(leftlim, rightlim,
                                              length.out = 9), 
                                          digits=3),
                           labels = label_percent(accuracy = 0.1)  # Ensure x-axis is also in %
        )+
        #scale_y_discrete(limits = rev(these_labels))+
        theme_minimal(base_size = 10) +  # Adjust font size for better readability
        theme(
          legend.position = "none",  # You can change this to "top", "bottom", etc.
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.background = element_rect(fill = "white", color = NA)  # White background
        )
      
      v[[attribute]] = p
    }
    
    
  }
  
  
  p1 = (v[["Gender"]]/
          v[["Age"]]/
          v[["Religion"]]/
          v[["Citysize"]]/
          v[["Profession"]]+
          plot_layout(heights = c(3,3,3,3,4)))
  
  p2 = (v[["Consc"]]/
          v[["Openness"]]/
          v[["Neuroticism"]]/
          v[["Restaurant"]]/
          v[["Transport"]]/
          (v[["Pet"]]+xlab("Effect size")))+
    plot_layout(heights = c(2,2,2,4,3,4))
  
  p=p1
  
  p = p+patchwork::plot_annotation(caption=paste0("Circle = ", 
                                                  subgroup1, 
                                                  "\nTriangle = ", 
                                                  subgroup2))
  
  ggsave(paste0(output_wd, 
                subdir, 
                subgroup_name,
                estimator,
                dpi = 600,
                "1.tif"), 
         p,
         height = 10,
         width = 8, create.dir = T)
  
  p=p2
  
  p = p+patchwork::plot_annotation(caption=paste0("Circle = ", 
                                                  subgroup1, 
                                                  "\nTriangle = ", 
                                                  subgroup2))
  
  ggsave(paste0(output_wd,
                subdir, 
                subgroup_name,
                estimator,
                dpi = 600,
                "2.tif"), 
         p,
         height = 10,
         width = 8, create.dir = T)
  
  saveRDS(effects_pooled, 
          file = paste0(output_wd,
                        subdir,
                        subgroup_name,
                        estimator,
                        "_data.rds"))
}


## Global variables ####

outcome="ideology"
#outcome="populism"

anonymyzed_path1 = "C:/Users/gscaduto/OneDrive - Tilburg University/Postdoc/SUBMISSIONS/IAFS/IJPOR/data and scripts/"

anonymyzed_path2 = "C:/Users/gscaduto/OneDrive - Tilburg University/Postdoc/SUBMISSIONS/IAFS/IJPOR/new output/"
gdrive_code = ""
clean = T

output_wd = paste0(gdrive_code, anonymyzed_path2,"Clean_", clean, "/bycountry/")

outcome = "ideology"

# Load dataset
if(clean==T)
{
  data = readRDS(paste0(anonymyzed_path1,
                        "/dataset_PR.RDS"))
}
if(clean ==F)
{
  data = readRDS(paste0(anonymyzed_path1,
                        "/dataset_PR_notclean.RDS"))
}

input_pooled_data = paste0(anonymyzed_path2,
                           "Clean_", clean, "/POOLED_with_sd/")


if(outcome=="ideology")
{
  formula_outcome = ccd_chosen_rw ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_profession+
    ccd_consc+ccd_openness+ ccd_neuroticism+
    ccd_restaurant+ccd_transport+ccd_pet
  
  formula_continuous = ccd_continuous ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_profession+
    ccd_consc+ccd_openness+ ccd_neuroticism+
    ccd_restaurant+ccd_transport+ccd_pet
}





## Effects ####
### Main effects ################# 


subdir = "AMCEs/"
leftlim=-0.15
rightlim = 0.15


result = full_analysis_bycountry(data,
                                 formula_outcome,
                                 "amce",
                                 subdir,
                                 leftlim=-0.1,
                                 rightlim = 0.1)

##### Pooled ####

for(attribute in unique(result$effects$feature))
{
  result$plot_pooled[[attribute]] = result$plot_pooled[[attribute]]+#xlab("Marginal Means")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, 
                                          rightlim,
                                          length.out = 9), 
                                      digits=3),
                       labels = label_percent(accuracy = 0.1))+
    theme(legend.position = "none",
          axis.title.y = element_text(size = 14),
          axis.ticks.x = element_blank(),    # Removes x-axis ticks
          axis.text.x = element_blank(),  # remove axis x ticks labels
          #axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)#,
          #axis.text.x = element_text(size = 14)
    )
  
}



plot_pooled_sociodemo = (result$plot_pooled[["Gender"]]+scale_x_continuous(limits = c(-0.35, 0.35), 
                                                                           breaks = round(seq(-0.35, 
                                                                                              0.35,
                                                                                              length.out = 9), 
                                                                                          digits=3),
                                                                           labels = label_percent(accuracy = 0.1))+
                           xlab("")+
                           theme(legend.position = "none",
                                 axis.text.x = element_text(size = 14)))/
  result$plot_pooled[["Age"]]/
  result$plot_pooled[["Citysize"]]/
  result$plot_pooled[["Religion"]]/
  (result$plot_pooled[["Profession"]]+scale_x_continuous(limits = c(leftlim,
                                                                    rightlim),
                                                         breaks = round(seq(leftlim,
                                                                            rightlim,
                                                                            length.out = 9),
                                                                        digits=3),
                                                         labels = label_percent(accuracy = 0.1))+
     xlab("Average Marginal Component Effect")+
     theme(legend.position = "bottom",
           axis.text.x = element_text(size = 14),
           legend.key.size = unit(30, "pt")))


ggsave(paste0(output_wd, subdir, "sociodemo_pooled.tif"),
       plot_pooled_sociodemo,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)
ggsave(paste0(output_wd, subdir, "sociodemo_pooled.png"),
       plot_pooled_sociodemo,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

plot_pooled_psychocultu = result$plot_pooled[["Consc"]]/
  result$plot_pooled[["Openness"]]/
  result$plot_pooled[["Neuroticism"]]/
  result$plot_pooled[["Restaurant"]]/
  result$plot_pooled[["Transport"]]/
  (result$plot_pooled[["Pet"]]+scale_x_continuous(limits = c(leftlim,
                                                             rightlim),
                                                  breaks = round(seq(leftlim,
                                                                     rightlim,
                                                                     length.out = 9),
                                                                 digits=3),
                                                  labels = label_percent(accuracy = 0.1))+
     xlab("Average Marginal Component Effect")+
     theme(legend.position = "bottom",
           axis.text.x = element_text(size = 14),
           legend.key.size = unit(30, "pt")))


ggsave(paste0(output_wd, subdir, "psychocultu_pooled.tif"),
       plot_pooled_psychocultu,
       height = 15,
       width = 10,
       dpi = 600,
       create.dir = T)

ggsave(paste0(output_wd, subdir, "psychocultu_pooled.png"),
       plot_pooled_psychocultu,
       height = 15,
       width = 10,
       dpi = 600,
       create.dir = T)



##### Comparative (MM) ####
subdir = "MMs/"

result = full_analysis_bycountry(data,
                                 formula_outcome,
                                 "mm",
                                 subdir,
                                 leftlim=-0.1,
                                 rightlim = 0.1)

leftlim=0.5-0.12
rightlim = 0.5+0.12

for(attribute in unique(result$effects$feature))
{
  result$plot_comparative[[attribute]] = result$plot_comparative[[attribute]]+#xlab("Marginal Means")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, 
                                          rightlim,
                                          length.out = 9), 
                                      digits=3),
                       labels = label_percent(accuracy = 0.1))+
    theme(legend.position = "none",
          axis.title.y = element_text(size = 14),
          axis.ticks.x = element_blank(),    # Removes x-axis ticks
          axis.text.x = element_blank(),  # remove axis x ticks labels
          #axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)#,
          #axis.text.x = element_text(size = 14)
    )
  
}


plot_comparative_sociodemo = (result$plot_comparative[["Gender"]]+scale_x_continuous(limits = c(0.5-0.4, 0.5+0.4), 
                                                                                     breaks = round(seq(0.5-0.4, 
                                                                                                        0.5+0.4,
                                                                                                        length.out = 9), 
                                                                                                    digits=3),
                                                                                     labels = label_percent(accuracy = 0.1))+
                                xlab("")+
                                theme(legend.position = "none",
                                      axis.text.x = element_text(size = 14)))/
  result$plot_comparative[["Age"]]/
  result$plot_comparative[["Citysize"]]/
  result$plot_comparative[["Religion"]]/
  (result$plot_comparative[["Profession"]]+scale_x_continuous(limits = c(leftlim,
                                                                         rightlim),
                                                              breaks = round(seq(leftlim,
                                                                                 rightlim,
                                                                                 length.out = 9),
                                                                             digits=3),
                                                              labels = label_percent(accuracy = 0.1))+
     xlab("")+
     theme(legend.position = "bottom",
           axis.text.x = element_text(size = 14),
           legend.key.size = unit(30, "pt")))


ggsave(paste0(output_wd, subdir, "sociodemo_comparative.tif"),
       plot_comparative_sociodemo,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

ggsave(paste0(output_wd, subdir, "sociodemo_comparative.png"),
       plot_comparative_sociodemo,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

plot_comparative_psychocultu = result$plot_comparative[["Consc"]]/
  result$plot_comparative[["Openness"]]/
  result$plot_comparative[["Neuroticism"]]/
  result$plot_comparative[["Restaurant"]]/
  result$plot_comparative[["Transport"]]/
  (result$plot_comparative[["Pet"]]+scale_x_continuous(limits = c(leftlim,
                                                                  rightlim),
                                                       breaks = round(seq(leftlim,
                                                                          rightlim,
                                                                          length.out = 9),
                                                                      digits=3),
                                                       labels = label_percent(accuracy = 0.1))+
     xlab("")+
     theme(legend.position = "bottom",
           axis.text.x = element_text(size = 14),
           legend.key.size = unit(30, "pt")))


ggsave(paste0(output_wd, subdir, "psychocultu_comparative.tif"),
       plot_comparative_psychocultu,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)


ggsave(paste0(output_wd, subdir, "psychocultu_comparative.png"),
       plot_comparative_psychocultu,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

##### Comparative (AMCE, for appendix) ####
subdir = "AMCEs/"

result = full_analysis_bycountry(data,
                                 formula_outcome,
                                 "amce",
                                 subdir,
                                 leftlim=-0.15,
                                 rightlim = 0.15)

leftlim=-0.17
rightlim=0.17

for(attribute in unique(result$effects$feature))
{
  result$plot_comparative[[attribute]] = result$plot_comparative[[attribute]]+#xlab("Marginal Means")+
    scale_x_continuous(limits = c(leftlim, rightlim), 
                       breaks = round(seq(leftlim, 
                                          rightlim,
                                          length.out = 9), 
                                      digits=3),
                       labels = label_percent(accuracy = 0.1))+
    theme(legend.position = "none",
          axis.title.y = element_text(size = 14),
          axis.ticks.x = element_blank(),    # Removes x-axis ticks
          axis.text.x = element_blank(),  # remove axis x ticks labels
          #axis.title.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)#,
          #axis.text.x = element_text(size = 14)
    )
  
}


plot_comparative_sociodemo = (result$plot_comparative[["Gender"]]+scale_x_continuous(limits = c(-0.4, +0.4), 
                                                                                     breaks = round(seq(-0.4, 
                                                                                                        +0.4,
                                                                                                        length.out = 9), 
                                                                                                    digits=3),
                                                                                     labels = label_percent(accuracy = 0.1))+
                                xlab("")+
                                theme(legend.position = "none",
                                      axis.text.x = element_text(size = 14)))/
  result$plot_comparative[["Age"]]/
  result$plot_comparative[["Citysize"]]/
  result$plot_comparative[["Religion"]]/
  (result$plot_comparative[["Profession"]]+scale_x_continuous(limits = c(leftlim,
                                                                         rightlim),
                                                              breaks = round(seq(leftlim,
                                                                                 rightlim,
                                                                                 length.out = 9),
                                                                             digits=3),
                                                              labels = label_percent(accuracy = 0.1))+
     xlab("")+
     theme(legend.position = "bottom",
           axis.text.x = element_text(size = 14),
           legend.key.size = unit(30, "pt")))


ggsave(paste0(output_wd, subdir, "sociodemo_comparative.tif"),
       plot_comparative_sociodemo,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

ggsave(paste0(output_wd, subdir, "sociodemo_comparative.png"),
       plot_comparative_sociodemo,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

plot_comparative_psychocultu = result$plot_comparative[["Consc"]]/
  result$plot_comparative[["Openness"]]/
  result$plot_comparative[["Neuroticism"]]/
  result$plot_comparative[["Restaurant"]]/
  result$plot_comparative[["Transport"]]/
  (result$plot_comparative[["Pet"]]+scale_x_continuous(limits = c(leftlim,
                                                                  rightlim),
                                                       breaks = round(seq(leftlim,
                                                                          rightlim,
                                                                          length.out = 9),
                                                                      digits=3),
                                                       labels = label_percent(accuracy = 0.1))+
     xlab("")+
     theme(legend.position = "bottom",
           axis.text.x = element_text(size = 14),
           legend.key.size = unit(30, "pt")))


ggsave(paste0(output_wd, subdir, "psychocultu_comparative.tif"),
       plot_comparative_psychocultu,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)

ggsave(paste0(output_wd, subdir, "psychocultu_comparative.png"),
       plot_comparative_psychocultu,
       height = 12,
       width = 10,
       dpi = 600,
       create.dir = T)



## Importance weights ####

#### Pooled ####


subdir = "Importance weights/"

importance_weights = readRDS(paste0(
  gdrive_code,
  input_pooled_data,
  "importance_weights.rds")
)

importance_weights_pool = importance_weights |>
  filter(country == "POOL")

attribute_order <- importance_weights_pool %>%
  arrange(desc(contribution_R2)) %>%
  pull(Attribute)

importance_weights_pool$Attribute <- factor(importance_weights_pool$Attribute, levels = attribute_order)


# Plot with POOL in the center
p = ggplot(importance_weights_pool, aes(x = Attribute, y = contribution_R2)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "black",
           linewidth = 0.2)+
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.4, by = 0.05)  # Adjust range and step as needed
  ) +
  labs(
    x = "Attributes",
    y = "Contribution to the sum of Pseudo R²"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave(paste0(output_wd, subdir, "Pseudo_R2_pooled.tif"), 
       p, 
       height = 8, 
       width = 8,
       dpi = 600, 
       create.dir = T)

ggsave(paste0(output_wd, subdir, "Pseudo_R2_pooled.png"), 
       p, 
       height = 8, 
       width = 8,
       dpi = 600, 
       create.dir = T)


country_levels <- c("CZ", "FR", "IT", "SW")

importance_weights = readRDS(paste0(
  gdrive_code,
  input_pooled_data,
  "importance_weights.rds")
)


#### Comparative ####


importance_weights_comp = importance_weights |>
  filter(country != "POOL")

# Apply factor levels to country

country_levels <- c("CZ", "FR", "IT", "SW")


importance_weights_comp$country <- factor(importance_weights_comp$country, levels = country_levels)

# Order attributes by POOL values (optional, but consistent)
attribute_order <- importance_weights_comp |>
  arrange(desc(contribution_R2)) %>%
  pull(Attribute)

importance_weights_comp$Attribute <- factor(importance_weights_comp$Attribute, levels = unique(attribute_order))

# Define custom fill colors
country_colors <- c(
  "CZ" = wesanderson::wes_palettes$Darjeeling1[3],
  "FR" = wesanderson::wes_palettes$Darjeeling1[2],
  "IT" = wesanderson::wes_palettes$Darjeeling1[1],
  "SW" = wesanderson::wes_palettes$Darjeeling1[4]
)

# Plot with POOL in the center
p = ggplot(importance_weights_comp, aes(x = Attribute, y = contribution_R2, fill = country)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8), 
           color = "gray20",
           linewidth = 0.2) +
  scale_fill_manual(
    values = country_colors,
    name = "Country",
    limits = country_levels
  ) +
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 0.4, by = 0.05)  # Adjust range and step as needed
  ) +
  labs(
    x = "Attributes",
    y = "Contribution to the sum of Pseudo R²"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y = element_text(size = 11),
    axis.title.x = element_text(size = 11),
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )

ggsave(paste0(output_wd, subdir, "Pseudo_R2_comp.tif"), 
       p, 
       height = 8, 
       width = 8,
       dpi = 600, 
       create.dir = T)

ggsave(paste0(output_wd, subdir, "Pseudo_R2_comp.png"), 
       p, 
       height = 8, 
       width = 8,
       dpi = 600, 
       create.dir = T)


