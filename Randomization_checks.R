###############################################################################
#this script is for the randomization checks related to the classic conjoint design
###############################################################################
# 
pacman::p_load(
  cregg, dplyr, ggpubr, cowplot,
  MASS, cjoint, corrplot, dplyr,
  forcats, ggplot2, gt, gtools,
  gtsummary, margins, openxlsx,
  patchwork, rio, texreg, tools
)

clean = T

anonymyzed_path1 = "C:/Users/100722gsc/OneDrive - Erasmus University Rotterdam/Postdoc/SUBMISSIONS/IAFS/IJPOR/data and scripts/"
anonymyzed_path2 = "C:/Users/100722gsc/OneDrive - Erasmus University Rotterdam/Postdoc/SUBMISSIONS/IAFS/IJPOR/new output/"

dataset_rep = anonymyzed_path1

output_wd = paste0(anonymyzed_path2,
                   "Clean_", clean, "/Randomization Checks/")

outcome="ideology"

if (!dir.exists(output_wd)) {
  dir.create(output_wd, recursive = TRUE)
}
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

if(outcome=="ideology")
{
  data$ccd_outcome = data$ccd_chosen_rw
}

if(outcome=="populism")
{
  data$ccd_outcome = data$ccd_populism
}

#### Randomization check 


names(data)
data1 = data[c(1:18)]

names(data1) = c("respid", "country", "Ntask", "Nprofile",
                 "Gender", "Age", "Religion", "Citysize", "Profession",
                 "Consc", "Openness", "Neuroticism", "Restaurant",
                 "Transport", "Pet", "chosen", "continuous", "populism")

plot(cj_freqs(data1, chosen ~ Gender + Age + Religion + Citysize +
                Profession + Consc +
                Openness + Neuroticism + Restaurant+
                Transport + Pet,
              id = ~respid), col="grey")

ggsave(paste0(output_wd,  "diagnostic_randomization.png"), 
       height = 15, width = 8, create.dir = T)

# plot(cj_freqs(data, ccd_chosen ~ ccd_gender + ccd_age + ccd_religion + ccd_citysize +
#                 ccd_profession + ccd_consc +
#                 ccd_openness + ccd_neuroticism + ccd_restaurant+
#                 ccd_transport + ccd_pet,
#               id = ~respid), col="grey")


###################
#### DIAGNOSTICS ####
###################


plot(cj_freqs(data, ccd_outcome ~ ccd_gender+
                ccd_age+ccd_religion+ccd_citysize+ccd_profession+
                ccd_consc+ccd_openness+ ccd_neuroticism+
                ccd_restaurant+ccd_transport+ccd_pet, 
              feature_labels = list(ccd_gender="Gender",
                                      ccd_age="Age",
                                    ccd_religion="Religion",
                                    ccd_citysize="City size",
                                    ccd_profession= "Profession",
                                    ccd_consc= "Conscientiousness",
                                    ccd_openness="Openness",
                                    ccd_neuroticism="Neuroticism",
                                    ccd_restaurant="Favorite Restaurant",
                                    ccd_transport="Mean of transportation",
                                    ccd_pet="Pet"),
              id = ~respid), col="grey")+theme(legend.position = "none")


ggsave(paste0(output_wd, "diagnostic_randomization.png"), 
       height = 14, width = 10, create.dir = T)


# # With ggplot
# aus = cj_freqs(data, ccd_outcome ~ ccd_gender+
#                  ccd_age+ccd_religion+ccd_citysize+ccd_profession+
#                  ccd_consc+ccd_openness+ ccd_neuroticism+
#                  ccd_restaurant+ccd_transport+ccd_pet,
#                id = ~respid)
# 
# v = list()
# 
# for(i in unique(aus$feature))
# {
#   
#   p = aus |>
#     filter(feature == i) |>
#     ggplot(aes(y=level, x=estimate, fill=feature))+
#     geom_col()+
#     ylab("")+
#     xlab("")+
#     ggtitle(as.character(i))+
#     theme(text = element_text(size = 15),
#           legend.position = "none",
#           plot.title = element_text(size=14))
#   
#   v[[i]] = p
# }
# 
# p = v[[1]]/v[[2]]/v[[3]]/v[[4]]/v[[5]]/v[[6]]
# 
# p
# 
# ggsave(paste0(output_wd,  "diagnostic_randomization_ggplot1.png"),
#        p, height = 12, width = 8, create.dir = T)
# 
# p= v[[7]]/v[[8]]/v[[9]]/v[[10]]/v[[11]]
# 
# p
# 
# ggsave(paste0(output_wd,  "diagnostic_randomization_ggplot2.png"),
#        p, height = 12, width = 8, create.dir = T)


#### Checking whether there is a preference for the profile shown to the right
# 
# data$ccd_profile_number = as.factor(data$ccd_profile_number)
# 
# data$ccd_outcome = factor(data$ccd_outcome)
# 

# ### checking it with a different approach: if the effect is significant, 
# # it means that  the effect of a certain attribute varies by whether a
# #profile is on the left or on the right
# 
# #DA RICONTROLLARE
#  
# data$ccd_profile_number = as.numeric(data$ccd_profile_number)
# 
# plot(cj(data, 
#         ccd_outcome ~ ccd_profile_number,
#         id = ~respid,
#         estimate = "amce"),
#      vline = 0)



### Attribute order effects conjoint



remove_left_square = gsub("\\[", "", data$C3_ATT_ORDER)
remove_right_square = gsub("\\]", "", remove_left_square)

positions_list = strsplit(remove_right_square, "-")

# create null variables for the position

number_of_attributes = 11

for(i in 1:number_of_attributes)
{
  data[,  paste0("attribute_",i,"_position")] = NA
}

for(attribute_number in 1:number_of_attributes)
{
  for(i in 1:nrow(data))
  {
    data[i, paste0("attribute_",attribute_number,"_position")] = which(positions_list[[i]]==attribute_number)
  }
}

#make the attribute position variables factors
for(i in 1:number_of_attributes)
{
  data[,  paste0("attribute_",i,"_position")] = factor(data[,  paste0("attribute_",i,"_position")],
                                                       levels = c("1","2","3",
                                                                  "4","5","6",
                                                                  "7","8","9",
                                                                  "10", "11"))
}



data1 = data[data$ccd_gender != "Non-Binary", ]

data1$ccd_gender = factor(data1$ccd_gender,
                          levels = c("Female", "Male"))



p1 = plot(cregg::cj(data1, ccd_outcome ~ ccd_gender, 
                    id = ~respid,
                    by = ~attribute_1_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_gender = "Gender")), 
          group = "attribute_1_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_gender == "Female", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_gender == "Male", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p2 = plot(cregg::cj(data, ccd_outcome ~ ccd_age, 
                    id = ~respid,
                    by = ~attribute_2_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_age = "Age")), 
          group = "attribute_2_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_age == "25 Years Old", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_age == "45 Years Old", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_age == "65 Years Old", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p3 = plot(cregg::cj(data, ccd_outcome ~ ccd_religion, 
                    id = ~respid,
                    by = ~attribute_3_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_religion = "Religion")), 
          group = "attribute_3_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_religion == "Non Believer", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_religion == "Non Practitioner", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  # geom_vline(aes(xintercept = mean(data[data$ccd_religion == "Practitioner", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p4 = plot(cregg::cj(data, ccd_outcome ~ ccd_citysize, 
                    id = ~respid,
                    by = ~attribute_4_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_citysize = "City size")), 
          group = "attribute_4_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_citysize == "Big", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_citysize == "Medium", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  # geom_vline(aes(xintercept = mean(data[data$ccd_citysize == "Small", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p5 = plot(cregg::cj(data, ccd_outcome ~ ccd_profession, 
                    id = ~respid,
                    by = ~attribute_5_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_profession = "Profession")), 
          group = "attribute_5_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_profession == "Entrepreneur", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_profession == "Lawyer", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  # geom_vline(aes(xintercept = mean(data[data$ccd_profession == "Waiter", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_profession == "Teacher", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p6 = plot(cregg::cj(data, ccd_outcome ~ ccd_consc, 
                    id = ~respid,
                    by = ~attribute_6_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_consc = "Conscientiousness")), 
          group = "attribute_6_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_consc == "Disorganized", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_consc == "Reliable", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(text = element_text(size=10),
        legend.title = element_blank(),
        legend.position = "right")

p7 = plot(cregg::cj(data, ccd_outcome ~ ccd_openness, 
                    id = ~respid,
                    by = ~attribute_7_position,
                    estimate = "mm",    
                    alpha=0.01,
                    feature_labels = list(ccd_openness = "Openness")), 
          group = "attribute_7_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_openness == "Open", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_openness == "Rigid", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p8 = plot(cregg::cj(data, ccd_outcome ~ ccd_neuroticism, 
                    id = ~respid,
                    by = ~attribute_8_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_neuroticism = "Neuroticism")), 
          group = "attribute_8_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_neuroticism == "Anxious", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_neuroticism == "Calm", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  theme_gray()+
  theme(legend.position = "none")

p9 = plot(cregg::cj(data, ccd_outcome ~ ccd_restaurant, 
                    id = ~respid,
                    by = ~attribute_9_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_restaurant = "Restaurant")), 
          group = "attribute_9_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_restaurant == "Asian", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_restaurant == "Vegan", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_restaurant == "Traditional", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  # geom_vline(aes(xintercept = mean(data[data$ccd_restaurant == "Steakhouse", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")

p10 = plot(cregg::cj(data, ccd_outcome ~ ccd_transport, 
                    id = ~respid,
                    by = ~attribute_10_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_transport = "Transport")), 
          group = "attribute_9_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_transport == "Bicycle", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_transport == "SUV", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ geom_vline(aes(xintercept = mean(data[data$ccd_transport == "Public Transport", ]$ccd_outcome)),
  #                                    col="black",
  #                                    alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(legend.position = "none")


p11 = plot(cregg::cj(data, ccd_outcome ~ ccd_pet, 
                    id = ~respid,
                    by = ~attribute_11_position,
                    estimate = "mm",
                    alpha=0.01,
                    feature_labels = list(ccd_pet = "Pet")), 
          group = "attribute_9_position")+
  # geom_vline(aes(xintercept = mean(data[data$ccd_pet == "Cat Large", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_pet == "Dog", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  # geom_vline(aes(xintercept = mean(data[data$ccd_pet == "No Pets", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+
  # geom_vline(aes(xintercept = mean(data[data$ccd_pet == "Small Dog", ]$ccd_outcome)),
  #            col="black",
  #            alpha= 1/4)+ 
  xlab("")+
  theme_gray()+
  theme(text = element_text(size=10),
        legend.title = element_blank(),
        legend.position = "right")


p=((p1|p2)/(p3|p4)/(p5|p6))+plot_annotation(caption=paste0("99% C.I."))

ggsave(paste0(output_wd,  "attribute_order_check1.png"),
       p, height = 10, width = 8, create.dir = T)

p=((p7|p8)/(p9|p10)/p11)+plot_annotation(caption=paste0("99% C.I."))

ggsave(paste0(output_wd,  "attribute_order_check2.png"),
       p, height = 10, width = 8, create.dir = T)

