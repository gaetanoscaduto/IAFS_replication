###############################################################################
# This script is for the randomization checks related to the classic conjoint design
###############################################################################

pacman::p_load(
  cregg, dplyr, ggpubr, cowplot,
  MASS, cjoint, corrplot, dplyr,
  forcats, ggplot2, gt, gtools,
  gtsummary, margins, openxlsx,
  patchwork, rio, texreg, tools
)

clean =T



anonymyzed_path1 = "C:/Users/100722gsc/OneDrive - Erasmus University Rotterdam/Postdoc/SUBMISSIONS/IAFS/IJPOR/data and scripts/"
anonymyzed_path2 = "C:/Users/100722gsc/OneDrive - Erasmus University Rotterdam/Postdoc/SUBMISSIONS/IAFS/IJPOR/new output/"

dataset_rep = anonymyzed_path1

output_wd = paste0(anonymyzed_path2,
                   "Clean_", clean, "/Randomization Checks/")

outcome = "ideology"

if (!dir.exists(output_wd)) {
  dir.create(output_wd, recursive = TRUE)
}

# Load dataset
if (clean == T) {
  data = readRDS(paste0(anonymyzed_path1, "/dataset_PR.RDS"))
}
if (clean == F) {
  data = readRDS(paste0(anonymyzed_path1, "/dataset_PR_notclean.RDS"))
}

if(clean == T)
{
  data1= readRDS(paste0(anonymyzed_path1, "/dataset_PR_notclean.RDS"))
  data1 = data1[data1$time_diff_mins>(1/2*median(data1$time_diff_mins)), ]
  data1 = data1[data1$time_diff_mins<(4*median(data1$time_diff_mins)), ]
  data1 = data1[data1$attention_check2 == "eucomm", ]
  data1$att_order = data1$C3_ATT_ORDER 
}
#context = "IT"
#context = "FR"
#context = "CZ"
context = "SW"

data1 = data1[data1$country == context, ]
data = data1 

output_wd = paste0(anonymyzed_path2,
                   "Clean_", clean, "/Randomization Checks/", context, "/")

if (outcome == "ideology") {
  data$ccd_outcome = data$ccd_chosen_rw
}

if (outcome == "populism") {
  data$ccd_outcome = data$ccd_populism
}

# data$rowid= paste0(data$respid, "_", data$ccd_task_number, "_", data$ccd_profile_number)
# data1 = merge(data, att1, by="rowid")
#### Randomization check ####

names(data)
data2 = cbind(data[c(1:18)], data["C3_ATT_ORDER"])

names(data2) = c("respid", "country", "Ntask", "Nprofile",
                 "Gender", "Age", "Religion", "Citysize", "Profession",
                 "Consc", "Openness", "Neuroticism", "Restaurant",
                 "Transport", "Pet", "chosen", "continuous", "populism", "att_order")

plot(cj_freqs(data2, chosen ~ Gender + Age + Religion + Citysize +
                Profession + Consc +
                Openness + Neuroticism + Restaurant +
                Transport + Pet,
              id = ~respid), col = "grey")

ggsave(paste0(output_wd, "diagnostic_randomization.png"),
       height = 15, width = 8, create.dir = T)


###################
#### DIAGNOSTICS ####
###################

plot(cj_freqs(data, ccd_outcome ~ ccd_gender +
                ccd_age + ccd_religion + ccd_citysize + ccd_profession +
                ccd_consc + ccd_openness + ccd_neuroticism +
                ccd_restaurant + ccd_transport + ccd_pet,
              feature_labels = list(ccd_gender = "Gender",
                                    ccd_age = "Age",
                                    ccd_religion = "Religion",
                                    ccd_citysize = "City size",
                                    ccd_profession = "Profession",
                                    ccd_consc = "Conscientiousness",
                                    ccd_openness = "Openness",
                                    ccd_neuroticism = "Neuroticism",
                                    ccd_restaurant = "Favorite Restaurant",
                                    ccd_transport = "Mean of transportation",
                                    ccd_pet = "Pet"),
              id = ~respid), col = "grey") +
  theme(legend.position = "none")

ggsave(paste0(output_wd, "diagnostic_randomization.png"),
       height = 14, width = 10, create.dir = T)


### Attribute order effects conjoint ###

# Filter out pilot respondents
data=data1[data1$att_order != "",]
# Parse attribute order variable
remove_left_square  = gsub("\\[", "", data$att_order)
remove_right_square = gsub("\\]", "", remove_left_square)
positions_list      = strsplit(remove_right_square, "-")

# Create null variables for the position
number_of_attributes = 11

for (i in 1:number_of_attributes) {
  data[, paste0("attribute_", i, "_position")] = NA
}

for(attribute_number in 1:number_of_attributes)
{
  for(i in 1:nrow(data))
  {
    data[i, paste0("attribute_",attribute_number,"_position")] = which(positions_list[[i]]==attribute_number)
  }
}
# Then check how many rows were flagged
for (i in 1:number_of_attributes) {
  col = paste0("attribute_", i, "_position")
  cat(col, "— NAs:", sum(is.na(data[[col]])), "\n")
}
# Make the attribute position variables factors
for (i in 1:number_of_attributes) {
  data[, paste0("attribute_", i, "_position")] =
    factor(data[, paste0("attribute_", i, "_position")],
           levels = c("1", "2", "3", "4", "5", "6",
                      "7", "8", "9", "10", "11"))
}


# data1 = data[data$Gender != "Non-Binary", ]
# 
# data1$ccd_gender = factor(data1$ccd_gender,
#                           levels = c("Female", "Male"))


# ── Shared note text ──────────────────────────────────────────────────────────
# Describes what the attribute-order plots show, used as a caption in patchwork.
attr_order_note = paste0(
  "Note: Each panel shows marginal means (MMs) for one conjoint attribute ",
  "estimated separately by the position in which that attribute appeared in the profile ",
  "\n(1 = top, 11 = bottom). Overlapping confidence intervals across positions indicate ",
  "the absence of attribute-order effects.\nError bars represent 99% confidence intervals.\n",
  "The outcome variable is the probability of choosing a profile."
)

# ── Legend title used across all attribute-order plots ────────────────────────
legend_title_position = "Attribute\nposition"


# ── Individual attribute-order panels ─────────────────────────────────────────

data$ccd_outcome = data$ccd_chosen_rw
data1 = data[data$ccd_gender != "Non-Binary", ]

data1$ccd_outcome = data1$ccd_chosen_rw

data1$ccd_gender = factor(data1$ccd_gender,
                          levels = c("Female", "Male"))

p1 = plot(cregg::cj(data1, ccd_outcome ~ ccd_gender,
                    id = ~respid,
                    by = ~attribute_1_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_gender = "Gender")),
          group = "attribute_1_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

p2 = plot(cregg::cj(data, ccd_outcome ~ ccd_age,
                    id = ~respid,
                    by = ~attribute_2_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_age = "Age")),
          group = "attribute_2_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

p3 = plot(cregg::cj(data, ccd_outcome ~ ccd_religion,
                    id = ~respid,
                    by = ~attribute_3_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_religion = "Religion")),
          group = "attribute_3_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

p4 = plot(cregg::cj(data, ccd_outcome ~ ccd_citysize,
                    id = ~respid,
                    by = ~attribute_4_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_citysize = "City size")),
          group = "attribute_4_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

p5 = plot(cregg::cj(data, ccd_outcome ~ ccd_profession,
                    id = ~respid,
                    by = ~attribute_5_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_profession = "Profession")),
          group = "attribute_5_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

# p6 retains the legend and receives the proper legend title
p6 = plot(cregg::cj(data, ccd_outcome ~ ccd_consc,
                    id = ~respid,
                    by = ~attribute_6_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_consc = "Conscientiousness")),
          group = "attribute_6_position") +
  xlab("") +
  theme_gray() +
  theme(text          = element_text(size = 10),
        legend.title  = element_text(size = 9),
        legend.position = "right") +
  guides(colour = guide_legend(title = legend_title_position),
         shape  = guide_legend(title = legend_title_position))

p7 = plot(cregg::cj(data, ccd_outcome ~ ccd_openness,
                    id = ~respid,
                    by = ~attribute_7_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_openness = "Openness")),
          group = "attribute_7_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

p8 = plot(cregg::cj(data, ccd_outcome ~ ccd_neuroticism,
                    id = ~respid,
                    by = ~attribute_8_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_neuroticism = "Neuroticism")),
          group = "attribute_8_position") +
  theme_gray() +
  theme(legend.position = "none")

p9 = plot(cregg::cj(data, ccd_outcome ~ ccd_restaurant,
                    id = ~respid,
                    by = ~attribute_9_position,
                    estimate = "mm",
                    alpha = 0.01,
                    feature_labels = list(ccd_restaurant = "Restaurant")),
          group = "attribute_9_position") +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

# NOTE (potential bug): the original script used `attribute_9_position` for
# both p10 and p11. These have been corrected to `attribute_10_position` and
# `attribute_11_position` respectively. Please verify this matches your
# intended analysis before using the output.

p10 = plot(cregg::cj(data, ccd_outcome ~ ccd_transport,
                     id = ~respid,
                     by = ~attribute_10_position,   # was attribute_9_position in original
                     estimate = "mm",
                     alpha = 0.01,
                     feature_labels = list(ccd_transport = "Transport")),
           group = "attribute_10_position") +       # was attribute_9_position in original
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")

# p11 retains the legend and receives the proper legend title
p11 = plot(cregg::cj(data, ccd_outcome ~ ccd_pet,
                     id = ~respid,
                     by = ~attribute_11_position,   # was attribute_9_position in original
                     estimate = "mm",
                     alpha = 0.01,
                     feature_labels = list(ccd_pet = "Pet")),
           group = "attribute_11_position") +       # was attribute_9_position in original
  xlab("") +
  theme_gray() +
  theme(text          = element_text(size = 10),
        legend.title  = element_text(size = 9),
        legend.position = "right") +
  guides(colour = guide_legend(title = legend_title_position),
         shape  = guide_legend(title = legend_title_position))


# ── Compose and save Figure 1 (attributes 1–6) ────────────────────────────────
fig1 = ((p1 | p2) / (p3 | p4) / (p5 | p6)) +
  plot_annotation(
    caption = paste0(
      "99% C.I.  |  ",
      attr_order_note
    ),
    theme = theme(
      plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 6))
    )
  )

ggsave(paste0(output_wd, "attribute_order_check1.png"),
       fig1, height = 10, width = 8, create.dir = T)


# ── Compose and save Figure 2 (attributes 7–11) ───────────────────────────────
fig2 = ((p7 | p8) / (p9 | p10) / p11) +
  plot_annotation(
    caption = paste0(
      "99% C.I.  |  ",
      attr_order_note
    ),
    theme = theme(
      plot.caption = element_text(size = 7, hjust = 0, margin = margin(t = 6))
    )
  )

ggsave(paste0(output_wd, "attribute_order_check2.png"),
       fig2, height = 10, width = 8, create.dir = T)

