# Split Infra dataset ------------------------------------------------------------------------------
infra_groups <- read_excel("input/Infra_question_groups.xlsx", sheet = "HER_INFRA_Updated")

infra_groups <- infra_groups %>% 
  mutate(name=str_squish(name),
         select_type = str_replace_all(type, " .*", ""))  

## Test --------------------------------------------------------------------------------------------
dataset_cols <- rbind(
  data.frame(Sheet="Data", Questions=names(infra_data)),
  data.frame(Sheet="Reps", Questions=names(infra_rep)),
  data.frame(Sheet="Docs", Questions=names(infra_doc)),
  data.frame(Sheet="Env", Questions=names(infra_env)),
  data.frame(Sheet="Features", Questions=names(infra_feat)),
  data.frame(Sheet="Elements", Questions=names(infra_elem))
)
# Check if all Dataset columns are in the file
dataset_cols[dataset_cols$Questions %notin% infra_groups$name & 
             # Excep series columns
             !grepl("_[1-9]", dataset_cols$Questions) & 
             # Except backend dataset questions
               dataset_cols$Questions %notin% c( "KEY", "PARENT_KEY"), ]

# Check if all the the file columns are in dataset
infra_groups$name[infra_groups$name %notin% dataset_cols$Questions]

## Filter HF -------------------------------------------------------------------------------
infra_HF_cols <- infra_groups %>% filter(category %in% c("HF", "CTO"))
# Get select_multiple series columns
HF_SM_cols <- infra_HF_cols %>% filter(select_type == "select_multiple") %>% 
  pull(name)
HF_SM_cols <- dataset_cols %>% 
  filter(grepl(paste0(HF_SM_cols, "_", collapse = "|"), Questions)) %>% pull(Questions)

# Get all columns from dataset to preserve order
infra_HF_cols <- dataset_cols$Questions[dataset_cols$Questions %in% c(infra_HF_cols$name, HF_SM_cols, "KEY", "PARENT_KEY")]

infra_HF_HEF_cols <- infra_groups %>% filter(category %in% c("HER & HF", "CTO")) %>% pull(name)

## Filter HF Dataset
infra_data_HF <- infra_data %>% 
  select(any_of(infra_HF_cols))
# # No data
# infra_rep_HF <- infra_rep %>%
#   select(any_of(infra_HF_cols))
# infra_doc_HF <- infra_doc %>% 
#   select(any_of(infra_HF_cols))
# infra_env_HF <- infra_env %>% 
#   select(any_of(infra_HF_cols))
# infra_feat_HF <- infra_feat %>% 
#   select(any_of(infra_HF_cols))
# infra_elem_HF <- infra_elem %>% 
#   select(any_of(infra_HF_cols))

## Filter HER & HF -------------------------------------------------------------------------------
infra_HER_HF_cols <- infra_groups %>% filter(category %in% c("HER & HF", "CTO"))
# Get select_multiple series columns
HER_HF_SM_cols <- infra_HER_HF_cols %>% filter(select_type == "select_multiple") %>% 
  pull(name)
HER_HF_SM_cols <- dataset_cols %>% 
  filter(grepl(paste0(HER_HF_SM_cols, "_", collapse = "|"), Questions)) %>% pull(Questions)

# Get all columns from dataset to preserve order
infra_HER_HF_cols <- dataset_cols$Questions[dataset_cols$Questions %in% c(infra_HER_HF_cols$name, HER_HF_SM_cols, "KEY", "PARENT_KEY")]


## Filter HF Dataset
infra_data_HER_HF <- infra_data %>% 
  select(any_of(infra_HER_HF_cols))
infra_rep_HER_HF <- infra_rep %>% 
  select(any_of(infra_HER_HF_cols))
infra_doc_HER_HF <- infra_doc %>% 
  select(any_of(infra_HER_HF_cols))
infra_env_HER_HF <- infra_env %>% 
  select(any_of(infra_HER_HF_cols))
infra_feat_HER_HF <- infra_feat %>% 
  select(any_of(infra_HER_HF_cols))
infra_elem_HER_HF <- infra_elem %>% 
  select(any_of(infra_HER_HF_cols))


## Tests ------------------------
# name infra not in HF or HER & HF

# ++

## Remove 
rm(dataset_cols, infra_groups)