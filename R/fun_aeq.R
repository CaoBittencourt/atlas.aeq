# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# # CRAN packages
# chr_pkg <- c(
#   'devtools'
# )
# 
# # Git packages
# chr_git <- c(
#   'CaoBittencourt' = 'atlas.gene',
#   'CaoBittencourt' = 'atlas.eqvl'
# )
# 
# # genevate / install CRAN packages
# lapply(
#   chr_pkg
#   , function(pkg){
# 
#     if(!require(pkg, character.only = T)){
# 
#       install.packages(pkg)
# 
#     }
# 
#     require(pkg, character.only = T)
# 
#   }
# )
# 
# # genevate / install Git packages
# Map(
#   function(git, profile){
# 
#     if(!require(git, character.only = T)){
# 
#       install_github(
#         paste0(profile, '/', git)
#         , upgrade = F
#         , force = T
#       )
# 
#     }
# 
#     require(git, character.only = T)
# 
#   }
#   , git = chr_git
#   , profile = names(chr_git)
# )
# 
# rm(chr_pkg, chr_git)

# [FUNCTIONS] ---------------------------
# - Attribute equivalence function ---------------------------------------------
fun_aeq_indispensability <- function(
    dbl_profile
    , dbl_scale_lb = 0
    , dbl_generality = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be a numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_generality' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generality)
        , is.null(dbl_generality)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  if(is.null(dbl_generality)){
    
    fun_gene_generality(
      dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generality
    
  }
  
  dbl_generality[[1]] -> dbl_generality
  
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  # Equivalence of normalized scores
  if(
    max(dbl_profile) != 
    dbl_scale_lb
  ){
    
    fun_eqvl_equivalence(
      dbl_var =
        dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
      , dbl_scale_ub =
        max(dbl_profile)
      , dbl_scaling =
        1 - dbl_generality
    ) -> dbl_attribute_eq
    
  } else {
    
    rep(
      1, length(dbl_profile)
    ) -> dbl_attribute_eq
    
  }
  
  if(is.matrix(dbl_profile)){
    
    colnames(dbl_profile) ->
      names(dbl_attribute_eq)
    
  } else {
    
    names(dbl_profile) ->
      names(dbl_attribute_eq)
    
  }
  
  # Output
  return(dbl_attribute_eq)
  
}

# # [TEST] ------------------------------------------------------------------
# # - fun_aeq_indispensability -------------------------------------------------
# fun_aeq_aequivalence(
#   dbl_profile =
#     rnorm(50, 50, 25) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
# ) |> round(4)
