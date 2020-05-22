# Step 1 - Matching
mod_match_exact <- matchit(foundfam_owned ~ 
                             degree_nm + degree_nm_sq + factor(compet_moder) + 
                             factor(compet_strong) + factor(industry) + factor(countrycode), 
                           data = data2, method = 'exact')

summary(mod_match_exact)

# Step 2 - restrict data to matched 
data_match_exact <- match.data(mod_match_exact)
dim(data_match_exact)

# Step 3 - Estimate treatment effects (6356 matched)
reg_match_exact <- lm(management ~ foundfam_owned + degree_nm + degree_nm_sq + 
                        factor(compet_moder) + factor(compet_strong) + 
                        factor(industry) + factor(countrycode), 
                      data = data_match_exact)

summary(reg_match_exact)

