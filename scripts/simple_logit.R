################################################################################
## [ PROJ ] < College Board Geomarket HS recruiting visits>
## [ FILE ] < simple_logit.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/8/2025
## [ DESC ] < logistic regression of probability that school i gets visit, separate model for each college>
################################################################################

### SETTINGS
rm(list = ls())
options(max.print=1000)
#options(width = 160)

####### RUN SCRIPT TO CREATE OBJECTS FOR ANALYSES


getwd()
source(file = file.path('scripts', 'create_cb_geo_hs_visits.R'))
getwd()

# remove objects from cb_geo (ajs manuscript) mapping
rm(create_rq1_map,format_vars,get_palette)

############

# START BUILDING LOGISTIC REGRESSION MODEL; CAN START WITH CODE FROM eda.R

# create test data frame
pubprivhs_univ_df <- pubprivhs_univ_df %>% 
  # string variables should be changed to factor variables. make this change upstream
  mutate(
    hs_state_code = factor(hs_state_code) %>% relevel(ref = "CT"), # make CT the reference group state
    hs_control = factor(hs_control,
                        levels = c("public", "private"))
  )

df <- pubprivhs_univ_df %>% filter(univ_id == 128902)

df %>% count(visit01) %>% mutate(pct_visit = n/sum(n, na.rm= TRUE))

mod_null <- glm(visit01 ~ 1, 
                data = df, 
                family = binomial)

## NULL MODEL
summary(mod_null)

# To interpret: convert log-odds intercept to probability
pvisit_null <- plogis(coef(mod_null)[1])
pvisit_null

# interpretation:
  #Here’s how to read what you just got:
  
  # Intercept (log-odds) = −3.833
  # Probability = plogis(-3.833) ≈ 0.02118 → about 2.1% of high schools in your sample got a visit from Connecticut College in 2017.
  # Because there are no predictors, this model just encodes that baseline probability in log-odds form.
  # In other words: “If I know nothing about the high school except that it’s in the set Connecticut College could have visited, the model says there’s about a 2.1% chance it got a visit.”

df %>% select(hs_state_code) %>% glimpse()
df %>% count(hs_state_code) %>% print(n=55)

# START ADDING XVARS
mod1 <- glm(
  visit01 ~ hs_control + hs_state_code,
  data = df,
  family = binomial
)

mod1 %>% summary()

# Interpretation:

  # 1. Interpret the intercept:
  # The intercept is the log-odds of visit01 = 1 when all predictors are at their reference categories, that is when state = CT and school control = public.
    coef(mod1)["(Intercept)"] # this is what we get from the summary() table
    # Convert log-odds to odds:
    exp(coef(mod1)["(Intercept)"])
    #(Intercept) 
    #0.1395702     
    # This means: when hs_control = "public" (reference category) AND hs_state_code = "CT" (reference category),
    # the odds of visit01 = 1 (i.e., a visit) are about 0.14 to 1.
    # In other words, for every 1 high school that *is* visited, there are about 7.17 (1 / 0.1396) high schools that are *not* visited.    
  
  # Convert log-odds to probability:
    plogis(coef(mod1)["(Intercept)"])
    # plogis(coef(mod1)["(Intercept)"]) = 0.1224762
    # This means: when hs_control = "public" and hs_state_code = "CT", the predicted probability of a visit is about 12.25%.
    
  
  # 2. Interpret the coefficient on hs_controlprivate:
  # The coefficient is the difference in log-odds between private and public schools, holding state constant.
    coef(mod1)["hs_controlprivate"]    
    # coef(mod1)["hs_controlprivate"] = 1.898513
    # This means: compared to public high schools (reference group), private high schools in the same state have an increase of about 1.90 in the log-odds of receiving a visit.
    
  # Convert the log-odds difference to an odds ratio (private vs public):
    exp(coef(mod1)["hs_controlprivate"])
    # exp(coef(mod1)["hs_controlprivate"]) = 6.675959
    # This means: compared to public high schools (reference group), private high schools have odds of receiving a visit that are about 6.68 times higher, holding state constant.
    
  # seems like calculating marginal effect for coef(mod1)["hs_controlprivate"] is more complicated
    # why this is not correct: 
      # plogis(coef(mod1)["hs_controlprivate"])
      # plogis(coef(mod1)["hs_controlprivate"]) just takes the log-odds difference for being private vs. public, pretends that’s the whole linear predictor, and converts it to a probability.
      # It ignores the intercept and other terms in your model, so it’s not the marginal effect — it’s the probability that would result if the only thing in the model was that coefficient, starting from zero log-odds.
    # correct approach conceptually:
    # Marginal effect (in a logit model) = change in predicted probability when a predictor changes, holding other variables constant.
      # That requires two probability predictions:
      # One with the predictor at the baseline (e.g., public)
      # One with the predictor switched (e.g., private)
      # Then you subtract: P(private) – P(public)    
    
    # If you want the true marginal effect for hs_controlprivate in your model, the correct approach is:
    
    new_data <- data.frame(
      hs_control = c("public", "private"),
      hs_state_code = "CT" # or any fixed reference state
    )
    new_data
    
    pred_probs <- predict(mod1, new_data, type = "response")
    pred_probs
    marginal_effect <- pred_probs[2] - pred_probs[1]
    
    pred_probs
    marginal_effect
    
    #That marginal_effect is the actual change in probability of a visit when moving from public → private, holding state constant.
    # pred_probs = c(0.1224762, 0.4823387)
    # Interpretation: Holding state constant (here fixed at CT), the model predicts that public high schools
    # have a 12.25% probability of receiving a visit, while private high schools have a 48.23% probability.
    
    # marginal_effect = 0.3598625
    # Interpretation: Moving from a public to a private high school increases the predicted probability
    # of receiving a visit by about 35.99 percentage points, holding state constant.
    
    
    # 3. Get predicted probabilities for each category:
    # Sometimes easier to explain than log-odds or odds ratios.
    new_data <- data.frame(
      hs_control = c("public", "private"),
      hs_state_code = "CT" # or whichever reference state
    )
    predict(mod1, new_data, type = "response") # returns predicted visit probability for each group
    
    
#NEXT STEPS, MONDAY 8/11. KEEP ON ADDING X VARIABLES TO YOUR LOGIT MODEL;
    # START ASKING GPT5 HOW TO FIND AND INTERPRET THE PERCENT OF VARIATION EXPLAINED BY THE MODEL [AND BY SPECIFIC VARIABLES]