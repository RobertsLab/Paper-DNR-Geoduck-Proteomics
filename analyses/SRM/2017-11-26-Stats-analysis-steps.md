SRM Protein & Environmental Data Analysis Steps  

Each Protein:  (assume proteins are independent)  
    1. Test for normality  
    2. Lambda transformation  
    3. Test for normality post transformation  
    4. Assess outliers, remove if necessary
    5. N-way ANOVA by: a) location b) habitat c) site d) region  
    6. Determine P-adjusted, correct for multiple comparisons (P/13)  
    7. Post-hoc test (name?) to ID differences  
    8. Ultimate goal: which proteins are different between locations?  
    9. Compare total abundance between sites (sum peptide abundance)  

Each environmental variable:  
    1. Normality of each env. variable (all time points)  
    2. Determine P-adjusted, correct for multiple comparisons (P/4)  
    3. N-way ANOVA for each env. variable by location  
    4. Post-hoc test (name?) to ID differences  
    5. Ultimate goal: which env. variables are different between locations?   

Prep for regression model:  
    1. Mean, median, variance, % above & below 1 (or 2) st. dev from mean  
    2. Plot() all env. variables- are any linearly related, aka not independent? If so, need to include interaction parameter in regression model.  
    3. Plot() protein peptides against each other to confirm linear correlation; equation should be ~1:1.  
    4. If all correlated select 1 peptide to use in regression model; highest abundance is best.  

Run regression models for each representative peptide:  
    1. Step-wise linear regression models with all env. variables; I would expect that only the variables that were found to be different via the ANOVA would significantly contribute to the model  
    2. General linear model  with variables ID'd in step-wise lm
    3. Figure out when to add a constant, and if I should do that in this scenario  
    4. Run anova on best fit model, find P-value of the env. variables to determine confidence in the influence of each env. variable on proteins.  
    5. Run model on the other peptides in the protein (not used as representative peptides); ID the R^2 and P values  
     