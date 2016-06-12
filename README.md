# Quatitative analysis report of air quality of Clinton, Gladstone QLD with R.

1. **Introduction:**
  + Why do we care about this analysis? Motivate the study.
  + What is the question we're answering with our analysis?

2. **Methods and materials:**
 + Conceptual model: how do the variables fit together?
 + Quantitative model: what is the functional form of the regression model(s) being investigated? (at this point there should be no parameter values reported, because we haven't even seen the data yet!)
 + Data: What are the data? How was it collected? Where's it from?

3. **Analysis:**
 + Exploratory analysis: What does the data look like? Numerical summaries. Graphical summaries.
 + Quant analysis: What are the results from fitting the model(s)?
 + Model choice: Which model does the best job? Let's talk about it.
 + Parameter summaries: Estimates, confidence intervals and hypothesis tests
 + Model checking: are the residuals normal and homogeneous?

4. **Interpretation:**
 + What do our results mean in terms of the question of interest? Use the results from the analysis section to back up your claims.
 + Are there any problems with our models? How would we fix them if we could?
 + What have others done in the same field? Do our results lead to the same conclusions?

5. **References:** What resources did you use to help justify claims that don't arise from your analysis? Do you need to cite an article/book/R package for where your data came from? All references listed must have been used somewhere in the text.

6. **Appendix:** Where you put the stuff that's not the main story of the article but might help clarify things such as the iterative process of model fitting, or a review of other studies that deal with the same topic.


**Attach your R script**. It needs to contain only the code used to generate the analysis you've done. Don't include broken code or code for figures that you didn't end up using.

### Note
**Analysis section**
**Model Comparison**
 + If you're fitting multiple models, you want to do the model comparison (ANOVA, comparing R2). A table of R2 values or a nicely formatted version of the ANOVA output can be a useful way of summarising the model comparison.

 + Decide which model is doing the best job of explaining the variability in your data.

 + The CSA is based around data analysis to answer a particular question, and so your analysis section should feature statements about the 
    + parameter estimates
    + confidence intervals
    + etc.

Do not just copy and paste the results of summary() into your article. Look at the way parameter estimates and CIs are represented in weeks 11 and 12, in a table format. 

If you feel the need to include R output in your CSA, make sure it's neatly formatted and placed in your appendix. You should then refer to that appendix section in the main text of your CSA. 

Every figure and table needs a brief caption explaining what it is and some discussion in the text that describes to the reader what it means in terms of your data analysis and scientific question of interest.