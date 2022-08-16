# Is Technological Proficiency Essential for Employment? Technology vs. Demographics in Employment

The summary of this analysis can be found in the [markdown file here](2017-04-08-employment.markdown).

This project is an extension of a previous project that used the same data set for submission to the 2016 USCLAP Statistics Competition. The paper, however, was focused on machine learning and model selection. We wanted to redo the project by focusing on the data instead, which led to this project.  

## Cleaning the data  
Since the cleaning process is very messy, it is hard to describe each step here. Please refer to the `cleaning.R` file for all of the data-cleaning. The process mainly involved removing conditional survey questions with too many nonresponses and combining conditional survey questions with the questions they were contingent on to maximize information per question.  

## Imputation of data  
The only variable that needed data imputing was age. There were less than 1% of observations with missing/refused to answer/don't know responses. We imputed these observations using KNN imputation.  

## Model  
We ran 5 different models:
* CART (rpart)  
* Random Forests  
* Logistic Regression with Penalty  
* Bayesian Generalized Linear Model  
* Gradient Descent Boosting  

Other than the `CART` model, which performed extremely poorly, all the other models performed similarly in terms of accuracy and AUC. The `Random Forests` performed marginally better than the other models, which is what we used to further analysis. The variable importance plots were also very similar for each model.  

Instead of using our model to explain different variables, we let the model guide the variables of importance to explore the data for further speculation. Since `Random Forests` lose some interpretability by randomly selecting samples of features, we expected manually exploring the data to be more satisfying anyways.  

## Results  
You can find the results on my website:  
<s>http://juntaeklee.com/employment/<s>

## Acknowledgments  
Special thank you to Noah Sebek '17 and Yiran Xu '17 for working on the original project with me!

**Enjoy!**
