---
title: "Is Technology Essential for Employment?"
layout: post
date: 2017-04-09
image: /assets/images/markdown.jpg
headerImage: false
tag:
- Employment
- Technology
category: blog
author: juntaeklee
description: Technology in Employment
# jemoji: '<img class="emoji" title=":ramen:" alt=":ramen:" src="https://assets.raw.githubusercontent.com/images/icons/emoji/unicode/1f35c.png" height="20" width="20" align="absmiddle">'
---

Last November, two of my peers, Noah Sebek and Yiran Xu, and I wrote a paper called
*Predicting Employment through Internet, Social Media, and Gaming Activity:
Comparing CART and Random Forests* about the performance of CART and Random
Forests in the context of predicting employment status of US residents through
survey data. Specifically, we hypothesized that with the growing use of
technology in employment applications, attitudes about technology would strongly
predict employment status. As the project unfolded, however, we placed a strong
emphasis on the machine learning aspect of the problem instead of letting
the data guide us--in retrospect, this makes me uncomfortable. As a result, I
decided to write a new version of this paper. While the conclusions of the new
analysis are similar, the analysis more rigorous in my opinion. This is my first
post in my effort to work on independent data analysis projects more often, so
I hope you enjoy!  

# Is Technology Essential for Employment? Predicting Employment through Internet, Social Media, and Gaming Activity  

The 2008 Financial Crisis was the worst financial crisis since The Great
Depression, decreasing employment by 8.8 million in just 14 months (BLS,
2012). The US has since dropped its overall unemployment rate to 4.5% in March,
2017 (Bureau of Labor). Knowing what factors 
decrease unemployment rate helps the government enact social policies more
effectively. Given the increased use of the internet for job postings and
applications, the resurgence of the employment rate can be partially
attributed to the development of technological resources and use by
employers. A natural conclusion is that those who are more comfortable with 
technology can benefit more greatly from these resources than those who are 
not. We use the [Pew Research Center's Gaming, Jobs, and Broadband](http://www.pewinternet.org/datasets/june-10-july-12-2015-gaming-jobs-and-broadband/) survey data to 
examine whether this conclusion holds true or not.  

## Cleaning the Data  

In the survey, 2001 people answered 140 questions about their demographics, 
personal information, and attitudes about gaming, internet, and technology. The
data-cleaning process mainly involved merging conditional questions and removing
variables correlated or associated with employment status. We reduced the 
variable for employment status to `Employed` or `Unemployed` by removing students
and those have have already retired. Since the frequency of underemployment/part-time
employment was low, we merged those reponses with the `Employed` factor. The final 
dataset consisted of 1395 observations and 45 features.  

## Building Random Forests  

We trained a *Random Forest* model with a 10-fold repeated cross validation,
repeated 3 times, on a randomly sample of 80% of the data and 43 non-id features.
The model produced 84.96% accuracy on the training data, and more importantly
83.73% accuracy on the testing data (area under the ROC curve: 0.8069). The
model's variable importance showed that the features with the largest weights
were `Disability`, `Age`, `Sex`, `Education`, `Internet Usage for Job Search`,
`Parental Status`, and `Marital Status`.  

![Variable Importance][fig:varimp]  
The results are inconsistent with our hypothesis that attitudes and activity of 
technology are strong predictors of employment. We analyze more closely how the 
variables of highest importance are related to employment.  

### Accessibility for persons with disability remains insufficient in the US

![Disability Mosaic][fig:disa]

[fig:varimp]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/varimp.png
[fig:disa]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/disability.png
