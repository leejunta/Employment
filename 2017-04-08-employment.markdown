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
Depression, decreasing employment by 8.8 million in just 14 months^1^. 
The US has since dropped its overall unemployment rate to 4.5% in March,
2017^2^. Knowing what factors 
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
employment was low, we merged those reponses with the `Employed` factor. In the 
analysis of this dataset, *we use the term `Unemployed` loosely to imply lack of
employment instead of the technical definition of unemployment*. The final 
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
technology are strong predictors of employment. We analyze more closely how some
of the variables of highest importance are related to employment.  

### Accomodations for persons with disability remain insufficient in the US

Approximately a quarter (24.2%) of people in the weighted data identified as
being unemployed. However, 71% of people with disabilities were unemployed.
In contrast, only 17% of people without any disabilities were unemployed. Only
13% of people in the survey had disabilities, but they comprised of almost 40% of the
unemployed group.  
![Disability Mosaic][fig:disa]  
Title I of the Americans with Disabilities Act Amendments Act (ADAAA), the
updated version of the Americans with Disabilities Act, requires 
employers to provide reasonable accomodations to qualified applicants or
employees, with the exception of companies with fewer than 15 employees^3^. The data 
suggests that either accomodations are insufficient, employers are not complying
with this law, or people with disabilities are unqualified for most available
jobs. I am willing to bet that Stephen Hawking with disagree with the last
consideration. Furthermore, based on the income distribution (this feature was
not included in the Random Forest), people with disabilities consistently
represent a larger proportion of the lower income class and smaller proportion of the
higher income class.  
![Disability Income][fig:disinc]  
According to the US Census Bureau, nearly 1 in 5 people in the US have a
disability. Surely, better accomodations and/or stronger regulations to provide
accomodations for people with disability are necessary. The fact that disability
status is the strongest indicator of employment status in our model (or any
resonable model, for that matter) is highly problematic.

### Gender roles pertaining to parenting may drive women out of the workforce

The current maternity leave policy in the US, the Family and Medical Leave Act, 
(FMLA) provides most US employees with upto 12 weeks of unpaid maternal leave^4^.
In contrast, in 2015, Slovak Republic offer up to 160 week of paid maternity or
paternity leave^5^. Among the 35 Organisation for Economic Co-operation and 
Development (OECD) countries, the US is the only country with no paid maternity
leave. Exploring employment status by sex and age, we find that the only
deviation of pattern for all factors is the relative increase in unemployment
for women between ages 26 to 30 while unemployment for men in the same age group
decreased.  
![Age-Sex Distribution][fig:agesex]  
In 2014, the average age of mothers at first birth was 26.3 years^6^, where 
women's unemployment rate increases in
our data. This could suggest that women's drop in employment may be attributed
to the lack of support for maternity leave. Exploring how unemployment differs
between men and women who are or are not parents, the men and women who are not
parents share similar unemployment distributions.  
![Parental Unemployment][fig:paremp]  
In contrast, mothers have an unemployment distribution more concentrated from ages 25 
to 50, a common age group to care for children. While we lack sufficient
information to make stronger claims about child care needs and employment, we
suspect that providing paid maternity and/or paternity leave would increase
mothers' employment rate.

### Technology used for employment purposes affect higher income jobs more than lower income

In additional to technological advances contributing to the US's record-low 
unemployment rates since the 2008 Financial Crisis, those who use technology and
the internet for assistance for job searches tend to have higher salaries. While
the income distribution for those who do not use technology for job searches is
skewed to the right, the distribution for those who is skewed to the left.  
![Job Search Income][fig:jobinc]
We suspect that the ability to use the internet for job searches is closely 
associated with having more technical skills. Since jobs with more technical
skills tend to have higher wages, this result is not especially astonishing.

## Demographics are still better predictors of employment than technology

While technology seems to have some indication of employment status and income
level, demographic information still predicts employment status more robustly.
Given the abundance of information in the survey about different topics,
further research can reveal relationships between features not discussed here.  

## References

^1^ Bureau of Labor Statistics (Apr 2017). *Labor Force Statistics from the Current Population Survey*. Retrieved from:  
https://data.bls.gov/timeseries/LNS14000000  

^2^ Bureau of Labor Statistics (Apr 2011). *Employment loss and the 2007–09
recession: an overview*. Retrieved from:  
https://www.bls.gov/mlr/2011/04/art1full.pdf  

^3^ ADA National Network. *What is the Americans with Disabilities Act (ADA)?*.
Retrieved from:  
https://adata.org/learn-about-ada  

^4^ United Census Bureau (Jul 2012). *Nearly 1 in 5 People Have a Disability in the U.S., Census Bureau Reports*. Retrieved from:  
https://www.census.gov/newsroom/releases/archives/miscellaneous/cb12-134.html  

^5^ OECD (Apr 2016). *PF2.5. Trends in parental leave policies since 1970*.
Retrieved from:  
http://www.oecd.org/els/family/PF2_5_Trends_in_leave_entitlements_around_childbirth.pdf  

^6^ Centers for Disease Control and Prevention (Jan 2016). *Mean Age of Mothers is on the Rise: United States, 2000–2014*. Retrieved from:  
https://www.cdc.gov/nchs/products/databriefs/db232.htm

[fig:varimp]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/varimp.png
[fig:disa]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/disability.png
[fig:disinc]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/income.png
[fig:agesex]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/ageempsex.png
[fig:paremp]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/parentalemp.png
[fig:jobinc]: https://raw.githubusercontent.com/leejunta/Employment/master/figures/incomeint.png
