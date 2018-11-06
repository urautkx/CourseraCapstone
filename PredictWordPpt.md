PredictNextWord - An App that finishes your sentence..
========================================================



author: Kamal S. Rautela

date: 4/30/2018



What is the PredictNextWord app about? 
========================================================
- PredictNextWord app provides suggestions of words that may follow when you type a phrase

- You can write a phrase of one or more words. e.g. If you type "How are you", the app suggests "doing" 
    
- In order for the app to be a "success", it should be fairly accurate and at the same time quite responsive

- I experimented with training sample size, use of 4 or 5-grams or not, use of database vs. no database, and use of dataframe vs. data tables in order to achive optimum accuracy and response time


An inside look 
==============================================================
- App uses Stupid Backoff Natural Language Processing Model.It has been trained on a set of sample texts randomly extracted from: Blogs, News and Twitter feeds

- The samples were split into 1,2,3,4 and 5 Ngrams (sets of 1,2,4,5 & 5 words).The input phrase is run against these ngrams in order to determine the possible matches and their respective frequencies 

- The top 1 to 3 most frequent words that may follow the input phrase are then shown as suggestions. In case, it does not find any match, it shows the three most commonly occuring words in the training sample. Those words are: the, to & and. 

- The ngrams are stored in an Amazon RDS database. The app fetches the ngrams from RDS database at the launch of the app

  
How good the model is? 
========================================================
- We have benchmarked the app using a benchmarking program written by a third party*

- 14482 prediction test cases created from 599 lines 

- Overall top-3 precision was 18.31%

- Average response time was 201.41 msec i.e. 1/5th of a sec

- Use of pre-prepared ngrams stored in RDS database improves launch time of Shiny app

- Use of Data Tables helps reduce app's response time

- Removing ngrams with low frequency improves launch time and response time

<small>* https://github.com/jan-san/dsci-benchmark</small>


Easy to use...but quite effective.  
========================================================
![alt text](ShinyappCapture.png)

You can access the application online using the link below:

https://urautkx.shinyapps.io/NLPProcessing/

Instructions to use:
1. Enter text in the text box on the left
2. Press Enter or click Submit
3. See the results on the right side 

I hope you like it!Please send feedbacks to: urautkx@gmail.com
