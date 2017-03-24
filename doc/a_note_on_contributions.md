### A Note on Contributions

Project 3

Team members:+ Ruxue Peng, Raphaël Ruscassie, Yifei Tang, Chengcen Zhou, Hongyi Zhu

Summary: In this project, we explored different methods for feature extraction and classification. After trying out things, we choose lasso for dimension reduction and the multilayer perceptron classifier, which belongs to the family of neural network.  
We are able to achieve a classification accuracy for images of poodle dogs and fried chicken of 90%. 

[Contribution Statement]   

(1)Data preprocessing: Raphaël, Hongyi, Ruxue  
(2)Feature Selection: Raphaël, Hongyi, Ruxue, Chengcen  
(3)Finding Classifier: Ruxue, Yifei  
  Detail: baseline, gb trees, bagging, random forest, neural network code in both R and python: Ruxue  
  RBF SVM, linear SVM: Yifei  
  Tuning of the above: Yifei and Ruxue  
(4)rPython related: Hongyi  
(5)Github organization, writing readme: Hongyi,Yifei,Ruxue  

Detail:
Ruxue tuned a gradient boosting tree and a random forest in Python. She also wrote iPython notebooks on bagging, neural network, tuning, and a linear support vector machine. Raphaël trimmed the 5000 given features per image down to roughly 400 through LASSO coefficients. Yifei wrote the files for the two classifiers in R: MLP and SVM. Connie helped develop new features with OpenCV but unfortunately it didn't work out and we didn't have time to implement. Hongyi Zhu worked on rPython, which is a piece of garbage, and it didn't work out. Hongyi Zhu also helped organize this github repo and synthesized results from all the team members into the following file: main.rmd, main.r, train.r, test.r, feature_selection.r.   

All team members approve our work presented in our GitHub repository including this contribution statement.  
It was a tough ride, but we made it.  
