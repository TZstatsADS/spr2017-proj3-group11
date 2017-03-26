# Project: Labradoodle or Fried Chicken? 

### Code lib Folder

### Reproduction  

To retrain the baseline, you can use the trunk in main.rmd in the doc folder directly.  For further info, check out the baseline folder here.  

To reproduce the data preprocess for MLP classifier and the SVM classifier, please go to feature_selection.R 

To use processed data directly, please go to the data folder.  

(For data preprocess for MLP, you'll need to call the function feature_selection_mlp(); for data preprocess for SVM, you'll need to call the function feature_selection_svm())  

To retrain the MLP and linear SVM classifier, please go to train.R  

To Cross Validate the MLP, please go to CV.R

To test your data on the classfier, please go to in_class_test.Rmd  

### Examine the code for classifier in both R and Python

To examine the code for MLP classifier alone, please go to Neural_Network_Classifier folder here:  
MLP-classifier.R for R coder, go to Neural Network.ipynb for python coder.  

To examine the code for linear SVM classifier alone, please go to SVM_Classifier folder here:  
SVM train.R for R coder, go to linear_svm.ipynb for python coder.  

### To find out other classifiers we tried:

Passive Regressive Classifier:  PassiveRegressiveClassifer.ipynb  

Random Forest: RF.R
Tuning Random Forest Parameter: RF_TUNE.R

Gradient Boosting Tree: gb_tree.R, gbtree_tuning_script.R for R coder, Untuned_gradient_boosting_tree.py and Tuned_GBTree.ipynb for python coder.  

bagging: bagging.ipynb

The Tuning structure we used in general: tuning_structure.ipynb


