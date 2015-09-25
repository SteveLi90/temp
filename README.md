# temp
Read the download file.
Merge the training and the test sets.
Extract only the mean and standard deviation.
Use descriptive activity names.
Label with descriptive activity names.
Create a tidy data set.

variables:
dtSubjectTrain  store the data from subject_train.txt
dtSubjectTest   store the data from subject_test.txt
dtActivityTrain store the data from Y_train.txt
dtActivityTest  store the data from Y_test.txt

fileToDataTable modify the data in the file to the variable

dt              data table
dtFeatures      named the data table
dtActivityNames set the activity name
dtTidy          the result tidy data
