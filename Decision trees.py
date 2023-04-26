# Step 1: Import necessary libraries
import os  # To work with file paths
import nibabel as nib  # To load NIfTI format files
import pandas as pd  # To load and work with CSV files
import numpy as np  # To work with arrays and numerical data
from sklearn.model_selection import train_test_split  # To split the data into training and testing sets
from sklearn.tree import DecisionTreeClassifier  # To create and train the decision tree model
from sklearn.metrics import accuracy_score, confusion_matrix  # To evaluate the performance of the model
from graphviz import Source  # To visualize the decision tree
from nilearn.image import resample_img  # To resample the images to a common voxel size

# Step 2: Load the data
# Set the path to the folder containing the NIfTI images
data_folder = "C:/Users/Juan A. Arias/Desktop/TFM/PETmasked"
nifti_data = []
# Loop through all files in the folder and load the ones with the .hdr extension
for filename in os.listdir(data_folder):
    if filename.endswith(".hdr"):
        nifti_file = os.path.join(data_folder, filename)
        nifti_data.append(nib.load(nifti_file).get_fdata())
# Concatenate the image data into a single array along the last axis
nifti_array = np.concatenate(nifti_data, axis=-1)
# Load the demographic data from the CSV file and merge it with the image data
demographic_data = pd.read_csv("C:/Users/Juan A. Arias/Desktop/TFM/Demographics.csv")
merged_data = pd.merge(pd.DataFrame(nifti_array.reshape(-1, nifti_array.shape[-1])), demographic_data, on="patient_id")

# Step 3: Split the data
# Separate the features (image and demographic data) from the target variable (diagnosis)
X = merged_data.drop(["diagnosis"], axis=1)
y = merged_data["diagnosis"]
# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Step 4: Train the decision tree
# Create a decision tree model with a maximum depth of 5
decision_tree = DecisionTreeClassifier(max_depth=5, random_state=42)
# Fit the model to the training data
decision_tree.fit(X_train, y_train)

# Step 5: Evaluate the decision tree
# Use the model to predict the diagnoses of the test data
y_pred = decision_tree.predict(X_test)
# Calculate the accuracy of the model
accuracy = accuracy_score(y_test, y_pred)
# Calculate the confusion matrix of the model
confusion_mat = confusion_matrix(y_test, y_pred)
# Print the accuracy and confusion matrix
print("Accuracy:", accuracy)
print("Confusion Matrix:")
print(confusion_mat)

# Step 6: Visualize the decision tree
# Create a visualization of the decision tree using Graphviz
graph = Source(tree.export_graphviz(decision_tree, out_file=None, class_names=["Healthy", "Alzheimer's"], filled=True))
# Save the visualization as a PNG file
graph.format = "png"
graph.render("decision_tree")
