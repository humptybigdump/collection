#TensorFlow implementation deduced from
#https://www.tensorflow.org/tutorials/quickstart/beginner

import tensorflow as tf
from tensorflow.keras import layers, models
import matplotlib.pyplot as plt
import numpy as np

# **************************************************************************
#(i) Load data sets for training and evaluation

#Obtain data from MNIST database
mnist = tf.keras.datasets.mnist
(x_train, y_train), (x_test, y_test) = mnist.load_data()
#Scale the pixel values from the interval [0,255] to the interval [0,1]
x_train, x_test = x_train / 255.0, x_test / 255.0

#Plot selected digits
num = 10
images = x_train[:num]
labels = y_train[:num]
num_row = 2
num_col = 5
fig, axes = plt.subplots(num_row, num_col, figsize=(1.5*num_col,2*num_row))
for i in range(num):
    ax = axes[i//num_col, i%num_col]
    ax.imshow(images[i], cmap='gray')
    ax.set_title('Label: {}'.format(labels[i]))

plt.tight_layout()
plt.savefig("nn_mnist.pdf", format="pdf", bbox_inches="tight")
plt.show(block=False)

# **************************************************************************
#(ii) Define neural network model and topology

#Define a feedforward architecture
model = models.Sequential([
    #flatten data
    layers.Flatten(input_shape=(28, 28)),
    #dense implements the operation:
    #      output = activation(dot(input, kernel) + bias)
    #where activation is the element-wise activation function passed as the
    #activation argument, kernel is a weights matrix created by the layer,
    #and bias is a bias vector created by the layer
    layers.Dense(128, activation='relu'),
    #dropout layer randomly sets input units to 0 with a frequency of rate
    #at each step during training time, which helps prevent overfitting
    layers.Dropout(0.2),
    #dense with linear activation function 
    layers.Dense(10, activation='softmax')
])

model.summary()

#Analyze model:
#For each example, the model returns a vector of logits or log-odds scores,
#one for each class. The term 'logits' refers to the following: The vector
#of raw (non-normalized) predictions that a classification model generates,
#which is ordinarily then passed to a normalization function. If the model
#is solving a multi-class classification problem, logits typically become
#an input to the softmax function. The softmax function then generates a
#vector of (normalized)probabilities with one value for each possible class.
predictions = model(x_train[:1]).numpy()
print("Predictions=",predictions)

# **************************************************************************
#(iii) Define loss function and initialize the optimization algorithm

#Define and parametrize optimizer:
#Before training, configure and compile the model using Keras Model.compile.
#The optimizer class is set to adam, the loss is set to a suitable loss
#function, and a metric to be evaluated for the model by setting the metrics
#parameter to accuracy is defined.
model.compile(optimizer='adam',
              loss='sparse_categorical_crossentropy',
              metrics=['accuracy'])

#Loss function:
#The error between two probability distributions can be computed  using the
#cross-entropy loss function. The cross entropy measures the difference
#between the discovered probability distribution of a classification model
#and the predicted values. 

# **************************************************************************
#(iv) Perform the training based on the training data set

#Variable 'epochs' refers to the number of passes a training dataset takes
#around an algorithm
model.fit(x_train, y_train, epochs=5)

# **************************************************************************
#(v) Evaluate the trained neural network based on the evaluation data set

#Evaluate using test data from MNIST database
test_loss, test_acc = model.evaluate(x_test, y_test, verbose=2)
print('\nTest accuracy:', test_acc)

#Generate predictions (probabilities, output layer) from the set of
#test data using 'predict'
pred   = model.predict(x_test[:num])
labels_pred = np.argmax(pred,1)
images = x_test[:num]
labels_exact = y_test[:num]
fig, axes = plt.subplots(num_row, num_col, figsize=(1.5*num_col,2*num_row))
for i in range(num):
    ax = axes[i//num_col, i%num_col]
    ax.imshow(images[i], cmap='gray')
    ax.set_title('{}'.format(labels_pred[i])+\
                 ' ({:.0f} %) / '.format(pred[i,labels_pred[i]]*100)+\
                 '{}'.format(labels_exact[i]))

plt.tight_layout()
plt.savefig("nn_mnist_tf_predict.pdf", format="pdf", bbox_inches="tight")
#plt.show(block=False)
plt.show()
