#PyTorch implementation deduced from
#https://deepdatamininglearning.readthedocs.io/en/latest/notebooks/\
#CMPE_pytorch4_2024Fall.html
#https://tripathisonam.medium.com/identifying-hand-written-digits-mnist\
#-using-pytorch-f9ded0cacc

import torch
import torch.nn as nn
import torch.optim as optim
import torch.nn.functional as F
import torchvision
import torchvision.transforms as transforms
import matplotlib.pyplot as plt
import numpy as np

# Hyperparameter
batch_size = 64
learning_rate = 0.005
epochs = 10

# **************************************************************************
#(i) Load data sets for training and evaluation

#Obtain data from MNIST database
transform = transforms.Compose([
    transforms.ToTensor(),
    transforms.Normalize((0.5,), (0.5,))
])
train_dataset = torchvision.datasets.MNIST(root='./data', train=True,\
          download=True, transform=transform)
train_loader = torch.utils.data.DataLoader(dataset=train_dataset,\
          batch_size=batch_size, shuffle=True)
test_dataset = torchvision.datasets.MNIST(root='./data', train=False,\
          download=True, transform=transform)
test_loader = torch.utils.data.DataLoader(dataset=test_dataset,\
          batch_size=batch_size, shuffle=False)

# **************************************************************************
#(ii) Define neural network model and topology

class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        self.fc1 = nn.Linear(28*28, 128)
        self.fc2 = nn.Linear(128, 10)
        self.dropout = nn.Dropout(0.2)
       
    def forward(self, x):
        x = x.view(-1, 28*28)
        x = torch.relu(self.fc1(x))
        x = self.dropout(x)
        x = self.fc2(x)
        return x

net = Net()

# **************************************************************************
#(iii) Define loss function and initialize the optimization algorithm

criterion = nn.CrossEntropyLoss()
optimizer = optim.SGD(net.parameters(), lr=learning_rate)

#Loss function:
#The error between two probability distributions can be computed  using the
#cross-entropy loss function. The cross entropy measures the difference
#between the discovered probability distribution of a classification model
#and the predicted values. 

# **************************************************************************
#(iv) Perform the training based on the training data set

#Variable 'epochs' refers to the number of passes a training dataset takes
#around an algorithm
for epoch in range(epochs):
    for images, labels in train_loader:
        optimizer.zero_grad()
        outputs = net(images)
        loss = criterion(outputs, labels)
        loss.backward()
        optimizer.step()
    print(f'Epoch [{epoch+1}/{epochs}], Loss: {loss.item():.4f}')

# **************************************************************************
#(v) Evaluate the trained neural network based on the evaluation data set

#Evaluate using test data from MNIST database    
net.eval()
with torch.no_grad():
    correct = 0
    total = 0
    for images, labels in test_loader:
        outputs = net(images)
        _, predicted = torch.max(outputs.data, 1)
        total += labels.size(0)
        correct += (predicted == labels).sum().item()
   
print(f'Accuracy of the network on the 10000 test images:\
       {100 * correct / total} %')

#Generate predictions (probabilities, output layer) from the set of
#test data using 'predict'
num = 10
num_row = 2
num_col = 5
fig, axes = plt.subplots(num_row, num_col, figsize=(1.5*num_col,2*num_row))
for i in range(num):
    image = test_loader.dataset[i][0]
    label_exact = test_loader.dataset[i][1]
    prob, label_pred = torch.max(F.softmax(net(image),dim=1),1)
    #Use softmax to transform output to probability
    ax = axes[i//num_col, i%num_col]
    ax.imshow(image[0].numpy(), cmap='gray')
    ax.set_title('{}'.format(label_pred[0].tolist())+\
                 ' ({:.0f} %) / '.format(prob[0].tolist()*100)+\
                 '{}'.format(label_exact))

plt.tight_layout()
plt.savefig("nn_mnist_torch_predict.pdf", format="pdf", bbox_inches="tight")
plt.show()
#plt.show(block=False)
