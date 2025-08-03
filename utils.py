import numpy as np
from timeit import default_timer as timer
import torch
from torch.autograd import Variable
import torch.nn as nn
import torch.nn.functional as F

def sample_train(train_dataset, test_dataset, batch_size, k, n_classes,
                 seed, shuffle_train=True, return_idxs=True):
    
    n = len(train_dataset)
    rrng = np.random.RandomState(seed)
    
    cpt = 0
    indices = torch.zeros(k)
    other = torch.zeros(n - k)
    card = k // n_classes
    
    for i in range(n_classes):
        class_items = (train_dataset.train_labels == i).nonzero()
        n_class = len(class_items)
        rd = np.random.permutation(np.arange(n_class))
        indices[i * card: (i + 1) * card] = class_items[rd[:card],0]
        other[cpt: cpt + n_class - card] = class_items[rd[card:],0]
        cpt += n_class - card

    other = other.long()
    train_dataset.train_labels[other] = -1

    train_loader = torch.utils.data.DataLoader(dataset=train_dataset, 
                                               batch_size=batch_size,
                                               num_workers=4,
                                               shuffle=shuffle_train)
    test_loader = torch.utils.data.DataLoader(dataset=test_dataset, 
                                              batch_size=batch_size,
                                              num_workers=4,
                                              shuffle=False)
    
    if return_idxs:
        return train_loader, test_loader, indices 
    return train_loader, test_loader


def ramp_up(epoch, max_epochs, max_val, mult):
    if epoch == 0:
        return 0.
    elif epoch >= max_epochs:
        return max_val
    return max_val * np.exp(mult * (1. - float(epoch) / max_epochs) ** 2)


def weight_schedule(epoch, max_epochs, max_val, mult, n_labeled, n_samples):
    max_val = max_val * (float(n_labeled) / n_samples)
    return ramp_up(epoch, max_epochs, max_val, mult)
