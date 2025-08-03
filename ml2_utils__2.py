import numpy as np

############
### Data
### https://sknadig.dev/TensorFlow2.0-dataset/
### https://github.com/xuwd11/cs294-112_hws/blob/master/hw4/utils.py
############
import numpy as np
import tensorflow as tf 


class Dataset(object):

    def __init__(self):
        self._states = []
        self._actions = []
        self._next_states = []
        self._rewards = []
        self._dones = []

    @property
    def is_empty(self):
        return len(self) == 0

    def __len__(self):
        return len(self._states)

    ##################
    ### Statistics ###
    ##################

    @property
    def state_mean(self):
        return np.mean(self._states, axis=0)

    @property
    def state_std(self):
        return np.std(self._states, axis=0)

    @property
    def action_mean(self):
        return np.mean(self._actions, axis=0)

    @property
    def action_std(self):
        return np.std(self._actions, axis=0)

    @property
    def delta_state_mean(self):
        return np.mean(np.array(self._next_states) - np.array(self._states), axis=0)

    @property
    def delta_state_std(self):
        return np.std(np.array(self._next_states) - np.array(self._states), axis=0)

    ###################
    ### Adding data ###
    ###################

    def add(self, state, action, next_state, reward, done):
        """
        Add (s, a, r, s') to this dataset
        """
        if not self.is_empty:
            # ensure the state, action, next_state are of the same dimension
            assert len(self._states[-1]) == len(np.ravel(state))
            assert len(self._actions[-1]) == len(np.ravel(action))
            assert len(self._next_states[-1]) == len(np.ravel(next_state))

        self._states.append(np.ravel(state))
        self._actions.append(np.ravel(action))
        self._next_states.append(np.ravel(next_state))
        self._rewards.append(reward)
        self._dones.append(done)

    def append(self, other_dataset):
        """
        Append other_dataset to this dataset
        """
        if not self.is_empty and not other_dataset.is_empty:
            # ensure the state, action, next_state are of the same dimension
            assert len(self._states[-1]) == len(other_dataset._states[-1])
            assert len(self._actions[-1]) == len(other_dataset._actions[-1])
            assert len(self._next_states[-1]) == len(other_dataset._next_states[-1])

        self._states += other_dataset._states
        self._actions += other_dataset._actions
        self._next_states += other_dataset._next_states
        self._rewards += other_dataset._rewards
        self._dones += other_dataset._dones

    ############################
    ### Iterate through data ###
    ############################

    def rollout_iterator(self):
        """
        Iterate through all the rollouts in the dataset sequentially
        """
        end_indices = np.nonzero(self._dones)[0] + 1

        states = np.asarray(self._states)
        actions = np.asarray(self._actions)
        next_states = np.asarray(self._next_states)
        rewards = np.asarray(self._rewards)
        dones = np.asarray(self._dones)

        start_idx = 0
        for end_idx in end_indices:
            indices = np.arange(start_idx, end_idx)
            yield states[indices], actions[indices], next_states[indices], rewards[indices], dones[indices]
            start_idx = end_idx
    
    
    def random_iterator(self):
        """
        Iterate once through all (s, a, r, s') in batches in a random order
        """
        self.iterator = iter(self.dataset)
        (states,actions,next_states,rewards,dones) = iterator.get_next()
        yield states,actions,next_states,rewards,dones

    
    def list_2_np(self):
        """
        Iterate once through all (s, a, r, s') in batches in a random order
        """
        states = np.asarray(self._states)
        actions = np.asarray(self._actions)
        next_states = np.asarray(self._next_states)
        rewards = np.asarray(self._rewards)
        dones = np.asarray(self._dones)
        return states, actions, rewards, next_states, dones

        
def normalize(x, mean, std, eps=1e-8):
    return (x - mean) / (std + eps)

def unnormalize(x, mean, std):
    return x * std + mean