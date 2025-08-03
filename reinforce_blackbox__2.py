import numpy as np
import torch

class Policy:
    def pi(self, s_t):
        '''
        returns the probability distribution over actions
        (torch.distributions.Distribution)

        s_t (np.ndarray): the current state
        '''
        raise NotImplementedError

    def act(self, s_t):
        '''
        s_t (np.ndarray): the current state
        Because of environment vectorization, this will produce
        E actions where E is the number of parallel environments.
        '''
        a_t = self.pi(s_t).rsample((s_t.shape[0],))
        return a_t

    def learn(self, states, actions, returns):
        '''
        states (np.ndarray): the list of states encountered during
                             rollout
        actions (np.ndarray): the list of actions encountered during
                              rollout
        returns (np.ndarray): the list of returns encountered during
                              rollout

        Because of environment vectorization, each of these has first
        two dimensions TxE where T is the number of time steps in the
        rollout and E is the number of parallel environments.
        '''
        actions = torch.tensor(actions)
        returns = torch.tensor(returns)

        log_prob = self.pi(states).log_prob(actions)
        loss = torch.mean(-log_prob*returns)
        self.opt.zero_grad()
        loss.backward()
        self.opt.step()


class DiagonalGaussianPolicyOnlyActionLogSigma(Policy):
    def __init__(self, actionSpace, mu, sigma):
        '''
        env (gym.Env): the environment
        lr (float): learning rate
        '''

        self.M = actionSpace
        self.mu = torch.tensor(mu, requires_grad=True)
        self.log_sigma = torch.tensor(np.log(sigma), requires_grad=True)

    def get_mean(self):
        return self.mu.detach().numpy()

    def get_std(self):
        return np.exp(self.log_sigma.detach().numpy())

    def get_parameters(self):
        return [self.mu] + [self.log_sigma]

    def pi(self, s_t):
        '''
        returns the probability distribution over actions
        s_t (np.ndarray): the current state
        '''

        mu = self.mu
        log_sigma = torch.clip(self.log_sigma, min = -10)
        sigma = torch.exp(log_sigma)

        pi = torch.distributions.MultivariateNormal(mu, torch.diag(sigma))
        return pi

    def get_gradient(self):
        return np.hstack([self.mu.grad.detach().numpy(), self.log_sigma.grad.detach().numpy()])


class DiagonalGaussianPolicyOnlyActionSigma(Policy):
    def __init__(self, actionSpace, mu, sigma):
        '''
        env (gym.Env): the environment
        lr (float): learning rate
        '''

        self.M = actionSpace
        self.mu = torch.tensor(mu, requires_grad=True)
        self.sigma = torch.tensor(sigma, requires_grad=True)


    def get_mean(self):
        return self.mu.detach().numpy()

    def get_std(self):
        return self.sigma.detach().numpy()

    def get_parameters(self):
        return [self.mu] + [self.sigma]

    def get_gradient(self):
        return np.hstack([self.mu.grad.detach().numpy(), self.sigma.grad.detach().numpy()])

    def pi(self, s_t):
        '''
        returns the probability distribution over actions
        s_t (np.ndarray): the current state
        '''

        mu = self.mu
        sigma = torch.clip(self.sigma, min = 0.00001)
        pi = torch.distributions.MultivariateNormal(mu, torch.diag(sigma))
        return pi



class REINFORCE:

    def __init__(self, policy, lr = 0.001, optimizer = 'sgd'):
        self.policy = policy
        if (optimizer == 'adam'):
            self.opt = torch.optim.Adam(policy.get_parameters(), lr=lr)
        else:
            self.opt = torch.optim.SGD(policy.get_parameters(), lr=lr)

        self.use_baseline = True
        self.loss = None

    def policy_update(self, states, actions, returns, do_update = True):
        actions_t = actions.detach()
        returns_t = returns.detach()
        if (self.use_baseline):
            baseline = torch.mean(returns_t)
        else:
            baseline = 0

        log_prob = torch.reshape(self.policy.pi(states).log_prob(actions_t), (actions_t.shape))
        self.loss = torch.mean(-log_prob*(returns_t - baseline))
        self.opt.zero_grad()
        self.loss.backward()
        if (do_update):
            self.opt.step()

class Reparametrization:

    def __init__(self, policy, lr = 0.001, optimizer = 'sgd'):
        self.policy = policy
        if (optimizer == 'adam'):
            self.opt = torch.optim.Adam(policy.get_parameters(), lr=lr)
        else:
            self.opt = torch.optim.SGD(policy.get_parameters(), lr=lr)

        self.loss = None

    def policy_update(self, states, actions, returns, do_update = True):
        self.loss = torch.mean(-returns)
        self.opt.zero_grad()
        self.loss.backward()
        if (do_update):
            self.opt.step()



def rewardFunction_easy(action):
    return  (-0.5 * torch.pow(action - 3.5, 2) / 1) + 2 * torch.exp(-0.5 * torch.pow(action - 4, 2) / 0.1)


def rewardFunction_hard(action):
    return  (-0.5 * torch.pow(action - 3.5, 2) / 1) + 2 * torch.exp(-0.5 * torch.pow(action - 4, 2) / 0.1) - 100 * torch.exp(-0.5 * torch.pow(action - 4.2, 2) / 0.001)
    #return  - 10000 * torch.exp(-0.5 * torch.pow(action - 5, 2) / 0.05)

def plot(fig, reward, policy, actions, rewards):

    import matplotlib.pyplot as plt
    plt.figure(fig)
    plt.clf()
    states = torch.zeros((1000,0))
    all_actions = np.linspace(0,5,1000)
    all_actions = all_actions.reshape(1000,1)

    probs = torch.exp(policy.pi(states).log_prob(torch.tensor(all_actions)))
    all_rewards = reward(torch.tensor(all_actions))

    plt.plot(all_actions, probs.detach().numpy(), 'r')
    plt.plot(all_actions, all_rewards.detach().numpy(), 'b')
    plt.plot(actions, rewards, 'b*')
    plt.ylim((-10,5))
    plt.draw()
    plt.pause(0.001)



if __name__ == '__main__':

    import time

    policy = DiagonalGaussianPolicyOnlyActionSigma(1, np.array([2.0]), np.array([0.1]))
    policy_update = REINFORCE(policy, 0.1, optimizer='sgd')

    import matplotlib.pyplot as plt
    fig = plt.figure()
    plt.ion()
    plt.show()



    numSamples = 10
    numIterations = 100
    states = torch.zeros((numSamples, 0))

    performance = np.zeros((numIterations,))
    rewardFunction = rewardFunction_easy

    for i in range(numIterations):
        actions = policy.act(states)

        rewards = rewardFunction(actions)


        policy_update.policy_update(states, actions, rewards)

        actions = actions.detach().numpy()
        rewards = rewards.detach().numpy()

        plot(fig, rewardFunction, policy, actions, rewards)
        time.sleep(0.01)
        print('Iteration %d: %f, mean: %f, std: %f' % (i, np.mean(rewards), policy.get_mean(), policy.get_std()))

        performance[i] = np.mean(rewards)

    plt.figure()
    plt.plot(performance)
    plt.draw()
    plt.pause(0.001)
    plt.pause(1)

    #check for unbiased and viarance of baseline
    policy_update.use_baseline = True
    numRepetitions = 1000
    gradient = np.zeros((numRepetitions,2))
    for i in range(numRepetitions):
        actions = policy.act(states)

        rewards = rewardFunction(actions)

        policy_update.policy_update(states, actions, rewards, do_update=False)

        actions = actions.detach().numpy()
        rewards = rewards.detach().numpy()

        gradient[i,:] = policy.get_gradient()

    print('Expected Gradient with Baseline: ', np.mean(gradient, axis=0), 'Variance: ', np.var(gradient, axis=0))

    policy_update.use_baseline = False
    gradient = np.zeros((numRepetitions, 2))
    for i in range(numRepetitions):
        actions = policy.act(states)

        rewards = rewardFunction(actions)

        policy_update.policy_update(states, actions, rewards, do_update=False)

        actions = actions.detach().numpy()
        rewards = rewards.detach().numpy()

        gradient[i, :] = policy.get_gradient()

    print('Expected Gradient without Baseline: ', np.mean(gradient, axis=0), 'Variance: ', np.var(gradient, axis=0))