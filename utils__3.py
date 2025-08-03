import time
import numpy as np
import torch

def discount_rewards(rewards, gamma=0.99):
    # Reverse the array direction for cumsum and then
    # revert back to the original order
    r = np.array([gamma**i * rewards[i] for i in range(len(rewards))])
    r = r[::-1].cumsum()[::-1]
    return r - r.mean()

def on_environment(env,policy,n_episode,horizon):
    # Set up lists to hold results
    for i_episode in range(n_episode):
        states  = []
        actions = []
        rewards = []
        state = env.reset()
        for t in range(horizon):
            env.render()
            time.sleep(0.05)
            if policy == "random":
                action = env.action_space.sample()
            else:
                # Get actions and convert to numpy array
                action_probs = policy.predict(state).detach().numpy()
                #action = np.random.choice(action_space, p=action_probs)
                action = np.argmax(action_probs)
            state, reward, done, info = env.step(action)
            ## Add the transition to the lists
            states.append(state)
            rewards.append(reward)
            actions.append(action)
            if done:
                print("Episode finished after {} timesteps".format(t+1))
                break
        print("total Reward", np.sum(rewards))
    ## Convert lists to numpy arrays
    states  =  np.array(states)
    actions =  np.array(actions)
    rewards =  np.array(rewards)
    ## Close the env
    env.close()
    return states,actions,rewards


def reinforce(env,policy,optimizer,args):
    action_space = np.arange(env.action_space.n)
    action_space
    # Set up lists to hold results
    total_rewards = []
    batch_rewards = []
    batch_actions = []
    batch_states  = []
    batch_counter = 1
    ep = 0
    epoch = 0
    while ep < args["num_episodes"]:
        s_0 = env.reset()
        states = []
        rewards = []
        actions = []
        done = False
        while done == False:
            # Get actions and convert to numpy array
            action_probs = policy.predict(s_0).detach().numpy()
            action = np.random.choice(action_space, p=action_probs)
            s_1, r, done, _ = env.step(action)
            states.append(s_0)
            rewards.append(r)
            actions.append(action)
            s_0 = s_1

            # If done, batch data
            if done:
                batch_rewards.extend(discount_rewards(rewards, args["gamma"]))
                batch_states.extend(states)
                batch_actions.extend(actions)
                batch_counter += 1
                total_rewards.append(sum(rewards))

                # If batch is complete, update network
                if batch_counter == args["batch_size"]:
                    print("Epoch:", epoch)
                    optimizer.zero_grad()
                    state_tensor = torch.FloatTensor(batch_states)
                    reward_tensor = torch.FloatTensor(batch_rewards)
                    # Actions are used as indices, must be 
                    # LongTensor
                    action_tensor = torch.LongTensor(batch_actions)
                    # Calculate loss
                    logprob = torch.log(policy.predict(state_tensor))
                    selected_logprobs = reward_tensor * torch.gather(logprob, 1,action_tensor.unsqueeze(1)).squeeze()
                    loss = -selected_logprobs.mean()

                    # Calculate gradients
                    loss.backward()
                    # Apply gradients
                    optimizer.step()

                    batch_rewards = []
                    batch_actions = []
                    batch_states = []
                    batch_counter = 1
                    epoch += 1

                avg_rewards = np.mean(total_rewards[-100:])
                # Print running average
                print("Episode: ", ep + 1)
                print("Average of last 100 Episode:", avg_rewards)
                ep += 1
                
    return policy,total_rewards