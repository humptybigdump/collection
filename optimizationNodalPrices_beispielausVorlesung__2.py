from gurobipy import *
import numpy as np
import pandas as pd

def calcH(nodes, adj, impedance):
    lines = int(np.sum(adj) / 2)
    A = np.zeros((lines, nodes - 1))

    k = 0
    found = False
    for i in range(0, len(adj)):
        for j in range(0, len(adj[i])):
            if adj[i][j] == 1 and found:
                if k < nodes - 1:
                    A[i][k] = 1
                if j < nodes - 1:
                    A[i][j] = -1
                found = False
                k=0
            elif adj[i][j] == 1 and not(found):
                k = j
                found = True

    omega = np.zeros((lines, lines))

    for i in range(lines):
        omega[i][i] = 1/impedance[i]

    H = np.matmul(np.matmul(omega, A), np.linalg.inv(np.matmul(np.matmul(A.transpose(), omega), A)))

    return H

def calcWelfare(priceResult, nodes, trans_capa_range, transLines, H):

        nodalPrices = np.zeros(nodes)
        price = -priceResult[0]
        nodalPrices[nodes-1] = price

        for i in range(0, nodes - 1):
            for j in trans_capa_range:
                price = price + (priceResult[(j+1)] + priceResult[(transLines+1+j)]) * H[j][i]

            nodalPrices[i] = price
            price = -priceResult[0]

        return nodalPrices



def run_gurobi_Nodal(capa, demand, H, trans_capa, gen_units, nodes, costs):
    try:

        decVariables = []
        priceResult = []

        trans_capa_range = range(len(trans_capa))
        gen_units_range = range(gen_units)
        demand_range = range(len(demand))

        m = Model("optimizedConMC")

        generation = m.addVars(gen_units_range, vtype=GRB.CONTINUOUS, lb=0, ub=GRB.INFINITY, name="GenUnit")

        m.setObjective(quicksum(generation[i]*costs[i] for i in gen_units_range), GRB.MINIMIZE)

        m.addConstr(quicksum(demand[i] for i in demand_range) == quicksum(generation[j] for j in gen_units_range), 'balance')

        m.addConstrs((quicksum((generation[j] - demand[j]) * H[i][j] for j in range(nodes - 1)) <= trans_capa[i] for i in trans_capa_range), 'NetworkContraint')
        m.addConstrs((quicksum((generation[j]  - demand[j]) * H[i][j] for j in range(nodes - 1)) >= -trans_capa[i] for i in trans_capa_range), 'NetworkContraintBackwards')

        m.addConstrs((generation[i]<=capa[i] for i in gen_units_range), 'capacity_bound')

        m.optimize()


        for v in m.getVars():
            #print('%s %g' % (v.varName, v.x))
            decVariables.append(v.x)

        print('done')

        print('Obj: %g' % m.objVal)
        priceResult = m.Pi

    except GurobiError as e:
        print('Error code ' + str(e.errno) + ": " + str(e))

    except AttributeError:
        print('Encountered an attribute error')

    return [decVariables, priceResult]


nodes = 3
capa = [30, 30, 0]
trans_capa = [8, 20 ,30]
adj = [[1,1,0], [0,1,1], [1,0,1]]
#costFactors = [2, 3.35, 0]
costs = [30, 33.5, 0]
gen_units = nodes
impedance = (1, 1/3, 2)

H = calcH(nodes, adj, impedance)

demand = [0,0,25]

[generation, prices] = run_gurobi_Nodal(capa, demand, H, trans_capa, gen_units, nodes, costs)
nodalPrices = calcWelfare(prices, nodes, range(len(trans_capa)), len(trans_capa), H)

nodes2 = 4
capa2 = [5,5,0,1]
demand2 = [0,0,5,3]
costs2 = [3,1, 0, 1]
gen_units2 = nodes2
impedance2 = [1,1,1,1]
adj2=[[1,1,0,0], [1,0,1,0], [0,1,0,1], [0,0,1,1]]
trans_capa2 = [10,10,10,1]

H2 = calcH(nodes2, adj2, impedance2)

[generation2, prices2] = run_gurobi_Nodal(capa2, demand2, H2, trans_capa2, gen_units2, nodes2, costs2)
nodalPrices2 = calcWelfare(prices2, nodes2, range(len(trans_capa2)), len(trans_capa2), H2)

o=1