t1 = ["r1(A)", "w1(A)", "r1(B)", "w1(B)"]
t2 = ["r2(B)", "w2(B)", "r2(A)", "w2(A)"]

out = []

def permutes(l1, l2, res):
    global out
    if len(l1) == 0:
        res += l2
        print("res" + str(res))
        out += [res]
        return
    if len(l2) == 0:
        res += l1
        out += [res]
        print("res" + str(res))
        return
    permutes(l1, l2[1:], res + [l2[0]])
    permutes(l1[1:], l2, res + [l1[0]])

def get_action(t):
    return t[0]

def get_transaction_id(t):
    return t[1]

def get_element(t):
    return t[3]

def can_lock(locks, t_id, element):
    return locks[element] == 0 or locks[element] == t_id

def is_executable(s):
    locks = { "A" : 0, "B" : 0 }
    for t in s:
        action = get_action(t)
        t_id = get_transaction_id(t)
        element = get_element(t)
        if action == "r":
            if can_lock(locks, t_id, element):
                locks[element] = t_id
            else:
                return False
        elif action == "w":
            if can_lock(locks, t_id, element):
                locks[element] = 0
            else:
                return False
    
    return True

def main():
    permutes(t1, t2, [])
    i = 0
    for s in out:
        if is_executable(s):
            i += 1
            print("s" + str(s))
    print("Anzahl ausführbarer Schedules: " + str(i))

if __name__ == "__main__":
    main()
