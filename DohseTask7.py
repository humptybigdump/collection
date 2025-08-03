import itertools, sys, getopt

def line_prepender(filename, line):
    with open(filename, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        f.write(line.rstrip('\r\n') + '\n' + content)

f = open("Dohse.cnf", "w")
#f.write("p cnf 1000 5000\n")

def onlyOne(vars): #implement to allow only 1 to be true
    for x in itertools.combinations(vars,2):
        f.write("-%s -%s 0\n"%(x[0],x[1]))

def atLeastone(vars):#implement to make at least one true
    for x in vars:
        f.write("%s "%x)                    # one has to be true
    f.write("0\n")

def setterOrCell(setters,cells): #for each setter and each cell: not setter or cell
    for s in setters:
        for c in cells:
            f.write("-%s %s 0\n"%(s,c))

def countVariables(vars):
    if len(vars)>0:

        for x in range(len(vars)-1,0,-1):
            f.write("-%s %s 0\n"%(vars[x],vars[x-1]))
        f.write("%s 0\n"%vars[len(vars)-1])
        

height = int(sys.argv[1])
width = int(sys.argv[2])
iNumBlock = int(sys.argv[3])
tNumBlock = int(sys.argv[4])
sNumBlock = int(sys.argv[5])
lNumBlock = int(sys.argv[6])
oNumBlock = int(sys.argv[7])

#print("heigth %s width %s i %s t %s s %s l %s o %s"%(height,width,iNumBlock,tNumBlock,sNumBlock,lNumBlock,oNumBlock))
cells = [ ]
variables = 1
iList = []
tList = []
sList = []
lList = []
oList = []

iFinalList = []
tFinalList = []
sFinalList = []
lFinalList = []
oFinalList = []

for h in range(height): #get an empthy list for each cell first argument height second width
    h_list = []
    for w in range(width):
        h_list.append([])
    cells.append(h_list)

for h in range(height):
    for w in range(width):
        if h <= height -4: #add I 
            setterList = []
            cellList = []
            for numSeters in range(iNumBlock):
                setterList.append(variables)
                iList.append(variables)
                variables += 1
            onlyOne(setterList)
            for i in range(4):
                cellList.append(variables)
                cells[h+i][w].append(variables)
                variables += 1
            setterOrCell(setterList,cellList)

        if (h <= height -2)  and  (w <= width - 3): #add T 
            setterList = []
            cellList = []
            for numSeters in range(tNumBlock):
                setterList.append(variables)
                tList.append(variables)
                variables += 1
            onlyOne(setterList)
            for i in range(3):
                cellList.append(variables)
                cells[h][w+i].append(variables)
                variables += 1
            cellList.append(variables)
            cells[h+1][w+1].append(variables)
            variables += 1
            setterOrCell(setterList,cellList)

        if (h >  0)  and  (w <= width -3): #add S 
            setterList = []
            cellList = []
            for numSeters in range(sNumBlock):
                setterList.append(variables)
                sList.append(variables)
                variables += 1
            onlyOne(setterList)
            for i in range(2):
                cellList.append(variables)
                cells[h][w+i].append(variables)
                variables += 1
                cellList.append(variables)
                cells[h-1][w+1+i].append(variables)
                variables += 1
            setterOrCell(setterList,cellList)

        if (h <= height -2)  and  (w <= width -3): #add L 
            setterList = []
            cellList = []
            for numSeters in range(lNumBlock):
                setterList.append(variables)
                lList.append(variables)
                variables += 1
            onlyOne(setterList)
            for i in range(3):
                cellList.append(variables)
                cells[h+1][w+i].append(variables)
                variables += 1
            cellList.append(variables)
            cells[h][w].append(variables)     
            variables += 1
            setterOrCell(setterList,cellList)

        if (h <= height -2)  and  (w <= width -2): #add O
            setterList = []
            cellList = []
            for numSeters in range(oNumBlock):
                setterList.append(variables)
                oList.append(variables)
                variables += 1
            onlyOne(setterList)
            for i in range(2):
                cellList.append(variables)
                cells[h][w+i].append(variables)
                variables += 1
                cellList.append(variables)
                cells[h+1][w+i].append(variables)
                variables += 1
            setterOrCell(setterList,cellList)

#print(cells)

for i in range (iNumBlock):
    setterList = []
    for element in range(0,len(iList),iNumBlock):
        setterList.append(iList[element+i])
    iFinalList.append(variables)   
    setterOrCell(setterList,[variables])
    variables += 1
    atLeastone(setterList)
countVariables(iFinalList)

for i in range (tNumBlock):
    setterList = []
    for element in range(0,len(tList),tNumBlock):
        setterList.append(tList[element+i])
    tFinalList.append(variables)   
    setterOrCell(setterList,[variables])
    variables += 1
    atLeastone(setterList)
countVariables(tFinalList)

for i in range (lNumBlock):
    setterList = []
    for element in range(0,len(lList),lNumBlock):
        setterList.append(lList[element+i])
    lFinalList.append(variables)   
    setterOrCell(setterList,[variables])
    variables += 1
    atLeastone(setterList)
countVariables(lFinalList)

for i in range (oNumBlock):
    setterList = []
    for element in range(0,len(oList),oNumBlock):
        setterList.append(oList[element+i])
    oFinalList.append(variables)   
    setterOrCell(setterList,[variables])
    variables += 1
    atLeastone(setterList)
countVariables(oFinalList)

for i in range (sNumBlock):
    setterList = []
    for element in range(0,len(sList),sNumBlock):
        setterList.append(sList[element+i])
    sFinalList.append(variables)   
    setterOrCell(setterList,[variables])
    variables += 1
    atLeastone(setterList)
countVariables(sFinalList)



for h in range(height): #only one block per cell
    for w in range(width):
        onlyOne(cells[h][w])


f.close()

count = 0
for line in open("Dohse.cnf").xreadlines(  ): count += 1

line_prepender("Dohse.cnf","p cnf %s %s\n"%(variables,count))
""" 
def between(file, startVar, numValues):
    for x in itertools.combinations(range(startVar,startVar+numValues),2):
        file.write("-%s -%s 0\n"%(x[0],x[1])) #not two values true
    for x in range(startVar,startVar+numValues):
        file.write("%s "%x)                    # one has to be true
    file.write("0\n")

#tage m
for person in  range(3):
    for day in range(5):
        print("Person %s Day %s starts at %s\n"%(person+1,day+1,variables))
        between(f,variables,8)
        variables = variables +8

for otherday in range(4):
    print("Day %s starts at %s\n"%(otherday+1,variables))
    between(f,variables,5)
    variables = variables +5 """

#print("Variables: %s"%variables)
