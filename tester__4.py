import numpy as np
testfield1d = np.linspace(0, 1, 10)**3
np.random.seed(42) # consistent data
testfield2d = np.random.rand(5, 5)


#anisoterms(field, i, j)
#def phiupdate(phi, U,  i, j):
#uupdate(U, dphi_dt, i, j):
#timestep(phi_old, phi_new, U_old, U_new, numx, numy):
#correct = []
#timestep(testfield2d, outfield_phi, testfield2d, outfield_U, testfield2d.shape[0], testfield2d.shape[1])
#for i in range(1,testfield2d.shape[0]-1):
#    for j in range(1,testfield2d.shape[1]-1):
               #correct.append(uupdate(testfield2d, testfield2d[j][i], i, j))

correct_A = np.array([0.82      , 0.99999932, 0.99991215, 0.99779669, 0.97329063,
       0.91      , 0.97329063, 0.99779669, 0.99991215, 0.99999932])
correct_Aprime = np.array([ 0.        ,  0.00098765,  0.01124176,  0.0556336 ,  0.16446047,
        0.        , -0.16446047, -0.0556336 , -0.01124176, -0.00098765])
correct_staggered = np.array([[0.68165306, 0.30114312, 0.651742  , 0.38483314],
       [0.62801823, 0.68165306, 0.6994859 , 0.651742  ],
       [0.42583792, 0.62801823, 0.51596618, 0.6994859 ],
       [0.65783779, 0.36953527, 0.68165306, 0.30114312],
       [0.5003708 , 0.65783779, 0.62801823, 0.68165306],
       [0.27933456, 0.5003708 , 0.42583792, 0.62801823],
       [0.3151593 , 0.30974838, 0.65783779, 0.36953527],
       [0.40380199, 0.3151593 , 0.5003708 , 0.65783779],
       [0.3864015 , 0.40380199, 0.27933456, 0.5003708 ]])
correct_anisoterms = np.array([[ 2.24038924e+00, -2.21940636e+00,  2.02022812e+00,
        -2.14237372e-01],
       [-5.49859499e-02,  3.32082357e-01, -6.60168504e-01,
         2.02022812e+00],
       [-9.38604491e-01,  1.72143838e-03,  2.35058588e-01,
        -6.60168504e-01],
       [-1.63189211e+00,  2.24038924e+00, -3.43424430e-01,
         2.37325981e+00],
       [-7.44072758e-01, -5.49859499e-02, -1.54813464e+00,
        -3.43424430e-01],
       [ 4.53467708e-01, -9.38604491e-01, -2.65404212e-02,
        -1.54813464e+00],
       [-4.11870550e-01, -1.63189211e+00,  3.64523217e-01,
         2.93183735e-01],
       [-5.74141392e-01, -7.44072758e-01, -1.89067478e-01,
         3.64523217e-01],
       [-1.63378452e-01,  4.53467708e-01, -3.18126002e-01,
        -1.89067478e-01]])
correct_phiupdate = np.array([ 16.61822921,  -7.80337703,  -0.55938227, -17.37709482,
        -5.24584593,   7.17231533,   2.96325392,  -1.51193619,
        -2.84074836])
correct_uupdate = np.array([ 33.90979651, -15.02841228,  -0.08805049, -32.87049216,
        -9.04114147,  15.23701816,   7.80800354,  -2.87197014,
        -3.94769685])
correct_timestep = np.array([[[0.        , 0.        , 0.        , 0.        , 0.        ],
        [0.        , 0.25750236, 0.77253562, 0.59440242, 0.        ],
        [0.        , 0.76138471, 0.76949249, 0.29840689, 0.        ],
        [0.        , 0.33980129, 0.5066132 , 0.39785604, 0.        ],
        [0.        , 0.        , 0.        , 0.        , 0.        ]],

       [[0.        , 0.        , 0.        , 0.        , 0.        ],
        [0.        , 0.56436204, 0.63319548, 0.59487666, 0.        ],
        [0.        , 0.46600432, 0.68747921, 0.43506868, 0.        ],
        [0.        , 0.41211112, 0.47994714, 0.3649365 , 0.        ],
        [0.        , 0.        , 0.        , 0.        , 0.        ]]])

def tester(testfunc, correct):
    fncname = testfunc.__name__
    vals = []
    if fncname in ["A", "Aprime"]:
        for i in range(len(testfield1d)):
            vals.append ( testfunc(testfield1d[i], testfield1d[-i]) )
    elif fncname in ["staggered", "anisoterms"]:
        for i in range(1,testfield2d.shape[0]-1):
            for j in range(1,testfield2d.shape[1]-1):
                vals.append(testfunc(testfield2d, int(i), int(j)))
    elif fncname == "phiupdate":
        for i in range(1,testfield2d.shape[0]-1):
            for j in range(1,testfield2d.shape[1]-1):
                vals.append(testfunc(testfield2d, testfield2d, i, j))
    elif fncname == "uupdate":
        for i in range(1,testfield2d.shape[0]-1):
            for j in range(1,testfield2d.shape[1]-1):
                vals.append(testfunc(testfield2d, testfield2d[j][i], i, j))
    else:
        #timestep
        outfield_phi = np.zeros_like(testfield2d)
        outfield_U = np.zeros_like(testfield2d)
        testfunc(testfield2d, outfield_phi, testfield2d, outfield_U, testfield2d.shape[0], testfield2d.shape[1])
        vals = [outfield_phi, outfield_U]
    
    diff = np.nansum(np.abs(vals - correct))
    if diff < 1e-6:
        print(fncname + " works")
        return True
    else:
        print(fncname + " doesn't work, diff: " + str(diff))
        return False
