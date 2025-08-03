def Ads3_fun(G_inert, S_inert, Isotherm, W_F, W_S, W_R):
    """
    Using the mass balance around the first stage and Isotherm 
    to calculate the concentration around second stage
    """
    
    W_S1 = Isotherm(W_R)
    W_R2 = (G_inert * W_R - S_inert * W_S + S_inert * W_S1) / G_inert

    # Using the mass balance around the second stage and Isotherm 
    # to calculate the concentration around third stage
    W_S2 = Isotherm(W_R2)
    W_R3 = (G_inert * W_R2 - S_inert * W_S1 + S_inert * W_S2) / G_inert

    # Using the mass balance around the third stage and Isotherm 
    # to calculate the concentration of the feed
    W_S3 = Isotherm(W_R3)
    W_Fcalc = (G_inert * W_R3 - S_inert * W_S2 + S_inert * W_S3) / G_inert

    fval = (W_F * 100 - W_Fcalc * 100) ** 2
    return fval