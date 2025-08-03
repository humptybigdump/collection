from scipy import optimize
from scipy.stats import norm
import numpy as np

def black_scholes(S_current, Strike, T_remaining, r, sigma, type_='C'):
    S = S_current
    K = Strike
    T = T_remaining
    
    d1 = (np.log(S / K ) + (r + 0.5 * sigma ** 2) * T ) / (sigma * np.sqrt(T ))
    d2 = d1 - sigma*np.sqrt(T)

    if type_ == 'C':
        C_current = S * norm.cdf(d1) - K * np.exp(-r * T) * norm.cdf(d2)
        return C_current
    else:
        P_current = K * np.exp(-r * T) * norm.cdf(-d2) - S * norm.cdf(-d1)
        return P_current
    
def black1976(F_current, Strike, T_remaining, r, sigma, type_='C'):
    F = F_current
    K = Strike
    T = T_remaining
    
    d1 = (np.log(F / K ) + (0.5 * sigma ** 2) * T ) / (sigma * np.sqrt(T))
    d2 = d1 - sigma*np.sqrt(T)

    if type_ == 'C':
        C_current = (F * norm.cdf(d1) - K * norm.cdf(d2)) * np.exp(-r*T)
        return C_current
    else:
        P_current = (K * norm.cdf(-d2) - F * norm.cdf(-d1)) * np.exp(-r*T)
        return P_current
    
def BS_PricingError(sigma, MarketOptionPrice, S, K, T, r, type_='C', underlying='stock'):
    #M: note: first variable is the one for which one later optimizes, the others are constants for the function (args)
    if underlying == 'future':
        return black1976(S, K, T, r, sigma, type_) - MarketOptionPrice  
    else:
        return black_scholes(S, K, T, r, sigma, type_) - MarketOptionPrice  
    
def vega(sigma, MarketOptionPrice, S, K, T, r, type_='C',underlying='stock'):
    d1 = (np.log(S / K) + (r + 0.5 * sigma ** 2) * T) / (sigma * np.sqrt(T))
    vega = S * norm.pdf(d1) * np.sqrt(T)
        
    return vega

def IV_BS_Newton(O_t, S_t, K, Tmt, r, type_='C', IV_0=0.45, underlying='stock'):
    IV = np.zeros(len(O_t))
    for t in range(len(IV)):
        args_         = (O_t[t], S_t[t], K[t], Tmt[t], r[t], type_, underlying)
        res = np.nan
        try:
            res = optimize.newton(BS_PricingError, IV_0, vega, args_)
        except RuntimeError:
            print("impl vol for t=", t, "could not be calculated")
        IV[t] = res
    return IV