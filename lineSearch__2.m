function out_ = lineSearch(handles_, param_)
  % General parameters
  xk      = param_.xStart;
  maxIter = param_.maxIter; 
  tol     = param_.tol;
  % Step length method
  stepLengthMethod = param_.alphak.method;
  % Search direction method
  searchDirectionMethod = param_.sk.method;

  % Local function handles
  fFun      = handles_.fFun;
  gradfFun  = handles_.gradfFun;
  
  % Initialize
  k         = 0;
  fDist     = 1e5;
  xIter     = xk;
  alphaIter = [];
  fk        = fFun(xk);
  gradfk    = gradfFun(xk);
    
  % Main loop
  while ( k < maxIter && fDist > tol )
    %%================== Search direction ==================%%
    switch searchDirectionMethod
      case 'steepestDescent'
        sk = -gradfk;
      case 'conjugatedGradient'
        % TODO: Exercise 1 task (iv)  
      case 'newton' 
        % TODO: Exercise 1 task (v)
      case 'quasiNewton'
        % TODO: Exercise 1 task (v)         
      otherwise
        error('Not implemented line search method!')
    end

    %%================== Step length ==================%%
    switch stepLengthMethod
      case 'wolfe'
        alphak = wolfe(fFun, xk, sk, fk, gradfk, param_);
      case 'quadraticInterpolation'
        % TODO: Exercise 1 task (iii)
        otherwise
        error('Not implemented step length method!')
    end
        
    %%================== Iterate ==================%%     
    xkp1 = xk + alphak*sk;

    %%================== Update values ==================%%
    fkp1      = fFun(xkp1);
    gradfkp1  = gradfFun(xkp1);
    fDist     = abs(fkp1);
    xIter     = [xIter xkp1];
    alphaIter = [alphaIter alphak];
    
    %%================== Shift iteration count k ==================%%
    k      = k + 1;
    xk     = xkp1;
    fk     = fkp1;
    gradfk = gradfkp1;
  end
  
  % Write solution to output
  if maxIter == k
    out_.converged = false;
  else
    out_.converged = true;
  end
  out_.iterations = k;
  out_.fDist      = fDist;
  out_.xOpt       = xk;
  out_.iter.x     = xIter;
  out_.iter.alpha = alphaIter;
end