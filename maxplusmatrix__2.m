classdef maxplusmatrix < double
    % Klasse fuer Max-Plus-Algebra (Matrix-Dioide)
    % Max-Operatoren: times, plus (max), inverse, power, hadamard-times
    % Schreibweise fuer spezielle Kantengewichte: e = 0, \epsilon = -inf
    % gilt fuer In- und Output
    % Beispiel: A1 = maxplusmatrix([1 0 ;4 -inf]);
    %           A2 = A1*A1;
    %           A2 = times(A1,A2);
    %           Ax = A1^0 + A1 + A1^2 + A1^3 + A1^4;
    %           Ax = plus(A1^0,A1,A1^2,A1^3,A1^4);
    
    %% Methoden
    methods
	
        %% Konstruktor: maxplusmatrix
        function mpo = maxplusmatrix(mat_in)
            mpo = mpo@double(mat_in);
        end

        %% Plus-Operator (entspricht Multiplikation)
        % mtimes ist die Funktion zur Matrixmultiplikation in Matlab. Diese
        % wird hier überladen, damit alternativ der Plus-Operator auch als
        % "*" geschrieben werden kann.
        function mp_new = mtimes(mp1,mp2)
            mp1 = double(mp1);
            mp2 = double(mp2);
            if max(size(mp1))==1 || max(size(mp2))==1
                C =(mp1) + (mp2);
            else
                if size(mp1,2) ~= size(mp2,1)
                    error('Dimensions must agree!')
                else
                    mp1_size = size(mp1);
                    mp2_size = size(mp2);
                    mp1mp1 = repmat(reshape(mp1,[mp1_size(1) 1 mp1_size(2)]),[1 mp2_size(2) 1]);
                    mp2mp2 = repmat(reshape(mp2',[1 mp2_size(2) mp2_size(1)]),[mp1_size(1) 1 1]);
                    CC = mp1mp1+mp2mp2;
                    C = max(CC,[],3);
                end
            end
            mp_new = maxplusmatrix(C);
        end


        %% Max-Operator (entspricht Addition)
        % mtimes ist die Funktion zur herkömmlichen Addition in Matlab. 
        % Diese wird hier überladen, damit alternativ der Max-Operator auch
        % als "+" geschrieben werden kann.
        function mp_new = plus(varargin)
            % varargin - input: n Matrizen gleicher Groesse

            C = -inf(size(varargin{1}));
            for ii = 1:length(varargin)
                C = max(C,double(varargin{ii}));
            end
            mp_new = maxplusmatrix(C);
        end


        %% Plus-Operator der Min-Plus-Algebra
        function mp_new = mtimesmin(mp1,mp2)
            mp1 = double(mp1);
            mp2 = double(mp2);
            if max(size(mp1))==1 || max(size(mp2))==1
                C =(mp1) + (mp2);
            else
                if size(mp1,2) ~= size(mp2,1)
                    error('Dimensions must agree!')
                else
                    mp1_size = size(mp1);
                    mp2_size = size(mp2);
                    mp1mp1 = repmat(reshape(mp1,[mp1_size(1) 1 mp1_size(2)]),[1 mp2_size(2) 1]);
                    mp2mp2 = repmat(reshape(mp2',[1 mp2_size(2) mp2_size(1)]),[mp1_size(1) 1 1]);
                    CC = mp1mp1 + mp2mp2;
                    C = min(CC,[],3);
                end
            end
            mp_new = maxplusmatrix(C);
        end


        %% Minimum-Operator der Min-Plus-Algebra
        function mp_new = minus(varargin)
            % varargin - input: n Matrizen gleicher Groesse

            C = inf(size(varargin{1}));
            for ii = 1:length(varargin)
                C = min(C,double(varargin{ii}));
            end
            mp_new = maxplusmatrix(C);
        end


        %% Inverse
        function mp_new = inv(mp)
            mp_new = maxplusmatrix(-1*double(mp));
        end


        %% Potenz
        function mp_new = mpower(mp,exp)
            if exp == -1
               mp_new = inv(mp);
            else
                mp_new = maxplusmatrix(-inf(size(mp)));
                mp_new(1:size(mp,1)+1:end) = 0;
                for i_exp = 1:exp
                    mp_new = mp_new*mp;
                end
            end
        end


        %% Hadamard-Multiplikation (elementweiser Plus-Operator)
        function mp_new = times(mp1,mp2)
            mp_new = maxplusmatrix(plus(double(mp1),double(mp2)));
        end
    end


    %% Alternative Funktionsaufrufe/Überschreiben von Funktionen
    methods
        function mp_new = odot(mp1,mp2)
            mp_new = times(mp1,mp2);
        end

        function mp_new = otimes(mp1,mp2)
            mp_new = mtimes(mp1,mp2);
        end

        function mp_new = oplus(varargin)
            mp_new = plus(varargin{:});
        end

        function mp_new = max(varargin)
            mp_new = plus(varargin{:});
        end
    end
end
