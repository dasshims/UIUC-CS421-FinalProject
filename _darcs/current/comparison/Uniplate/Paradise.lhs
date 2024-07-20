%----------------------------------------------------------------------------
%
%  Title       :  Paradise.lhs
%  Author(s)   :  Alex Gerdes
%  License     :  BSD
%  Created     :  6 March 2008
%
%  Remarks     :  - 
%
%----------------------------------------------------------------------------

> module Paradise(increase) where
> import Data.Generics.PlateData(transformBi)
> import CompanyDatatypes(Company, Salary(S))
> import CompanyReps

> -- Increase salary by percentage
> increase :: Float -> Company -> Company
> increase k = transformBi (incS k)

> -- "interesting" code for increase
> incS :: Float -> Salary -> Salary
> incS k (S s) = S (s * (1+k))
