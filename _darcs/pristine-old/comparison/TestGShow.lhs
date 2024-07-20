
This test exercices GENERIC show for the infamous company datatypes. The
output of the program should be some representation of the infamous
"genCom" company.


NOTE that this program does not produce the SAME output as the SYB gshow.
Instead, it produces the same output as deriving Show would.

> import CompanyDatatypes
> import GShow (gshowsCompany)

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

To add:
 * Differentiate a purely structural show and one with ad hoc definitions.
   This is in order to show lists in a special way.

> main = print $ gshowsCompany genCom

