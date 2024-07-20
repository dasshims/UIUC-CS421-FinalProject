> import CompanyDatatypes
> import GShowExt (gshowsCompany)

The GShow-test tests the following criteria:
* Consumer generic functions
* Access to constructor names, though this is not a criterion in the paper yet.
* Extension, GShow is extended with a case for lists, so they are printed
  the Haskell way.

What is not required of show:
* Showing strings the way Haskell's show does.
  Why? For simplicity

Where does list occur? It does in the company datatype.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> main = print $ GShowExt.gshowsCompany genCom
