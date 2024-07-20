> import CompanyDatatypes
> import GShow (gshowsCompany)

The GShow-test tests the following criteria:
* Consumer generic functions
* Access to constructor names, though this is not a criterion in the paper yet.

What is not required of show:
* Showing lists the way Haskell does.
  Why? Because ad-hoc cases are already tested with the TestGShowExt and Paradise tests.
* Showing strings the way Haskell's show does.
  Why? For simplicity

The ad-hoc case for lists is tested in TestGShowExt.

Where does list occur? It does in the company datatype.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Function show}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> main = print $ gshowsCompany genCom
