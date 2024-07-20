> module GShowExt where
> import LIGD()
> import GShowDef(gShows)
> import CompanyDatatypes(Company)
> import CompanyReps(rCompany)
> gshowsCompany     :: Company -> String
> gshowsCompany x   =  error "LIGD.GShowExt.gshowsCompany: LIGD does not supportad-hoc cases for datatypes"
