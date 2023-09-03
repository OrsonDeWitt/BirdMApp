/*
Header file for using internal C-level facilities
provided by xts.

This is not 100% designed for end users, so
any user comments and bug reports are very
welcomed.

Copyright Jeffrey A. Ryan 2008

This source is distributed with the same license
as the full xts software, GPL (>= 2).
*/
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#ifndef _XTS
#define _XTS

#ifdef __cplusplus
extern "C" {
#endif

/*
INTERNAL SYMBOLS
*/
extern SEXP xts_IndexSymbol;
extern SEXP xts_ClassSymbol;
extern SEXP xts_IndexTformatSymbol;
extern SEXP xts_IndexTclassSymbol;
extern SEXP xts_IndexTzoneSymbol;

/*
DATA TOOLS
*/
#define  xts_ATTRIB(x)                  coerceVector(do_xtsAttributes(x),LISTSXP)
#define  xts_COREATTRIB(x)              coerceVector(do_xtsCoreAttributes(x),LISTSXP)

// attr(x, 'index') or .index(x)
#define  GET_xtsIndex(x)                getAttrib(x, xts_IndexSymbol)
#define  SET_xtsIndex(x,value)          setAttrib(x, xts_IndexSymbol, value)

// attr(x, '.indexFORMAT') or indexFormat(x)
#define  GET_xtsIndexFormat(x)          getAttrib(x, xts_IndexFormatSymbol)
#define  SET_xtsIndexFormat(x,value)    setAttrib(x, xts_IndexFormatSymbol, value)

// attr(x, '.CLASS') or CLASS(x)
#define  GET_xtsCLASS(x)                getAttrib(x, xts_ClassSymbol)
#define  SET_xtsCLASS(x,value)          setAttrib(x, xts_ClassSymbol, value)

/*
IMPORTS FROM zoo
*/
extern SEXP(*zoo_lag)(SEXP,SEXP,SEXP);
extern SEXP(*zoo_coredata)(SEXP,SEXP);

/*
FUNCTIONS
*/
SEXP do_xtsAttributes(SEXP x);              // xtsAttributes i.e. user-added attributes
SEXP add_xtsCoreAttributes(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP do_xtsCoreAttributes(SEXP x);          /* xtsCoreAttributes xts-specific attributes
                                               CLASS, .indexFORMAT, tclass, & class */
SEXP coredata(SEXP x, SEXP copyAttr);
SEXP coredata_xts(SEXP x);
SEXP add_class(SEXP x, SEXP klass);
SEXP lagXts(SEXP x, SEXP k, SEXP pad);
SEXP lag_xts(SEXP x, SEXP k, SEXP pad);
SEXP do_is_ordered(SEXP x, SEXP increasing, SEXP strictly);
SEXP mergeXts(SEXP args);
SEXP do_rbind_xts(SEXP x, SEXP y, SEXP dup);
SEXP rbindXts(SEXP args);
SEXP do_subset_xts(SEXP x, SEXP sr, SEXP sc, SEXP drop);
SEXP _do_subset_xts(SEXP x, SEXP sr, SEXP sc, SEXP drop);
SEXP number_of_cols(SEXP args);
SEXP naCheck(SEXP x, SEXP check);

SEXP make_index_unique(SEXP x, SEXP eps);
SEXP make_unique(SEXP X, SEXP eps);
SEXP endpoints(SEXP _x, SEXP _on, SEXP _k, SEXP _addlast);
SEXP do_merge_xts(SEXP x, SEXP y, SEXP all, SEXP fill, SEXP retclass, SEXP colnames, 
                  SEXP suffixes, SEXP retside, SEXP check_names, SEXP env, SEXP coerce);
SEXP na_omit_xts(SEXP x);
SEXP na_locf(SEXP x, SEXP fromlast, SEXP maxgap, SEXP limit);

SEXP tryXts(SEXP x);
SEXP binsearch(SEXP, SEXP, SEXP);
SEXP any_negative(SEXP);
SEXP fill_window_dups_rev(SEXP data, SEXP index);
SEXP non_duplicates(SEXP data, SEXP from_last);

SEXP toPeriod(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP xts_period_apply(SEXP data, SEXP index, SEXP function, SEXP env);
SEXP xts_period_min(SEXP data, SEXP index);
SEXP xts_period_max(SEXP data, SEXP index);
SEXP xts_period_sum(SEXP data, SEXP index);
SEXP xts_period_prod(SEXP data, SEXP index);

SEXP roll_min(SEXP x, SEXP n);
SEXP roll_max(SEXP x, SEXP n);
SEXP roll_sum(SEXP x, SEXP n);
SEXP roll_cov(SEXP x, SEXP n, SEXP sample, SEXP);

SEXP dimnames_zoo(SEXP x);
SEXP xts_set_dimnames(SEXP x, SEXP value);


void copyAttributes(SEXP x, SEXP y);    // internal only
void copy_xtsAttributes(SEXP x, SEXP y);    // internal only
void copy_xtsCoreAttributes(SEXP x, SEXP y);// internal only    

SEXP isXts(SEXP x);                         // is.xts analogue
int firstNonNA(SEXP x);
SEXP extract_col (SEXP x, SEXP j, SEXP drop, SEXP first_, SEXP last_);
SEXP do_startofyear(SEXP from, SEXP to, SEXP origin);
int xts_ncols(SEXP x);

#endif /* _XTS */

#ifdef __cplusplus
}
#endif
