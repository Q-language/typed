#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* Section generated by pkgbuild, do not edit */
/* .Call calls */
extern SEXP is_promise(SEXP, SEXP);
extern SEXP rlang_eval(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"is_promise", (DL_FUNC) &is_promise, 2},
    {"rlang_eval", (DL_FUNC) &rlang_eval, 2},
    {NULL, NULL, 0}
};
/* End section generated by pkgbuild */

void R_init_typed(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
