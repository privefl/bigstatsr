#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP bigstatsr_auc_cpp(SEXP, SEXP);
extern SEXP bigstatsr_bigcolvars(SEXP, SEXP, SEXP);
extern SEXP bigstatsr_complete2(SEXP);
extern SEXP bigstatsr_COPY_cdfit_binomial_hsr(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_COPY_cdfit_gaussian_hsr(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_COPY_sparse_svm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_cpMatVec4(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_decodeMat(SEXP, SEXP);
extern SEXP bigstatsr_decodeVec(SEXP, SEXP);
extern SEXP bigstatsr_incrMat(SEXP, SEXP);
extern SEXP bigstatsr_incrSup2(SEXP, SEXP);
extern SEXP bigstatsr_IRLS(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_linRegPcadapt_cpp(SEXP, SEXP, SEXP);
extern SEXP bigstatsr_mycount1(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_mycount2(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_pMatVec4(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigstatsr_scaling(SEXP, SEXP, SEXP);
extern SEXP bigstatsr_transpose3(SEXP, SEXP);
extern SEXP bigstatsr_univLinReg5(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"bigstatsr_auc_cpp",                 (DL_FUNC) &bigstatsr_auc_cpp,                  2},
  {"bigstatsr_bigcolvars",              (DL_FUNC) &bigstatsr_bigcolvars,               3},
  {"bigstatsr_complete2",               (DL_FUNC) &bigstatsr_complete2,                1},
  {"bigstatsr_COPY_cdfit_binomial_hsr", (DL_FUNC) &bigstatsr_COPY_cdfit_binomial_hsr, 16},
  {"bigstatsr_COPY_cdfit_gaussian_hsr", (DL_FUNC) &bigstatsr_COPY_cdfit_gaussian_hsr, 15},
  {"bigstatsr_COPY_sparse_svm",         (DL_FUNC) &bigstatsr_COPY_sparse_svm,         15},
  {"bigstatsr_cpMatVec4",               (DL_FUNC) &bigstatsr_cpMatVec4,                4},
  {"bigstatsr_decodeMat",               (DL_FUNC) &bigstatsr_decodeMat,                2},
  {"bigstatsr_decodeVec",               (DL_FUNC) &bigstatsr_decodeVec,                2},
  {"bigstatsr_incrMat",                 (DL_FUNC) &bigstatsr_incrMat,                  2},
  {"bigstatsr_incrSup2",                (DL_FUNC) &bigstatsr_incrSup2,                 2},
  {"bigstatsr_IRLS",                    (DL_FUNC) &bigstatsr_IRLS,                     9},
  {"bigstatsr_linRegPcadapt_cpp",       (DL_FUNC) &bigstatsr_linRegPcadapt_cpp,        3},
  {"bigstatsr_mycount1",                (DL_FUNC) &bigstatsr_mycount1,                 4},
  {"bigstatsr_mycount2",                (DL_FUNC) &bigstatsr_mycount2,                 4},
  {"bigstatsr_pMatVec4",                (DL_FUNC) &bigstatsr_pMatVec4,                 4},
  {"bigstatsr_scaling",                 (DL_FUNC) &bigstatsr_scaling,                  3},
  {"bigstatsr_transpose3",              (DL_FUNC) &bigstatsr_transpose3,               2},
  {"bigstatsr_univLinReg5",             (DL_FUNC) &bigstatsr_univLinReg5,              5},
  {NULL, NULL, 0}
};

void R_init_bigstatsr(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
