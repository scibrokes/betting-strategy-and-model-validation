## ================== Load Data ================================
## Simulate and save Kelly models for easily loading
source('./function/libs.R')
load('./KellyApps/shinyData.RData', envir = .GlobalEnv)


## --------------- 1. K1 ------------------------
## Reserved Stakes Kelly Models.
K1 <- vKelly(dat)

## --------------- 2. K2 ------------------------
## Reserved EM Probabilities Kelly Models.
K2 <- vKelly2(dat)

## --------------- 3. K1W1 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L)
K1W1 <- vKelly(dat, type = 'weight1')

## --------------- 4. K1W2 ------------------------
## Reserved Stakes Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L)
K1W2 <- vKelly(dat, type = 'weight2')

## --------------- 5. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L)
K2W1 <- vKelly2(dat, type = 'weight1')

## --------------- 6. K2W1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L)
K2W2 <- vKelly2(dat, type = 'weight2')

## --------------- 7. K1W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1WS1 <- vKelly(dat, type = 'weight1', weight.stakes = .....)

## --------------- 8. K1W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1WS2 <- vKelly()

## --------------- 9. K1W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2WS1 <- vKelly()

## --------------- 10. K1W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2WS2 <- vKelly()

## --------------- 11. K2W1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1WS1 <- vKelly()

## --------------- 12. K2W1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1WS2 <- vKelly()

## --------------- 13. K2W2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2WS1 <- vKelly()

## --------------- 14. K2W2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2WS2 <- vKelly()

## --------------- 15. K1D1 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K1D1 <- vKelly()

## --------------- 16. K1D2 ------------------------
## Reserved Stakes Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly()

## --------------- 17. K2D1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic theta values. (mix weighted on W, WH, P, C, LH, L)
K2D1 <- vKelly()

## --------------- 18. K2D2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year until latest soccer match's dynamic dres values. (separate weighted on W, WH, P, C, LH, L)
K2D2 <- vKelly()

## --------------- 19. K1D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1WS1 <- vKelly()

## --------------- 20. K1D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1WS2 <- vKelly()

## --------------- 21. K1D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2WS1 <- vKelly()

## --------------- 22. K1D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2WS2 <- vKelly()

## --------------- 23. K2D1WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1WS1 <- vKelly()

## --------------- 24. K2D1WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1WS2 <- vKelly()

## --------------- 25. K2D2WS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2WS1 <- vKelly()

## --------------- 26. K2D2WS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2WS2 <- vKelly()

## --------------- 27. K1W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W1DWS1 <- vKelly()

## --------------- 28. K1W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W1DWS2 <- vKelly()

## --------------- 29. K1W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1W2DWS1 <- vKelly()

## --------------- 30. K1W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1W2DWS2 <- vKelly()

## --------------- 31. K2W1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W1DWS1 <- vKelly()

## --------------- 32. K2W1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W1DWS2 <- vKelly()

## --------------- 33. K2W2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2W2DWS1 <- vKelly()

## --------------- 34. K2W2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2W2DWS2 <- vKelly()

## --------------- 35. K1D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D1DWS1 <- vKelly()

## --------------- 36. K1D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D1DWS2 <- vKelly()

## --------------- 37. K1D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K1D2DWS1 <- vKelly()

## --------------- 38. K1D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K1D2DWS2 <- vKelly()

## --------------- 39. K2D1DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D1DWS1 <- vKelly()

## --------------- 40. K2D1DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual theta constant value. (mix weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D1DWS2 <- vKelly()

## --------------- 41. K2D2DWS1 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a cross league variance weighted stakes constant.
K2D2DWS1 <- vKelly()

## --------------- 42. K2D2DWS2 ------------------------
## Reserved EM Probabilities Kelly Models with Weight past year annual dres values. (separate weighted on W, WH, P, C, LH, L) plus a separate variance weighted stakes values.
K2D2DWS2 <- vKelly()

















