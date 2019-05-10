
.getCutoff <- function(Obs, Fit)
{
  SumObs = sum(Obs)
  LengObs = length(Obs)
  tt = c(100)
  
  Quant = quantile(Fit)
  if (sum(Quant) > 0)
  {
    i = Quant[2]
    a = 2
    while (i <= Quant[5])
    {
      se = sum((Fit >= i)[Obs == 1]) / SumObs
      sp = sum((Fit < i)[Obs == 0]) / (LengObs - SumObs)
      tt[a] = abs(se - sp)
      if (tt[a] > tt[a - 1])
        break
      i = i + ((Quant[5] - Quant[2]) / 1000)
      a = a + 1
    }
    
    b = (i - ((Quant[5] - Quant[2]) / 1000))
    Cut = as.numeric(b)
    
    Sensitivity = 100 * sum((Fit >= b)[Obs == 1]) / SumObs
    Specificity = 100 * sum((Fit < b)[Obs == 0]) / (LengObs - SumObs)
    
    return(list(Cut = Cut
                , Sensitivity = Sensitivity
                , Specificity = Specificity
    ))
  } else
  {
    return(NA)
  }
}