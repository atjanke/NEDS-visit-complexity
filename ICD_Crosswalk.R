ICD <- ICD %>%
      
    rename.(DX=value) %>%
    mutate.(
      
      
      ICD_52100 = case_when(DX=='K029'   |
                            DX=='52101'  |
                            DX=='52109'  |
                            DX=='K029'   |
                            DX=='K0261'  |
                            TRUE==FALSE ~ 1,T~0),
      ICD_5259 = case_when(DX=='K089'   |
                           DX=='52511'  |
                           DX=='52519'  |
                           DX=='K089'   |
                           TRUE==FALSE ~ 1,T~0),
      ICD_71941 = case_when(DX=='7230'   |
                            DX=='M25519' |
                            TRUE==FALSE ~ 1,T~0),
      ICD_7231 = case_when(DX=='7230'   |
                           DX=='7239'   |
                           DX=='M542'   |
                           TRUE==FALSE ~ 1,T~0),
      ICD_V642 = case_when(DX=='Z5329' |
                           TRUE==FALSE ~ 1,T~0)
      
      )