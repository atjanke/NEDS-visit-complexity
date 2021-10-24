Inputs <- Inputs %>%
  mutate.(ICD_52100= case_when(DX==	'52100' | DX==	'5210' | DX==	'52101' | DX==	'52109' | DX==	'K029' | DX==	'K0261'  ~1,T~0)) %>%											
  mutate.(ICD_5259= case_when(DX==	'5259' | DX==	'5250' | DX==	'52511' | DX==	'52519'  ~1,T~0)) %>%
  mutate.(ICD_71941= case_when(DX==	'71941' | DX==	'71940' | DX==	'M25519'  ~1,T~0)) %>%														
  mutate.(ICD_7231= case_when(DX==	'7231' | DX==	'7230' | DX==	'7239' | DX==	'M542'  ~1,T~0)) %>%													
  mutate.(ICD_V642= case_when(DX==	'V642' | DX==	'Z5329'  ~1,T~0)) %>%															
  mutate.(ICD_8470= case_when(DX==	'8470' | DX==	'S134XXA'  ~1,T~0)) %>%															
  mutate.(ICD_4659= case_when(DX==	'4659' | DX==	'J069'  ~1,T~0)) %>%															
  mutate.(ICD_7862= case_when(DX==	'7862' | DX==	'R05'  ~1,T~0)) %>%															
  mutate.(ICD_78609= case_when(DX==	'78609' | DX==	'R0600' | DX==	'R0609' | DX==	'R063' | DX==	'R0683' ~1,T~0)) %>%										
  mutate.(ICD_3051= case_when(DX==	'3051' | DX==	'F17200'  ~1,T~0)) %>%															
  mutate.(ICD_30000= case_when(DX==	'30000' | DX==	'F419'  ~1,T~0)) %>%															
  mutate.(ICD_6826= case_when(DX==	'6826' | DX==	'L03119' | DX==	'L03129'  ~1,T~0)) %>%														
  mutate.(ICD_6823= case_when(DX==	'6823' | DX==	'L03119' | DX==	'L03129'  ~1,T~0)) %>%														
  mutate.(ICD_78650= case_when(DX==	'78650' | DX==	'R079'  ~1,T~0)) %>%															
  mutate.(ICD_78909= case_when(DX==	'78909' | DX==	'R1010' | DX==	'R102' | DX==	'R1030'  ~1,T~0)) %>%													
  mutate.(ICD_7820= case_when(DX==	'7820' | DX==	'R200' | DX==	'R201' | DX==	'R202' | DX==	'R203' | DX==	'R208' ~1,T~0)) %>%									
  mutate.(ICD_7291= case_when(DX==	'7291' | DX==	'M609' | DX==	'M791' | DX==	'M797'  ~1,T~0)) %>%													
  mutate.(ICD_7295= case_when(DX==	'7295' | DX==	'M79609'  ~1,T~0)) %>%															
  mutate.(ICD_7802= case_when(DX==	'7802' | DX==	'R55'  ~1,T~0)) %>%															
  mutate.(ICD_7851= case_when(DX==	'7851' | DX==	'R002'  ~1,T~0)) %>%															
  mutate.(ICD_4019= case_when(DX==	'4019' | DX==	'I10'  ~1,T~0)) %>%															
  mutate.(ICD_2720= case_when(DX==	'2720' | DX==	'E780'  ~1,T~0)) %>%															
  mutate.(ICD_V5869= case_when(DX==	'V5869' | DX==	'Z79891' | DX==	'Z79899'  ~1,T~0)) %>%														
  mutate.(ICD_2859= case_when(DX==	'2859' | DX==	'D649'  ~1,T~0)) %>%															
  mutate.(ICD_49390= case_when(DX==	'49390' | DX==	'J45909' | DX==	'J45998'  ~1,T~0)) %>%														
  mutate.(ICD_311= case_when(DX==	'311' | DX==	'F329'  ~1,T~0)) %>%															
  mutate.(ICD_71946= case_when(DX==	'71946' | DX==	'M25569'  ~1,T~0)) %>%															
  mutate.(ICD_5990= case_when(DX==	'5990' | DX==	'N390'  ~1,T~0)) %>%															
  mutate.(ICD_5997= case_when(DX==	'5997' | DX==	'59970' | DX==	'59771' | DX==	'59972' | DX==	'R319' | DX==	'R310' | DX==	'R311' | DX==	'R312'  ~1,T~0)) %>%									
  mutate.(ICD_78906= case_when(DX==	'78906' | DX==	'R1013'  ~1,T~0)) %>%															
  mutate.(ICD_78079= case_when(DX==	'78079' | DX==	'G933' | DX==	'R531' | DX==	'R5381' | DX==	'R5383'  ~1,T~0)) %>%												
  mutate.(ICD_78900= case_when(DX==	'78900' | DX==	'R109'  ~1,T~0)) %>%															
  mutate.(ICD_2768= case_when(DX==	'2768' | DX==	'E876'  ~1,T~0)) %>%															
  mutate.(ICD_25000= case_when(DX==	'25000' | DX==	'E119'  ~1,T~0)) %>%															
  mutate.(ICD_84500= case_when(DX==	'84500' | DX==	'S92409A' | DX==	'S96919A'  ~1,T~0)) %>%														
  mutate.(ICD_72981= case_when(DX==	'72981' | DX==	'M7989'  ~1,T~0)) %>%															
  mutate.(ICD_7804= case_when(DX==	'7804' | DX==	'R42'  ~1,T~0)) %>%															
  mutate.(ICD_8830= case_when(DX==	'8830' | DX==	'S61209A'  ~1,T~0)) %>%															
  mutate.(ICD_78703= case_when(DX==	'78703' | DX==	'R1110' | DX==	'R1111' | DX==	'R1112'  ~1,T~0)) %>%													
  mutate.(ICD_92320= case_when(DX==	'92320' | DX==	'S60229A'  ~1,T~0)) %>%															
  mutate.(ICD_920= case_when(DX==	'920' | DX==	'S0093XA'  ~1,T~0)) %>%															
  mutate.(ICD_78659= case_when(DX==	'78659' | DX==	'S1093XA'  ~1,T~0)) %>%															
  mutate.(ICD_V1259= case_when(DX==	'V1259' | DX==	'Z8679'  ~1,T~0)) %>%															
  mutate.(ICD_V1582= case_when(DX==	'V1582' | DX==	'Z87891'  ~1,T~0)) %>%															
  mutate.(ICD_8820= case_when(DX==	'8820' | DX==	'S61409A'  ~1,T~0)) %>%															
  mutate.(ICD_9221= case_when(DX==	'9221' | DX==	'S20219A'  ~1,T~0)) %>%															
  mutate.(ICD_4660= case_when(DX==	'4660' | DX==	'J209'  ~1,T~0)) %>%															
  mutate.(ICD_4739= case_when(DX==	'4739' | DX==	'J329'  ~1,T~0)) %>%															
  mutate.(ICD_7840= case_when(DX==	'7840' | DX==	'G441' | DX==	'R51'  ~1,T~0)) %>%														
  mutate.(ICD_78791= case_when(DX==	'78791' | DX==	'K522' | DX==	'K5298' | DX==	'R197'  ~1,T~0)) %>%													
  mutate.(ICD_64893= case_when(DX==	'64893' | DX==	'O2511' | DX==	'O2512' | DX==	'O2513' | DX==	'O99281' | DX==	'O99282' | DX==	'O99283'  ~1,T~0)) %>%										
  mutate.(ICD_8449= case_when(DX==	'8449' | DX==	'S8390XA' | DX==	'S86919A'  ~1,T~0)) %>%														
  mutate.(ICD_78652= case_when(DX==	'78652' | DX==	'R071' | DX==	'R0781'  ~1,T~0)) %>%														
  mutate.(ICD_4280= case_when(DX==	'4280' | DX==	'I509'  ~1,T~0)) %>%															
  mutate.(ICD_41400= case_when(DX==	'41400' | DX==	'I2510'  ~1,T~0)) %>%															
  mutate.(ICD_486= case_when(DX==	'486' | DX==	'J189'  ~1,T~0)) %>%															
  mutate.(ICD_412= case_when(DX==	'412' | DX==	'I252'  ~1,T~0)) %>%															
  mutate.(ICD_41401= case_when(DX==	'41401' | DX==	'I2510'  ~1,T~0)) %>%															
  mutate.(ICD_2449= case_when(DX==	'2449' | DX==	'E039'  ~1,T~0)) %>%															
  mutate.(ICD_95901= case_when(DX==	'95901' | DX==	'S098XXA' | DX==	'S0990XA'  ~1,T~0)) %>%														
  mutate.(ICD_53081= case_when(DX==	'53081' | DX==	'K219'  ~1,T~0)) %>%															
  mutate.(ICD_462= case_when(DX==	'462' | DX==	'J029'  ~1,T~0)) %>%															
  mutate.(ICD_78701= case_when(DX==	'78701' | DX==	'R112'  ~1,T~0)) %>%															
  mutate.(ICD_56400= case_when(DX==	'56400' | DX==	'K5900'  ~1,T~0)) %>%															
  mutate.(ICD_V065= case_when(DX==	'V065' | DX==	'Z23'  ~1,T~0)) %>%															
  mutate.(ICD_V4581= case_when(DX==	'V4581' | DX==	'Z951'  ~1,T~0)) %>%															
  mutate.(ICD_78039= case_when(DX==	'78039' | DX==	'R569'  ~1,T~0)) %>%															
  mutate.(ICD_490= case_when(DX==	'490' | DX==	'J40'  ~1,T~0)) %>%															
  mutate.(ICD_29680= case_when(DX==	'29680' | DX==	'F319'  ~1,T~0)) %>%															
  mutate.(ICD_3829= case_when(DX==	'3829' | DX==	'H6690'  ~1,T~0)) %>%															
  mutate.(ICD_49392= case_when(DX==	'49392' | DX==	'J45901'  ~1,T~0)) %>%															
  mutate.(ICD_7806= case_when(DX==	'7806' | DX==	'78060' | DX==	'78061' | DX==	'78062' | DX==	'78063' | DX==	'78064' | DX==	'78065' | DX==	'R502' | DX==	'R509' | DX==	'R5081' | DX==	'R5082' | DX==	'R5083' | DX==	'R6883' | DX==	'R680' | DX==	'R5084'  ~1,T~0)) %>%		
  mutate.(ICD_8472= case_when(DX==	'8472' | DX==	'S335XXA'  ~1,T~0)) %>%															
  mutate.(ICD_53550= case_when(DX==	'53550' | DX==	'K2970' | DX==	'K2990'  ~1,T~0)) %>%														
  mutate.(ICD_V583= case_when(DX==	'V583' | DX==	'V5830' | DX==	'V5831' | DX==	'V5832' | DX==	'Z4800' | DX==	'Z4801' | DX==	'Z4802'  ~1,T~0)) %>%										
  mutate.(ICD_27651= case_when(DX==	'27651' | DX==	'E860'  ~1,T~0)) %>%															
  mutate.(ICD_7242= case_when(DX==	'7242' | DX==	'M545'  ~1,T~0)) %>%															
  mutate.(ICD_7245= case_when(DX==	'7245' | DX==	'M5489' | DX==	'M549'  ~1,T~0)) %>%														
  mutate.(ICD_30500= case_when(DX==	'30500' | DX==	'F1010'  ~1,T~0)) %>%															
  mutate.(ICD_92411= case_when(DX==	'92411' | DX==	'S8000XA'  ~1,T~0)) %>%															
  mutate.(ICD_6259= case_when(DX==	'6259' | DX==	'N9489' | DX==	'R102'  ~1,T~0)) %>%														
  mutate.(ICD_42789= case_when(DX==	'42789' | DX==	'I498' | DX==	'R001'  ~1,T~0)) %>%														
  mutate.(ICD_496= case_when(DX==	'496' | DX==	'J449'  ~1,T~0)) %>%															
  mutate.(ICD_42731= case_when(DX==	'42731' | DX==	'I4891'  ~1,T~0)) %>%															
  mutate.(ICD_5589= case_when(DX==	'5589' | DX==	'K5289' | DX==	'K529'  ~1,T~0)) %>%														
  mutate.(ICD_5921= case_when(DX==	'5921' | DX==	'N201'  ~1,T~0)) %>%															
  mutate.(ICD_8730= case_when(DX==	'8730' | DX==	'S0100XA'  ~1,T~0)) %>%															
  mutate.(ICD_87342= case_when(DX==	'87342' | DX==	'S0180XA'  ~1,T~0)) %>%															
  mutate.(ICD_64003= case_when(DX==	'64003' | DX==	'O200'  ~1,T~0)) %>%															
  mutate.(ICD_V4589= case_when(DX==	'V4589' | DX==	'Z9889'  ~1,T~0)) %>%															
  mutate.(ICD_78702= case_when(DX==	'78702' | DX==	'R110'  ~1,T~0)) %>%															
  mutate.(ICD_V5867= case_when(DX==	'V5867' | DX==	'Z794'  ~1,T~0)) %>%															
  mutate.(ICD_2724= case_when(DX==	'2724' | DX==	'E784' | DX==	'E785'  ~1,T~0)) %>%														
  mutate.(ICD_78605= case_when(DX==	'78605' | DX==	'R0602'  ~1,T~0)) %>%															
  mutate.(ICD_5920= case_when(DX==	'5920' | DX==	'N200'  ~1,T~0)) %>%															
  mutate.(ICD_8409= case_when(DX==	'8409' | DX==	'S43409A' | DX==	'S46919A'  ~1,T~0)) %>%														
  mutate.(ICD_V5861= case_when(DX==	'V5861' | DX==	'Z7901'  ~1,T~0)) %>%															
  mutate.(ICD_07999= case_when(DX==	'07999' | DX==	'B9789'  ~1,T~0)) %>%															
  mutate.(ICD_7821= case_when(DX==	'7821' | DX==	'R21'  ~1,T~0)) %>%															
  mutate.(ICD_34690= case_when(DX==	'34690' | DX==	'G43909'  ~1,T~0)) %>%															
  mutate.(ICD_7243= case_when(DX==	'7243' | DX==	'M5430'  ~1,T~0)) %>%															
  mutate.(CPT87491= case_when(DX==	'87491'  ~1,T~0)) %>%																
  mutate.(CPT87591= case_when(DX==	'87591'  ~1,T~0)) %>%																
  mutate.(CPT90772= case_when(DX==	'90772' | DX==	'96372'  ~1,T~0)) %>%															
  mutate.(CPT73130= case_when(DX==	'73130'  ~1,T~0)) %>%																
  mutate.(CPT12002= case_when(DX==	'12002'  ~1,T~0)) %>%																
  mutate.(CPT80053= case_when(DX==	'80053'  ~1,T~0)) %>%																
  mutate.(CPT71020= case_when(DX==	'71020' | DX==	'71045' | DX==	'71046' | DX==	'71047' | DX==	'71048'  ~1,T~0)) %>%												
  mutate.(CPT93005= case_when(DX==	'93005'  ~1,T~0)) %>%																
  mutate.(CPT85025= case_when(DX==	'85025'  ~1,T~0)) %>%																
  mutate.(CPT84484= case_when(DX==	'84484'  ~1,T~0)) %>%																
  mutate.(CPT87086= case_when(DX==	'87086'  ~1,T~0)) %>%																
  mutate.(CPT82550= case_when(DX==	'82550'  ~1,T~0)) %>%																
  mutate.(CPT81003= case_when(DX==	'81003'  ~1,T~0)) %>%																
  mutate.(CPT36415= case_when(DX==	'36415'  ~1,T~0)) %>%																
  mutate.(CPT90765= case_when(DX==	'90765' | DX==	'90721'  ~1,T~0)) %>%															
  mutate.(CPT80048= case_when(DX==	'80048'  ~1,T~0)) %>%																
  mutate.(CPT71010= case_when(DX==	'71010' | DX==	'71045' | DX==	'71046' | DX==	'71047' | DX==	'71048'  ~1,T~0)) %>%												
  mutate.(CPT82553= case_when(DX==	'82553'  ~1,T~0)) %>%																
  mutate.(CPT85027= case_when(DX==	'85027'  ~1,T~0)) %>%																
  mutate.(CPT29125= case_when(DX==	'29125'  ~1,T~0)) %>%																
  mutate.(CPT73110= case_when(DX==	'73110'  ~1,T~0)) %>%																
  mutate.(CPT74020= case_when(DX==	'74020' | DX==	'74018' | DX==	'74019'  ~1,T~0)) %>%														
  mutate.(CPT82150= case_when(DX==	'82150'  ~1,T~0)) %>%																
  mutate.(CPT83690= case_when(DX==	'83690'  ~1,T~0)) %>%																
  mutate.(CPT93010= case_when(DX==	'93010'  ~1,T~0)) %>%																
  mutate.(CPT85379= case_when(DX==	'85379'  ~1,T~0)) %>%																
  mutate.(CPT83874= case_when(DX==	'83874'  ~1,T~0)) %>%																
  mutate.(CPT81001= case_when(DX==	'81001'  ~1,T~0)) %>%																
  mutate.(CPT84703= case_when(DX==	'84703'  ~1,T~0)) %>%																
  mutate.(CPT87210= case_when(DX==	'87210'  ~1,T~0)) %>%																
  mutate.(CPT84443= case_when(DX==	'84443'  ~1,T~0)) %>%																
  mutate.(CPT84702= case_when(DX==	'84702'  ~1,T~0)) %>%																
  mutate.(CPT36000= case_when(DX==	'36000'  ~1,T~0)) %>%																
  mutate.(CPTJ1170= case_when(DX==	'J1170'  ~1,T~0)) %>%																
  mutate.(CPTJ2270= case_when(DX==	'J2270'  ~1,T~0)) %>%																
  mutate.(CPTJ2550= case_when(DX==	'J2550'  ~1,T~0)) %>%																
  mutate.(CPT87088= case_when(DX==	'87088'  ~1,T~0)) %>%																
  mutate.(CPT87186= case_when(DX==	'87186'  ~1,T~0)) %>%																
  mutate.(CPT74150= case_when(DX==	'74150'  ~1,T~0)) %>%																
  mutate.(CPT72192= case_when(DX==	'72192'  ~1,T~0)) %>%																
  mutate.(CPT87077= case_when(DX==	'87077'  ~1,T~0)) %>%																
  mutate.(CPTC8952= case_when(DX==	'C8952'  ~1,T~0)) %>%																
  mutate.(CPT81025= case_when(DX==	'81025'  ~1,T~0)) %>%																
  mutate.(CPT85007= case_when(DX==	'85007'  ~1,T~0)) %>%																
  mutate.(CPT85610= case_when(DX==	'85610'  ~1,T~0)) %>%																
  mutate.(CPT85730= case_when(DX==	'85730'  ~1,T~0)) %>%																
  mutate.(CPT94760= case_when(DX==	'94760'  ~1,T~0)) %>%																
  mutate.(CPT83735= case_when(DX==	'83735'  ~1,T~0)) %>%																
  mutate.(CPT73610= case_when(DX==	'73610'  ~1,T~0)) %>%																
  mutate.(CPT70450= case_when(DX==	'70450'  ~1,T~0)) %>%																
  mutate.(CPT90760= case_when(DX==	'90760' | DX==	'96361'  ~1,T~0)) %>%															
  mutate.(CPTJ7030= case_when(DX==	'J7030'  ~1,T~0)) %>%																
  mutate.(CPT12001= case_when(DX==	'12001'  ~1,T~0)) %>%																
  mutate.(CPTA9270= case_when(DX==	'A9270'  ~1,T~0)) %>%																
  mutate.(CPT93041= case_when(DX==	'93041'  ~1,T~0)) %>%																
  mutate.(CPT83880= case_when(DX==	'83880'  ~1,T~0)) %>%																
  mutate.(CPT87070= case_when(DX==	'87070'  ~1,T~0)) %>%																
  mutate.(CPT87205= case_when(DX==	'87205'  ~1,T~0)) %>%																
  mutate.(CPT10060= case_when(DX==	'10060'  ~1,T~0)) %>%																
  mutate.(CPT90718= case_when(DX==	'90718' | DX==	'90714' | DX==	'90715'  ~1,T~0)) %>%														
  mutate.(CPT72050= case_when(DX==	'72050'  ~1,T~0)) %>%																
  mutate.(CPT90775= case_when(DX==	'90775'  ~1,T~0)) %>%																
  mutate.(CPT80076= case_when(DX==	'80076'  ~1,T~0)) %>%																
  mutate.(CPT94761= case_when(DX==	'94761'  ~1,T~0)) %>%																
  mutate.(CPT90774= case_when(DX==	'90774' | DX==	'96372'  ~1,T~0)) %>%															
  mutate.(CPTJ1885= case_when(DX==	'J1885'  ~1,T~0)) %>%																
  mutate.(CPTJ2765= case_when(DX==	'J2765'  ~1,T~0)) %>%																
  mutate.(CPT81015= case_when(DX==	'81015'  ~1,T~0)) %>%																
  mutate.(CPT72193= case_when(DX==	'72193'  ~1,T~0)) %>%																
  mutate.(CPT74160= case_when(DX==	'74160'  ~1,T~0)) %>%																
  mutate.(CPTQ9949= case_when(DX==	'Q9949'  ~1,T~0)) %>%																
  mutate.(CPT87040= case_when(DX==	'87040'  ~1,T~0)) %>%																
  mutate.(CPT94640= case_when(DX==	'94640'  ~1,T~0)) %>%																
  mutate.(CPT73510= case_when(DX==	'73510'  ~1,T~0)) %>%																
  mutate.(CPTC8950= case_when(DX==	'C8950'  ~1,T~0)) %>%																
  mutate.(CPTC8951= case_when(DX==	'C8951'  ~1,T~0)) %>%																
  mutate.(CPT74022= case_when(DX==	'74022'  ~1,T~0)) %>%																
  mutate.(CPT90471= case_when(DX==	'90471'  ~1,T~0)) %>%																
  mutate.(CPT87880= case_when(DX==	'87880'  ~1,T~0)) %>%																
  mutate.(CPT94664= case_when(DX==	'94664'  ~1,T~0)) %>%																
  mutate.(CPT93971= case_when(DX==	'93971'  ~1,T~0)) %>%																
  mutate.(CPT72110= case_when(DX==	'72110'  ~1,T~0)) %>%																
  mutate.(CPT82055= case_when(DX==	'82055' | DX==	'G6040'  ~1,T~0)) %>%															
  mutate.(CPT80100= case_when(DX==	'80100'  ~1,T~0)) %>%																
  mutate.(CPT84100= case_when(DX==	'84100'  ~1,T~0)) %>%																
  mutate.(CPT73630= case_when(DX==	'73630'  ~1,T~0)) %>%																
  mutate.(CPT29515= case_when(DX==	'29515'  ~1,T~0)) %>%																
  mutate.(CPT76705= case_when(DX==	'76705'  ~1,T~0)) %>%																
  mutate.(CPT81002= case_when(DX==	'81002'  ~1,T~0)) %>%																
  mutate.(CPT73030= case_when(DX==	'73030'  ~1,T~0)) %>%																
  mutate.(CPT51702= case_when(DX==	'51702'  ~1,T~0)) %>%																
  mutate.(CPT72125= case_when(DX==	'72125'  ~1,T~0)) %>%																
  mutate.(CPT82003= case_when(DX==	'82003'  ~1,T~0)) %>%																
  mutate.(CPT80196= case_when(DX==	'80196'  ~1,T~0)) %>%																
  mutate.(CPT72040= case_when(DX==	'72040'  ~1,T~0)) %>%																
  mutate.(CPT72170= case_when(DX==	'72170'  ~1,T~0)) %>%																
  mutate.(CPT73564= case_when(DX==	'73564'  ~1,T~0)) %>%																
  mutate.(CPTJ2405= case_when(DX==	'J2405'  ~1,T~0)) %>%																
  mutate.(CPT73140= case_when(DX==	'73140'  ~1,T~0)) %>%																
  mutate.(CPT71260= case_when(DX==	'71260'  ~1,T~0)) %>%																
  mutate.(CPT80101= case_when(DX==	'80101'  ~1,T~0)) %>%																
  mutate.(CPT90761= case_when(DX==	'90761'  ~1,T~0)) %>%																
  mutate.(CPT73562= case_when(DX==	'73562'  ~1,T~0)) %>%																
  mutate.(CPT72100= case_when(DX==	'72100'  ~1,T~0)) %>%																
  mutate.(CPT76830= case_when(DX==	'76830'  ~1,T~0)) %>%																
  mutate.(CPT76856= case_when(DX==	'76856'  ~1,T~0)) %>%																
  mutate.(CPT73080= case_when(DX==	'73080'  ~1,T~0)) %>%																
  mutate.(CPT84132= case_when(DX==	'84132'  ~1,T~0)) %>%																
  mutate.(CPT82565= case_when(DX==	'82565'  ~1,T~0)) %>%																
  mutate.(CPT84520= case_when(DX==	'84520'  ~1,T~0)) %>%																
  mutate.(CPT87081= case_when(DX==	'87081'  ~1,T~0)) %>%																
  mutate.(CPT82803= case_when(DX==	'82803'  ~1,T~0)) %>%																
  mutate.(CPT82947= case_when(DX==	'82947'  ~1,T~0)) %>%																
  mutate.(CPT82962= case_when(DX==	'82962'  ~1,T~0)) %>%																
  mutate.(CPT86900= case_when(DX==	'86900'  ~1,T~0)) %>%																
  mutate.(CPT86901= case_when(DX==	'86901'  ~1,T~0)) %>%																
  mutate.(CPT86850= case_when(DX==	'86850'  ~1,T~0)) %>%																
  mutate.(CPT74000= case_when(DX==	'74000'  ~1,T~0)) %>%																
  mutate.(CPT81000= case_when(DX==	'81000'  ~1,T~0)) %>%																
  mutate.(CPT73590= case_when(DX==	'73590'  ~1,T~0))															