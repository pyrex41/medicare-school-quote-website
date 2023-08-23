module Presets exposing (..)

-- Use https://content.naic.org/cis_consumer_information.htm to look up NAIC codes
naicCategory =
    { preferred =
        [ 10345
        , 12321
        , 13100
        , 28207
        , 38520
        , 47171
        , 47570
        , 52618
        , 53228
        , 53287
        , 53473
        , 53589
        , 53902
        , 54631
        , 54720
        , 54828
        , 54933
        , 55026
        , 60093
        , 60131
        , 60219
        , 61557
        , 61727
        , 62825
        , 63444
        , 65722
        , 67369
        , 68500
        , 69868
        , 70580
        , 70670
        , 71412
        , 71835
        , 72052
        , 72850
        , 73288
        , 77780
        , 78700
        , 79413
        , 88366
        , 95120
        , 95610
        , 96016
        , 96202
        , 98167
        , 22667
        , 50028
        , 20702
        , 26417
        , 60348
        , 20699
        , 60380
        , 82538
        ]
    , nonPreferred =
        [ 79413001 -- custom workaround for level 1 UHC
        , 79413002 -- custom workaround for level 2 UHC
        , 88366001 -- custom workaround for CIGNA Substandard
        , 61727001 -- custom workaround for CIGNA Standard II
        , 31119
        , 61859
        , 63967
        , 64211
        , 65641
        , 65870
        , 66214
        , 66281
        , 67059
        , 67326
        , 68462
        , 68845
        , 76112
        , 79987
        , 86231
        , 82538001 -- custom workaround for Allstate discounts
        ]
    , outside =
        [ 11121
        , 12358
        , 14933
        , 19178
        , 25178
        , 26581
        , 26921
        , 29076
        , 47027
        , 47051
        , 47058
        , 47350
        , 53139
        , 53872
        , 54704
        , 54771
        , 55891
        , 56014
        , 56383
        , 56499
        , 56693
        , 56820
        , 57053
        , 57347
        , 57991
        , 60016
        , 60128
        , 60176
        , 60183
        , 60836
        , 61115
        , 61239
        , 61700
        , 61751
        , 61999
        , 62065
        , 62146
        , 62553
        , 66141
        , 66583
        , 66828
        , 67539
        , 67628
        , 67679
        , 67784
        , 67814
        , 68420
        , 68543
        , 68802
        , 69132
        , 69663
        , 69698
        , 70122
        , 70408
        , 70769
        , 70939
        , 70998
        , 71390
        , 71773
        , 71919
        , 73504
        , 77216
        , 77828
        , 77950
        , 78743
        , 80578
        , 81043
        , 81200
        , 81701
        , 81779
        , 82538
        , 82880
        , 85189
        , 89005
        , 90212
        , 91472
        , 91785
        , 92916
        , 94587
        , 95561
        , 95683
        , 95725
        , 95796
        , 95839
        , 95844
        , 95923
        ]
    }

displayNames =
    { preferred =
        [ ( 10345, "Anthem" )
        , ( 12321, "AETNA" )
        , ( 13100, "Mutual of Omaha" )
        , ( 28207, "Anthem" )
        , ( 38520, "BCBS of SC" )
        , ( 47171, "BCBS of KC" )
        , ( 47570, "BCBS Premera" )
        , ( 52618, "Anthem of ME" )
        , ( 53228, "BCBS of MA" )
        , ( 53287, "BCBS Highmark" )
        , ( 53473, "BCBS of RI" )
        , ( 53589, "BCBS of AZ" )
        , ( 53902, "BCBS Regence" )
        , ( 54631, "BCBS of NC" )
        , ( 54720, "BCBS of PA" )
        , ( 54828, "BCBS of WV" )
        , ( 54933, "BCBS of OR" )
        , ( 55026, "BCBS of MN" )
        , ( 60093, "AARP of NY" )
        , ( 60131, "BCBS of ID" )
        , ( 60219, "Humana" )
        , ( 61557, "BCBS of CA" )
        , ( 61727, "CIGNA" )
        , ( 62825, "Anthem BCBS" )
        , ( 63444, "Accendo" )
        , ( 65722, "CIGNA" )
        , ( 67369, "CIGNA" )
        , ( 68500, "AETNA" )
        , ( 69868, "Mutual of Omaha" )
        , ( 70580, "Humana" )
        , ( 70670, "BCBS IL/TX/NM/OK" )
        , ( 71412, "Mutual of Omaha" )
        , ( 71835, "Anthem VA/NV" )
        , ( 72052, "AETNA" )
        , ( 72850, "Mutual of Omaha" )
        , ( 73288, "Humana" )
        , ( 77780, "BCBS of NE" )
        , ( 78700, "AETNA" )
        , ( 79413, "AARP / UHC" )
        , ( 88366, "CIGNA" )
        , ( 95120, "Anthem of KY" )
        , ( 95610, "BCBS of MI" )
        , ( 96016, "AARP of AZ" )
        , ( 96202, "BCBS CareFirst" )
        , ( 98167, "BCBS of FL" )
        , ( 22667, "ACE / CHUBB" )
        , ( 50028, "ACE / CHUBB" )
        , ( 20702, "ACE / CHUBB" )
        , ( 26417, "ACE / CHUBB" )
        , ( 60348, "ACE / CHUBB" )
        , ( 20699, "ACE / CHUBB" )
        , ( 60380, "AFLAC" )
        , ( 82538, "Allstate" )
        ]
    , nonPreferred =
        [ ( 31119, "Medico" )
        , ( 61859, "Christian Fidelity" )
        , ( 63967, "GPM" )
        , ( 64211, "GTL" )
        , ( 65641, "Medico" )
        , ( 65870, "Manhattan Life" )
        , ( 66214, "Heartland" )
        , ( 66281, "Transamerica" )
        , ( 67059, "GPM" )
        , ( 67326, "Old Surety" )
        , ( 68462, "Reserve National" )
        , ( 68845, "Shenandoah" )
        , ( 76112, "Oxford" )
        , ( 79987, "Medico" )
        , ( 86231, "Transamerica" )
        ]
    , outside =
        [ ( 11121, "UNIFIED LIFE INSURANCE COMPANY" )
        , ( 12358, "Avalon Insurance Company" )
        , ( 14933, "Montana Health Cooperative" )
        , ( 19178, "Southern Guaranty Insurance Company" )
        , ( 25178, "State Farm Mutual Automobile Insurance Company" )
        , ( 26581, "INDEPENDENCE AMERICAN INSURANCE COMPANY" )
        , ( 26921, "EVEREST REINSURANCE COMPANY" )
        , ( 29076, "Medical Mutual of Ohio" )
        , ( 47027, "CDPHP Universal Benefits, Inc." )
        , ( 47051, "FirstCommunity Health Plan Inc." )
        , ( 47058, "CAREFIRST OF MARYLAND, INC." )
        , ( 47350, "ASURIS NORTHWEST HEALTH" )
        , ( 53139, "Wisconsin Physicians Service Insurance Corporation" )
        , ( 53872, "KPS Health Plans" )
        , ( 54704, "Independence Hospital Indemnity Plan, Inc." )
        , ( 54771, "Highmark Inc." )
        , ( 55891, "Noridian Mutual Insurance Company" )
        , ( 56014, "Thrivent Financial for Lutherans" )
        , ( 56383, "The Order Of United Commercial Travelers Of America" )
        , ( 56499, "Assured Life Association" )
        , ( 56693, "Greek Catholic Union Of The USA" )
        , ( 56820, "Polish Falcons of America" )
        , ( 57053, "Catholic United Financial" )
        , ( 57347, "Catholic Life Insurance" )
        , ( 57991, "Everence Association Inc" )
        , ( 60016, "THP Insurance Company" )
        , ( 60128, "Wellmark of South Dakota, Inc." )
        , ( 60176, "Prosperity Life Group" )
        , ( 60183, "Prosperity Life Group" )
        , ( 60836, "AMERICAN REPUBLIC INSURANCE COMPANY" )
        , ( 61115, "ATLANTIC COAST LIFE INSURANCE COMPANY" )
        , ( 61239, "BANKERS FIDELITY LIFE INSURANCE COMPANY" )
        , ( 61700, "Renaissance Life & Health Insurance Company of America" )
        , ( 61751, "Central States Health and Life Co. of Omaha" )
        , ( 61999, "Great Southern Life Insurance Company" )
        , ( 62065, "COLONIAL PENN LIFE INSURANCE COMPANY" )
        , ( 62146, "Combined Insurance Company of America" )
        , ( 62553, "COUNTRY Life Insurance Company" )
        , ( 66141, "HEALTH NET LIFE INSURANCE COMPANY" )
        , ( 66583, "National Guardian Life Insurance Company" )
        , ( 66828, "FALLON HEALTH AND LIFE ASSURANCE CO. INC." )
        , ( 67539, "Pan American Life Insurance Company" )
        , ( 67628, "PEKIN LIFE INSURANCE COMPANY" )
        , ( 67679, "AMERICAN REPUBLIC CORP INSURANCE COMPANY" )
        , ( 67784, "Philadelphia American Life Insurance Company" )
        , ( 67814, "Nassau Life Insurance Company" )
        , ( 68420, "WMI MUTUAL INSURANCE COMPANY" )
        , ( 68543, "Liberty Bankers Life Insurance Company" )
        , ( 68802, "Sentinel Security Life Insurance Company" )
        , ( 69132, "STATE MUTUAL INSURANCE COMPANY" )
        , ( 69663, "USAA LIFE INSURANCE COMPANY" )
        , ( 69698, "New Era Life Insurance Company of the Midwest" )
        , ( 70122, "UNIVERSAL FIDELITY LIFE INSURANCE COMPANY" )
        , ( 70408, "UNION SECURITY INSURANCE COMPANY" )
        , ( 70769, "Erie Family Life Insurance Company" )
        , ( 70939, "GERBER LIFE INSURANCE COMPANY" )
        , ( 70998, "QualChoice Life and Health Insurance Company, Inc." )
        , ( 71390, "Puritan Life Insurance Company of America" )
        , ( 71773, "American National Life Insurance Company of Texas" )
        , ( 71919, "Bankers Fidelity Assurance Company" )
        , ( 73504, "Lumico Life Insurance Company" )
        , ( 77216, "Aultcare Insurance Company" )
        , ( 77828, "Companion Life Insurance Company" )
        , ( 77950, "HEALTH ALLIANCE MEDICAL PLANS, INC." )
        , ( 78743, "New Era Life Insurance Company" )
        , ( 80578, "PHYSICIANS MUTUAL INSURANCE COMPANY" )
        , ( 81043, "Bankers Life Insurance Company" )
        , ( 81200, "Louisiana Health Service and Indemnity Company" )
        , ( 81701, "Educators Mutual Insurance Association" )
        , ( 81779, "INDIVIDUAL ASSURANCE COMPANY, LIFE, HEALTH & ACCIDENT" )
        , ( 82538, "National Health Insurance Company" )
        , ( 82880, "CSI Life Insurance Company" )
        , ( 85189, "Western United Life Assurance Company" )
        , ( 89005, "Farm Bureau Health Plans" )
        , ( 90212, "Great Southern Life Insurance Company (Americo)" )
        , ( 91472, "GLOBE LIFE AND ACCIDENT INSURANCE COMPANY" )
        , ( 91785, "Equitable National Life Insurance Company" )
        , ( 92916, "UNITED AMERICAN INSURANCE COMPANY" )
        , ( 94587, "Members Health Insurance Company" )
        , ( 95561, "Priority Health" )
        , ( 95683, "Sanford Health Plan" )
        , ( 95725, "SANFORD HEALTH PLAN OF MINNESOTA" )
        , ( 95796, "Unity Health Plans Insurance Corporation" )
        , ( 95839, "AVERA HEALTH PLANS, INC." )
        , ( 95844, "Health Alliance Plan of Michigan" )
        , ( 95923, "Geisinger Health Plan" )
        ]
    }
