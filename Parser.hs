{-# OPTIONS_GHC -w #-}
module Parser where
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import WData
import LData
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,293) ([0,0,2048,0,192,296,0,0,32,0,0,4,32768,37893,0,0,4096,0,0,0,0,0,1,0,0,0,0,512,0,24576,9473,0,0,1,0,0,0,0,0,0,0,6,0,49152,18946,0,22528,2368,0,0,0,0,32768,0,0,0,0,0,256,0,0,64,0,0,0,0,32768,0,0,16390,9,0,72,0,0,0,0,0,0,0,96,148,0,32780,18,0,0,0,0,256,0,0,0,0,0,2,0,24,1,0,0,4,24576,46080,0,3072,4992,0,0,512,0,48,74,0,16390,9,49152,0,0,0,8192,0,3072,0,0,0,128,0,32944,22,0,28694,2,0,16384,0,22528,2368,0,2816,296,0,352,37,0,0,4,0,0,0,45056,4736,0,5632,600,0,32768,0,0,0,0,0,0,1,0,0,0,4096,1056,0,0,0,0,1152,0,0,0,0,12288,19200,0,0,16,0,0,0,0,0,32,0,0,0,24576,33794,0,0,0,0,384,16,0,0,4,0,384,0,0,0,0,0,0,0,4864,1056,0,0,0,0,16384,0,32768,4097,0,12288,18944,0,0,0,0,0,256,0,0,32,0,192,0,0,0,0,16384,4224,0,0,0,0,0,1,0,16416,8,0,10251,1,24576,9489,0,49152,4,0,0,128,0,768,1,0,6,0,49152,0,0,0,32,0,0,2,0,4120,37,0,1024,0,0,32768,0,0,259,0,0,32,0,48,2,0,16390,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,32768,20481,2,0,0,0,1536,64,0,0,256,0,128,33,0,8208,4,0,0,0,0,2048,0,5632,592,0,0,0,0,16472,9,0,10251,1,0,6,0,0,0,0,16384,0,0,4096,0,32768,20481,2,12288,18944,0,0,0,0,192,296,0,0,0,0,0,0,24576,37888,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse1","%start_parse2","%start_parse3","%start_parse4","%start_parse5","%start_parse6","Env","E1","Store","S","Term","Qual","Type","PreType","Values","PreValues","LEnv","LE1","LStore","LS","LTerm","Pi","LType","\"lin\"","\"un\"","\"1\"","\"w\"","\"Bool\"","'\\\\'","\"->\"","'*'","'<'","'>'","'.'","':'","','","'('","')'","split","as","in","var","%eof"]
        bit_start = st * 45
        bit_end = (st + 1) * 45
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..44]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (44) = happyShift action_33
action_0 (9) = happyGoto action_31
action_0 (10) = happyGoto action_32
action_0 _ = happyReduce_6

action_1 (26) = happyShift action_26
action_1 (27) = happyShift action_27
action_1 (39) = happyShift action_28
action_1 (41) = happyShift action_29
action_1 (44) = happyShift action_30
action_1 (13) = happyGoto action_24
action_1 (14) = happyGoto action_25
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (44) = happyShift action_23
action_2 (11) = happyGoto action_21
action_2 (12) = happyGoto action_22
action_2 _ = happyReduce_10

action_3 (44) = happyShift action_20
action_3 (19) = happyGoto action_18
action_3 (20) = happyGoto action_19
action_3 _ = happyReduce_32

action_4 (28) = happyShift action_12
action_4 (29) = happyShift action_13
action_4 (31) = happyShift action_14
action_4 (39) = happyShift action_15
action_4 (41) = happyShift action_16
action_4 (44) = happyShift action_17
action_4 (23) = happyGoto action_10
action_4 (24) = happyGoto action_11
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (44) = happyShift action_9
action_5 (21) = happyGoto action_7
action_5 (22) = happyGoto action_8
action_5 _ = happyReduce_36

action_6 _ = happyFail (happyExpListPerState 6)

action_7 (38) = happyShift action_51
action_7 (45) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_38

action_9 (37) = happyShift action_50
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (28) = happyShift action_12
action_10 (29) = happyShift action_13
action_10 (31) = happyShift action_14
action_10 (39) = happyShift action_15
action_10 (41) = happyShift action_16
action_10 (44) = happyShift action_17
action_10 (45) = happyAccept
action_10 (23) = happyGoto action_49
action_10 (24) = happyGoto action_11
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (34) = happyShift action_48
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_46

action_13 _ = happyReduce_47

action_14 (28) = happyShift action_12
action_14 (29) = happyShift action_13
action_14 (24) = happyGoto action_47
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (28) = happyShift action_12
action_15 (29) = happyShift action_13
action_15 (31) = happyShift action_14
action_15 (39) = happyShift action_15
action_15 (41) = happyShift action_16
action_15 (44) = happyShift action_17
action_15 (23) = happyGoto action_46
action_15 (24) = happyGoto action_11
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (28) = happyShift action_12
action_16 (29) = happyShift action_13
action_16 (31) = happyShift action_14
action_16 (39) = happyShift action_15
action_16 (41) = happyShift action_16
action_16 (44) = happyShift action_17
action_16 (23) = happyGoto action_45
action_16 (24) = happyGoto action_11
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_40

action_18 (38) = happyShift action_44
action_18 (45) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_34

action_20 (37) = happyShift action_43
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (38) = happyShift action_42
action_21 (45) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_12

action_23 (37) = happyShift action_41
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (26) = happyShift action_26
action_24 (27) = happyShift action_27
action_24 (39) = happyShift action_28
action_24 (41) = happyShift action_29
action_24 (44) = happyShift action_30
action_24 (45) = happyAccept
action_24 (13) = happyGoto action_40
action_24 (14) = happyGoto action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (31) = happyShift action_38
action_25 (34) = happyShift action_39
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_20

action_27 _ = happyReduce_21

action_28 (26) = happyShift action_26
action_28 (27) = happyShift action_27
action_28 (39) = happyShift action_28
action_28 (41) = happyShift action_29
action_28 (44) = happyShift action_30
action_28 (13) = happyGoto action_37
action_28 (14) = happyGoto action_25
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (26) = happyShift action_26
action_29 (27) = happyShift action_27
action_29 (39) = happyShift action_28
action_29 (41) = happyShift action_29
action_29 (44) = happyShift action_30
action_29 (13) = happyGoto action_36
action_29 (14) = happyGoto action_25
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_14

action_31 (38) = happyShift action_35
action_31 (45) = happyAccept
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_8

action_33 (37) = happyShift action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (26) = happyShift action_26
action_34 (27) = happyShift action_27
action_34 (39) = happyShift action_70
action_34 (14) = happyGoto action_68
action_34 (15) = happyGoto action_69
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (44) = happyShift action_33
action_35 (10) = happyGoto action_67
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (26) = happyShift action_26
action_36 (27) = happyShift action_27
action_36 (39) = happyShift action_28
action_36 (41) = happyShift action_29
action_36 (42) = happyShift action_66
action_36 (44) = happyShift action_30
action_36 (13) = happyGoto action_40
action_36 (14) = happyGoto action_25
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (26) = happyShift action_26
action_37 (27) = happyShift action_27
action_37 (39) = happyShift action_28
action_37 (40) = happyShift action_65
action_37 (41) = happyShift action_29
action_37 (44) = happyShift action_30
action_37 (13) = happyGoto action_40
action_37 (14) = happyGoto action_25
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (44) = happyShift action_64
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (26) = happyShift action_26
action_39 (27) = happyShift action_27
action_39 (39) = happyShift action_28
action_39 (41) = happyShift action_29
action_39 (44) = happyShift action_30
action_39 (13) = happyGoto action_63
action_39 (14) = happyGoto action_25
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (26) = happyShift action_26
action_40 (27) = happyShift action_27
action_40 (39) = happyShift action_28
action_40 (41) = happyShift action_29
action_40 (44) = happyShift action_30
action_40 (13) = happyGoto action_40
action_40 (14) = happyGoto action_25
action_40 _ = happyReduce_18

action_41 (26) = happyShift action_26
action_41 (27) = happyShift action_27
action_41 (14) = happyGoto action_61
action_41 (17) = happyGoto action_62
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (44) = happyShift action_23
action_42 (12) = happyGoto action_60
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (28) = happyShift action_12
action_43 (29) = happyShift action_13
action_43 (24) = happyGoto action_59
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (44) = happyShift action_20
action_44 (20) = happyGoto action_58
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (28) = happyShift action_12
action_45 (29) = happyShift action_13
action_45 (31) = happyShift action_14
action_45 (39) = happyShift action_15
action_45 (41) = happyShift action_16
action_45 (42) = happyShift action_57
action_45 (44) = happyShift action_17
action_45 (23) = happyGoto action_49
action_45 (24) = happyGoto action_11
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (28) = happyShift action_12
action_46 (29) = happyShift action_13
action_46 (31) = happyShift action_14
action_46 (39) = happyShift action_15
action_46 (40) = happyShift action_56
action_46 (41) = happyShift action_16
action_46 (44) = happyShift action_17
action_46 (23) = happyGoto action_49
action_46 (24) = happyGoto action_11
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (44) = happyShift action_55
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (28) = happyShift action_12
action_48 (29) = happyShift action_13
action_48 (31) = happyShift action_14
action_48 (39) = happyShift action_15
action_48 (41) = happyShift action_16
action_48 (44) = happyShift action_17
action_48 (23) = happyGoto action_54
action_48 (24) = happyGoto action_11
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (28) = happyShift action_12
action_49 (29) = happyShift action_13
action_49 (31) = happyShift action_14
action_49 (39) = happyShift action_15
action_49 (41) = happyShift action_16
action_49 (44) = happyShift action_17
action_49 (23) = happyGoto action_49
action_49 (24) = happyGoto action_11
action_49 _ = happyReduce_44

action_50 (28) = happyShift action_12
action_50 (29) = happyShift action_13
action_50 (31) = happyShift action_14
action_50 (39) = happyShift action_15
action_50 (41) = happyShift action_16
action_50 (44) = happyShift action_17
action_50 (23) = happyGoto action_53
action_50 (24) = happyGoto action_11
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (44) = happyShift action_9
action_51 (22) = happyGoto action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_37

action_53 (28) = happyShift action_12
action_53 (29) = happyShift action_13
action_53 (31) = happyShift action_14
action_53 (39) = happyShift action_15
action_53 (41) = happyShift action_16
action_53 (44) = happyShift action_17
action_53 (23) = happyGoto action_49
action_53 (24) = happyGoto action_11
action_53 _ = happyReduce_39

action_54 (28) = happyShift action_12
action_54 (29) = happyShift action_13
action_54 (31) = happyShift action_14
action_54 (38) = happyShift action_89
action_54 (39) = happyShift action_15
action_54 (41) = happyShift action_16
action_54 (44) = happyShift action_17
action_54 (23) = happyGoto action_49
action_54 (24) = happyGoto action_11
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (37) = happyShift action_88
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_45

action_57 (44) = happyShift action_87
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_33

action_59 (30) = happyShift action_84
action_59 (39) = happyShift action_85
action_59 (44) = happyShift action_86
action_59 (25) = happyGoto action_83
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_11

action_61 (31) = happyShift action_81
action_61 (34) = happyShift action_82
action_61 (18) = happyGoto action_80
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_13

action_63 (26) = happyShift action_26
action_63 (27) = happyShift action_27
action_63 (38) = happyShift action_79
action_63 (39) = happyShift action_28
action_63 (41) = happyShift action_29
action_63 (44) = happyShift action_30
action_63 (13) = happyGoto action_40
action_63 (14) = happyGoto action_25
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (37) = happyShift action_78
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_19

action_66 (44) = happyShift action_77
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_7

action_68 (26) = happyShift action_26
action_68 (27) = happyShift action_27
action_68 (30) = happyShift action_74
action_68 (39) = happyShift action_75
action_68 (44) = happyShift action_76
action_68 (14) = happyGoto action_68
action_68 (15) = happyGoto action_72
action_68 (16) = happyGoto action_73
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_9

action_70 (26) = happyShift action_26
action_70 (27) = happyShift action_27
action_70 (39) = happyShift action_70
action_70 (14) = happyGoto action_68
action_70 (15) = happyGoto action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (40) = happyShift action_105
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (32) = happyShift action_103
action_72 (33) = happyShift action_104
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_23

action_74 _ = happyReduce_24

action_75 (26) = happyShift action_26
action_75 (27) = happyShift action_27
action_75 (30) = happyShift action_74
action_75 (39) = happyShift action_75
action_75 (44) = happyShift action_76
action_75 (14) = happyGoto action_68
action_75 (15) = happyGoto action_101
action_75 (16) = happyGoto action_102
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_27

action_77 (38) = happyShift action_100
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (26) = happyShift action_26
action_78 (27) = happyShift action_27
action_78 (39) = happyShift action_70
action_78 (14) = happyGoto action_68
action_78 (15) = happyGoto action_99
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (26) = happyShift action_26
action_79 (27) = happyShift action_27
action_79 (39) = happyShift action_28
action_79 (41) = happyShift action_29
action_79 (44) = happyShift action_30
action_79 (13) = happyGoto action_98
action_79 (14) = happyGoto action_25
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_29

action_81 (44) = happyShift action_97
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (44) = happyShift action_96
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (32) = happyShift action_94
action_83 (33) = happyShift action_95
action_83 _ = happyReduce_35

action_84 _ = happyReduce_48

action_85 (30) = happyShift action_84
action_85 (39) = happyShift action_85
action_85 (44) = happyShift action_86
action_85 (25) = happyGoto action_93
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_51

action_87 (38) = happyShift action_92
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (30) = happyShift action_84
action_88 (39) = happyShift action_85
action_88 (44) = happyShift action_86
action_88 (25) = happyGoto action_91
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (28) = happyShift action_12
action_89 (29) = happyShift action_13
action_89 (31) = happyShift action_14
action_89 (39) = happyShift action_15
action_89 (41) = happyShift action_16
action_89 (44) = happyShift action_17
action_89 (23) = happyGoto action_90
action_89 (24) = happyGoto action_11
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (28) = happyShift action_12
action_90 (29) = happyShift action_13
action_90 (31) = happyShift action_14
action_90 (35) = happyShift action_119
action_90 (39) = happyShift action_15
action_90 (41) = happyShift action_16
action_90 (44) = happyShift action_17
action_90 (23) = happyGoto action_49
action_90 (24) = happyGoto action_11
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (32) = happyShift action_94
action_91 (33) = happyShift action_95
action_91 (36) = happyShift action_118
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (44) = happyShift action_117
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (32) = happyShift action_94
action_93 (33) = happyShift action_95
action_93 (40) = happyShift action_116
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (28) = happyShift action_12
action_94 (29) = happyShift action_13
action_94 (24) = happyGoto action_115
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (28) = happyShift action_12
action_95 (29) = happyShift action_13
action_95 (24) = happyGoto action_114
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (38) = happyShift action_113
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (37) = happyShift action_112
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (26) = happyShift action_26
action_98 (27) = happyShift action_27
action_98 (35) = happyShift action_111
action_98 (39) = happyShift action_28
action_98 (41) = happyShift action_29
action_98 (44) = happyShift action_30
action_98 (13) = happyGoto action_40
action_98 (14) = happyGoto action_25
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (36) = happyShift action_110
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (44) = happyShift action_109
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (32) = happyShift action_103
action_101 (33) = happyShift action_104
action_101 (40) = happyShift action_105
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (40) = happyShift action_108
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (26) = happyShift action_26
action_103 (27) = happyShift action_27
action_103 (39) = happyShift action_70
action_103 (14) = happyGoto action_68
action_103 (15) = happyGoto action_107
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (26) = happyShift action_26
action_104 (27) = happyShift action_27
action_104 (39) = happyShift action_70
action_104 (14) = happyGoto action_68
action_104 (15) = happyGoto action_106
action_104 _ = happyFail (happyExpListPerState 104)

action_105 _ = happyReduce_22

action_106 _ = happyReduce_26

action_107 _ = happyReduce_25

action_108 _ = happyReduce_28

action_109 (43) = happyShift action_127
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (26) = happyShift action_26
action_110 (27) = happyShift action_27
action_110 (39) = happyShift action_28
action_110 (41) = happyShift action_29
action_110 (44) = happyShift action_30
action_110 (13) = happyGoto action_126
action_110 (14) = happyGoto action_25
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_15

action_112 (26) = happyShift action_26
action_112 (27) = happyShift action_27
action_112 (39) = happyShift action_70
action_112 (14) = happyGoto action_68
action_112 (15) = happyGoto action_125
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (44) = happyShift action_124
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (30) = happyShift action_84
action_114 (39) = happyShift action_85
action_114 (44) = happyShift action_86
action_114 (25) = happyGoto action_123
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (30) = happyShift action_84
action_115 (39) = happyShift action_85
action_115 (44) = happyShift action_86
action_115 (25) = happyGoto action_122
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_52

action_117 (43) = happyShift action_121
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (28) = happyShift action_12
action_118 (29) = happyShift action_13
action_118 (31) = happyShift action_14
action_118 (39) = happyShift action_15
action_118 (41) = happyShift action_16
action_118 (44) = happyShift action_17
action_118 (23) = happyGoto action_120
action_118 (24) = happyGoto action_11
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_41

action_120 (28) = happyShift action_12
action_120 (29) = happyShift action_13
action_120 (31) = happyShift action_14
action_120 (39) = happyShift action_15
action_120 (41) = happyShift action_16
action_120 (44) = happyShift action_17
action_120 (23) = happyGoto action_49
action_120 (24) = happyGoto action_11
action_120 _ = happyReduce_43

action_121 (28) = happyShift action_12
action_121 (29) = happyShift action_13
action_121 (31) = happyShift action_14
action_121 (39) = happyShift action_15
action_121 (41) = happyShift action_16
action_121 (44) = happyShift action_17
action_121 (23) = happyGoto action_131
action_121 (24) = happyGoto action_11
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (32) = happyShift action_94
action_122 (33) = happyShift action_95
action_122 _ = happyReduce_49

action_123 (33) = happyFail []
action_123 _ = happyReduce_50

action_124 (35) = happyShift action_130
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (36) = happyShift action_129
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (26) = happyShift action_26
action_126 (27) = happyShift action_27
action_126 (39) = happyShift action_28
action_126 (41) = happyShift action_29
action_126 (44) = happyShift action_30
action_126 (13) = happyGoto action_40
action_126 (14) = happyGoto action_25
action_126 _ = happyReduce_17

action_127 (26) = happyShift action_26
action_127 (27) = happyShift action_27
action_127 (39) = happyShift action_28
action_127 (41) = happyShift action_29
action_127 (44) = happyShift action_30
action_127 (13) = happyGoto action_128
action_127 (14) = happyGoto action_25
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (26) = happyFail []
action_128 (27) = happyFail []
action_128 (39) = happyFail []
action_128 (41) = happyFail []
action_128 (44) = happyFail []
action_128 (13) = happyGoto action_40
action_128 (14) = happyGoto action_25
action_128 _ = happyReduce_16

action_129 (26) = happyShift action_26
action_129 (27) = happyShift action_27
action_129 (39) = happyShift action_28
action_129 (41) = happyShift action_29
action_129 (44) = happyShift action_30
action_129 (13) = happyGoto action_132
action_129 (14) = happyGoto action_25
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_30

action_131 (28) = happyFail []
action_131 (29) = happyFail []
action_131 (39) = happyFail []
action_131 (41) = happyFail []
action_131 (44) = happyFail []
action_131 (23) = happyGoto action_49
action_131 (24) = happyGoto action_11
action_131 _ = happyReduce_42

action_132 (26) = happyShift action_26
action_132 (27) = happyShift action_27
action_132 (39) = happyShift action_28
action_132 (41) = happyShift action_29
action_132 (44) = happyShift action_30
action_132 (13) = happyGoto action_40
action_132 (14) = happyGoto action_25
action_132 _ = happyReduce_31

happyReduce_6 = happySpecReduce_0  9 happyReduction_6
happyReduction_6  =  HappyAbsSyn9
		 ([]
	)

happyReduce_7 = happySpecReduce_3  9 happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  9 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn15  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 ((happy_var_1, happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  11 happyReduction_10
happyReduction_10  =  HappyAbsSyn11
		 ([]
	)

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn13
		 (Var happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 6 13 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Pair happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 8 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Split happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 13 happyReduction_17
happyReduction_17 ((HappyAbsSyn13  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Lambda happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_2  13 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (App happy_var_1 happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn14
		 (LIN
	)

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn14
		 (UN
	)

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  15 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (Pre happy_var_1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn16
		 (TBool
	)

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (Arrow happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (TypePair happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn16
		 (TVar happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  17 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn17
		 (QValue happy_var_1 happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 5 18 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (RPair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 6 18 happyReduction_31
happyReduction_31 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (RLambda happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_0  19 happyReduction_32
happyReduction_32  =  HappyAbsSyn19
		 ([]
	)

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_3 : happy_var_1
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 20 happyReduction_35
happyReduction_35 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((happy_var_1, happy_var_3, happy_var_4)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_0  21 happyReduction_36
happyReduction_36  =  HappyAbsSyn21
		 ([]
	)

happyReduce_37 = happySpecReduce_3  21 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_3 : happy_var_1
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  21 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn23  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn22
		 ((happy_var_1, happy_var_3)
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  23 happyReduction_40
happyReduction_40 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn23
		 (LVar happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happyReduce 6 23 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (LPair happy_var_3 happy_var_5 happy_var_1
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 8 23 happyReduction_42
happyReduction_42 ((HappyAbsSyn23  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (LSplit happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 7 23 happyReduction_43
happyReduction_43 ((HappyAbsSyn23  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (LLambda happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  23 happyReduction_44
happyReduction_44 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (LApp happy_var_1 happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  23 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  24 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn24
		 (One
	)

happyReduce_47 = happySpecReduce_1  24 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn24
		 (Omega
	)

happyReduce_48 = happySpecReduce_1  25 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn25
		 (LTBool
	)

happyReduce_49 = happyReduce 4 25 happyReduction_49
happyReduction_49 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (LArrow happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 4 25 happyReduction_50
happyReduction_50 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (LTypePair happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  25 happyReduction_51
happyReduction_51 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn25
		 (LTVar happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  25 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 45 45 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLin -> cont 26;
	TokenUn -> cont 27;
	TokenOne -> cont 28;
	TokenOmega -> cont 29;
	TokenStringBool -> cont 30;
	TokenLambda -> cont 31;
	TokenArrow -> cont 32;
	TokenPair -> cont 33;
	TokenLess -> cont 34;
	TokenBigger -> cont 35;
	TokenDot -> cont 36;
	TokenColon -> cont 37;
	TokenComma -> cont 38;
	TokenOBrack -> cont 39;
	TokenCBrack -> cont 40;
	TokenSplit -> cont 41;
	TokenAs -> cont 42;
	TokenIn -> cont 43;
	TokenVar happy_dollar_dollar -> cont 44;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 45 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse1 tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

parse2 tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

parse3 tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

parse4 tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

parse5 tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

parse6 tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenVar String
      | TokenLin
      | TokenUn
      | TokenOne
      | TokenOmega
      | TokenStringBool
      | TokenArrow
      | TokenLambda
      | TokenPair
      | TokenLess
      | TokenBigger
      | TokenDot
      | TokenColon
      | TokenComma
      | TokenOBrack
      | TokenCBrack
      | TokenSplit
      | TokenAs
      | TokenIn
      deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexVar (c:cs)
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('*':cs) = TokenPair : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenBigger : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOBrack : lexer cs
lexer (')':cs) = TokenCBrack : lexer cs

lexVar :: String -> [Token]
lexVar cs =
   case span (\x -> (isAlpha x) || (isDigit x)) cs of
      ("split", rest) -> TokenSplit : lexer rest
      ("as", rest) -> TokenAs : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("Bool", rest) -> TokenStringBool : lexer rest
      ("lin", rest) -> TokenLin : lexer rest
      ("un", rest) -> TokenUn : lexer rest
      ("1", rest) -> TokenOne : lexer rest
      ("w", rest) -> TokenOmega : lexer rest
      (var, rest) -> TokenVar var : lexer rest

parseWEnv :: String -> Env
parseWEnv env = parse1(lexer env)

parseWTerm :: String -> Term
parseWTerm term = parse2(lexer term)

parsePrimWStore :: [(String, Values)] -> Store
parsePrimWStore [] = Map.empty
parsePrimWStore l = Map.fromList l

parseWStore :: String -> Store
parseWStore store = parsePrimWStore $ parse3 (lexer store)

parseLEnv :: String -> LEnv
parseLEnv env = parse4(lexer env)

parseLTerm :: String -> LTerm
parseLTerm term = parse5(lexer term)

parsePrimLStore :: [(String, LTerm)] -> LStore
parsePrimLStore [] = Map.empty
parsePrimLStore l = Map.fromList l

parseLStore :: String -> LStore
parseLStore store = parsePrimLStore $ parse6 (lexer store)

parseWL :: String -> Term
parseWL term = parse2(lexer term)

parseLW :: String -> LTerm
parseLW term = parse5(lexer term)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
