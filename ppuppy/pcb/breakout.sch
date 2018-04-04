EESchema Schematic File Version 2
LIBS:power
LIBS:device
LIBS:switches
LIBS:relays
LIBS:motors
LIBS:transistors
LIBS:conn
LIBS:linear
LIBS:regul
LIBS:74xx
LIBS:cmos4000
LIBS:adc-dac
LIBS:memory
LIBS:xilinx
LIBS:microcontrollers
LIBS:dsp
LIBS:microchip
LIBS:analog_switches
LIBS:motorola
LIBS:texas
LIBS:intel
LIBS:audio
LIBS:interface
LIBS:digital-audio
LIBS:philips
LIBS:display
LIBS:cypress
LIBS:siliconi
LIBS:opto
LIBS:atmel
LIBS:contrib
LIBS:valves
LIBS:nescartheader
EELAYER 25 0
EELAYER END
$Descr User 19685 13780
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Raspberry_Pi_2_3 piheader1
U 1 1 5ABC2594
P 10050 3450
F 0 "piheader1" H 10750 2200 50  0000 C CNN
F 1 "Raspberry_Pi_2_3" H 9650 4350 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_2x20_Pitch2.54mm" H 11050 4700 50  0001 C CNN
F 3 "" H 10100 3300 50  0001 C CNN
	1    10050 3450
	1    0    0    -1  
$EndComp
$Comp
L nescartheader cartedge1
U 1 1 5ABC7FEB
P 3750 5500
F 0 "cartedge1" H 4700 5900 60  0000 C CNN
F 1 "nescartheader" H 3800 5900 60  0000 C CNN
F 2 "footprints:cartedge" H 3750 5500 60  0001 C CNN
F 3 "" H 3750 5500 60  0001 C CNN
	1    3750 5500
	1    0    0    -1  
$EndComp
Text GLabel 9650 4750 3    60   Input ~ 0
GND
Text GLabel 4500 5650 3    60   Input ~ 0
GND
Text GLabel 1000 4550 1    60   Input ~ 0
GND
Text GLabel 4500 4550 1    60   Input ~ 0
5V
Text GLabel 9850 2150 1    60   Input ~ 0
5V
$Comp
L R R11
U 1 1 5ABC8817
P 3350 1600
F 0 "R11" V 3430 1600 50  0000 C CNN
F 1 "4.7k" V 3350 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 3280 1600 50  0001 C CNN
F 3 "" H 3350 1600 50  0001 C CNN
	1    3350 1600
	1    0    0    -1  
$EndComp
$Comp
L R R24
U 1 1 5ABC88DC
P 3900 1600
F 0 "R24" V 3980 1600 50  0000 C CNN
F 1 "4.7k" V 3900 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 3830 1600 50  0001 C CNN
F 3 "" H 3900 1600 50  0001 C CNN
	1    3900 1600
	1    0    0    -1  
$EndComp
$Comp
L R R25
U 1 1 5ABC8915
P 4450 1600
F 0 "R25" V 4530 1600 50  0000 C CNN
F 1 "4.7k" V 4450 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 4380 1600 50  0001 C CNN
F 3 "" H 4450 1600 50  0001 C CNN
	1    4450 1600
	1    0    0    -1  
$EndComp
$Comp
L R R26
U 1 1 5ABC8934
P 5000 1600
F 0 "R26" V 5080 1600 50  0000 C CNN
F 1 "4.7k" V 5000 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 4930 1600 50  0001 C CNN
F 3 "" H 5000 1600 50  0001 C CNN
	1    5000 1600
	1    0    0    -1  
$EndComp
$Comp
L R R27
U 1 1 5ABC8979
P 5550 1600
F 0 "R27" V 5630 1600 50  0000 C CNN
F 1 "4.7k" V 5550 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 5480 1600 50  0001 C CNN
F 3 "" H 5550 1600 50  0001 C CNN
	1    5550 1600
	1    0    0    -1  
$EndComp
$Comp
L R R28
U 1 1 5ABC89B4
P 6100 1600
F 0 "R28" V 6180 1600 50  0000 C CNN
F 1 "4.7k" V 6100 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 6030 1600 50  0001 C CNN
F 3 "" H 6100 1600 50  0001 C CNN
	1    6100 1600
	1    0    0    -1  
$EndComp
Text GLabel 3250 1400 0    60   Input ~ 0
5V
$Comp
L R R29
U 1 1 5ABC8A4B
P 6650 1600
F 0 "R29" V 6730 1600 50  0000 C CNN
F 1 "4.7k" V 6650 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 6580 1600 50  0001 C CNN
F 3 "" H 6650 1600 50  0001 C CNN
	1    6650 1600
	1    0    0    -1  
$EndComp
$Comp
L R R30
U 1 1 5ABC8A78
P 7200 1600
F 0 "R30" V 7280 1600 50  0000 C CNN
F 1 "4.7k" V 7200 1600 50  0000 C CNN
F 2 "Resistors_THT:R_Axial_DIN0309_L9.0mm_D3.2mm_P25.40mm_Horizontal" V 7130 1600 50  0001 C CNN
F 3 "" H 7200 1600 50  0001 C CNN
	1    7200 1600
	1    0    0    -1  
$EndComp
$Comp
L 2N3904 Q1
U 1 1 5ABC8BF6
P 3550 2000
F 0 "Q1" H 3750 2075 50  0000 L CNN
F 1 "2N3904" H 3750 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 3750 1925 50  0001 L CIN
F 3 "" H 3550 2000 50  0001 L CNN
	1    3550 2000
	0    -1   -1   0   
$EndComp
$Comp
L 2N3904 Q2
U 1 1 5ABC9A2C
P 4100 2000
F 0 "Q2" H 4300 2075 50  0000 L CNN
F 1 "2N3904" H 4300 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 4300 1925 50  0001 L CIN
F 3 "" H 4100 2000 50  0001 L CNN
	1    4100 2000
	0    -1   -1   0   
$EndComp
Text GLabel 7800 2200 2    60   Input ~ 0
GND
$Comp
L 2N3904 Q3
U 1 1 5ABC9C7E
P 4650 2000
F 0 "Q3" H 4850 2075 50  0000 L CNN
F 1 "2N3904" H 4850 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 4850 1925 50  0001 L CIN
F 3 "" H 4650 2000 50  0001 L CNN
	1    4650 2000
	0    -1   -1   0   
$EndComp
$Comp
L 2N3904 Q4
U 1 1 5ABC9D31
P 5200 2000
F 0 "Q4" H 5400 2075 50  0000 L CNN
F 1 "2N3904" H 5400 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 5400 1925 50  0001 L CIN
F 3 "" H 5200 2000 50  0001 L CNN
	1    5200 2000
	0    -1   -1   0   
$EndComp
$Comp
L 2N3904 Q5
U 1 1 5ABC9E1B
P 5750 2000
F 0 "Q5" H 5950 2075 50  0000 L CNN
F 1 "2N3904" H 5950 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 5950 1925 50  0001 L CIN
F 3 "" H 5750 2000 50  0001 L CNN
	1    5750 2000
	0    -1   -1   0   
$EndComp
$Comp
L 2N3904 Q6
U 1 1 5ABC9F4A
P 6300 2000
F 0 "Q6" H 6500 2075 50  0000 L CNN
F 1 "2N3904" H 6500 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 6500 1925 50  0001 L CIN
F 3 "" H 6300 2000 50  0001 L CNN
	1    6300 2000
	0    -1   -1   0   
$EndComp
$Comp
L 2N3904 Q7
U 1 1 5ABC9FE1
P 6850 2000
F 0 "Q7" H 7050 2075 50  0000 L CNN
F 1 "2N3904" H 7050 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 7050 1925 50  0001 L CIN
F 3 "" H 6850 2000 50  0001 L CNN
	1    6850 2000
	0    -1   -1   0   
$EndComp
$Comp
L 2N3904 Q8
U 1 1 5ABCA089
P 7400 2000
F 0 "Q8" H 7600 2075 50  0000 L CNN
F 1 "2N3904" H 7600 2000 50  0000 L CNN
F 2 "TO_SOT_Packages_THT:TO-92_Molded_Wide" H 7600 1925 50  0001 L CIN
F 3 "" H 7400 2000 50  0001 L CNN
	1    7400 2000
	0    -1   -1   0   
$EndComp
Text GLabel 3100 2950 1    60   Input ~ 0
GND
$Comp
L GND #PWR01
U 1 1 5AC05FD4
P 10700 1300
F 0 "#PWR01" H 10700 1050 50  0001 C CNN
F 1 "GND" H 10700 1150 50  0000 C CNN
F 2 "" H 10700 1300 50  0001 C CNN
F 3 "" H 10700 1300 50  0001 C CNN
	1    10700 1300
	1    0    0    -1  
$EndComp
Text GLabel 10700 1050 1    60   Input ~ 0
GND
$Comp
L +5V #PWR02
U 1 1 5AC070DC
P 10450 1350
F 0 "#PWR02" H 10450 1200 50  0001 C CNN
F 1 "+5V" H 10450 1490 50  0000 C CNN
F 2 "" H 10450 1350 50  0001 C CNN
F 3 "" H 10450 1350 50  0001 C CNN
	1    10450 1350
	-1   0    0    1   
$EndComp
Text GLabel 10450 1050 1    60   Input ~ 0
5V
Wire Wire Line
	3250 1400 7200 1400
Wire Wire Line
	3350 1400 3350 1450
Connection ~ 3350 1400
Wire Wire Line
	3900 1400 3900 1450
Connection ~ 3900 1400
Wire Wire Line
	4450 1400 4450 1450
Connection ~ 4450 1400
Wire Wire Line
	5000 1400 5000 1450
Connection ~ 5000 1400
Wire Wire Line
	5550 1400 5550 1450
Connection ~ 5550 1400
Wire Wire Line
	6100 1400 6100 1450
Connection ~ 6100 1400
Wire Wire Line
	6650 1400 6650 1450
Connection ~ 6650 1400
Wire Wire Line
	7200 1400 7200 1450
Connection ~ 7200 1400
Wire Wire Line
	3350 1750 3350 2950
Wire Wire Line
	3350 2950 3900 2950
Wire Wire Line
	3900 2950 3900 4550
Connection ~ 3350 1900
Wire Wire Line
	3900 1750 3900 2900
Wire Wire Line
	3900 2900 4000 2900
Wire Wire Line
	4000 2900 4000 4550
Connection ~ 3900 1900
Wire Wire Line
	3550 2200 3550 2700
Wire Wire Line
	4100 2200 4100 2650
Wire Wire Line
	4450 1750 4450 2900
Wire Wire Line
	5000 1750 5000 2950
Wire Wire Line
	5550 1750 5550 6000
Wire Wire Line
	6100 1750 6100 6050
Wire Wire Line
	6650 1750 6650 6100
Wire Wire Line
	7200 1750 7200 6150
Wire Wire Line
	4650 2200 4650 2600
Wire Wire Line
	5200 2200 5200 2550
Wire Wire Line
	5750 2200 5750 2500
Wire Wire Line
	6300 2200 6300 2450
Wire Wire Line
	6850 2200 6850 2400
Wire Wire Line
	7400 2200 7400 2350
Wire Wire Line
	4450 2900 4100 2900
Wire Wire Line
	4100 2900 4100 4550
Connection ~ 4450 1900
Wire Wire Line
	5000 2950 4200 2950
Wire Wire Line
	4200 2950 4200 4550
Connection ~ 5000 1900
Wire Wire Line
	5550 6000 4200 6000
Wire Wire Line
	4200 6000 4200 5650
Connection ~ 5550 1900
Wire Wire Line
	6100 6050 4100 6050
Wire Wire Line
	4100 6050 4100 5650
Connection ~ 6100 1900
Wire Wire Line
	6650 6100 4000 6100
Wire Wire Line
	4000 6100 4000 5650
Connection ~ 6650 1900
Wire Wire Line
	7200 6150 3900 6150
Wire Wire Line
	3900 6150 3900 5650
Connection ~ 7200 1900
Wire Wire Line
	9150 2750 9050 2750
Wire Wire Line
	9050 2750 8500 2400
Wire Wire Line
	9150 2850 9050 2850
Wire Wire Line
	9050 2850 8500 2500
Wire Wire Line
	9150 2950 9050 2950
Wire Wire Line
	9050 2950 8500 2600
Wire Wire Line
	9150 3050 9050 3050
Wire Wire Line
	9050 3050 8500 2700
Wire Wire Line
	9150 3150 9050 3150
Wire Wire Line
	9050 3150 8500 2800
Wire Wire Line
	9150 3250 9050 3250
Wire Wire Line
	9050 3250 8500 2900
Wire Wire Line
	9150 3350 9050 3350
Wire Wire Line
	9050 3350 8500 3000
Wire Wire Line
	8500 3100 9050 3450
Wire Wire Line
	3550 2700 7400 2700
Wire Wire Line
	7400 2700 8100 3100
Wire Wire Line
	4100 2650 7450 2650
Wire Wire Line
	4650 2600 7500 2600
Wire Wire Line
	5200 2550 7550 2550
Wire Wire Line
	5750 2500 7600 2500
Wire Wire Line
	6300 2450 7650 2450
Wire Wire Line
	6850 2400 7700 2400
Wire Wire Line
	7400 2350 7750 2350
Wire Wire Line
	7450 2650 8100 3000
Wire Wire Line
	7500 2600 8100 2900
Wire Wire Line
	7550 2550 8100 2800
Wire Wire Line
	7600 2500 8100 2700
Wire Wire Line
	7650 2450 8100 2600
Wire Wire Line
	7700 2400 8100 2500
Wire Wire Line
	7750 2350 8100 2400
Wire Wire Line
	3750 2200 7800 2200
Connection ~ 3750 2200
Connection ~ 4300 2200
Connection ~ 4850 2200
Connection ~ 5400 2200
Connection ~ 5950 2200
Connection ~ 6500 2200
Connection ~ 7050 2200
Connection ~ 7600 2200
Wire Wire Line
	7600 2200 7600 1900
Wire Wire Line
	7050 2200 7050 1900
Wire Wire Line
	6500 2200 6500 1900
Wire Wire Line
	5950 2200 5950 1900
Wire Wire Line
	5400 2200 5400 1900
Wire Wire Line
	4850 2200 4850 1900
Wire Wire Line
	4300 2200 4300 1900
Wire Wire Line
	3750 2200 3750 1900
Wire Wire Line
	7450 3450 8100 3550
Connection ~ 3100 3450
Wire Wire Line
	3000 4550 3000 4450
Wire Wire Line
	10700 1050 10700 1300
Wire Wire Line
	10450 1050 10450 1350
Wire Wire Line
	3200 6550 3200 7050
Wire Wire Line
	3300 6550 3300 7050
Connection ~ 3300 6850
Connection ~ 3200 6750
Wire Wire Line
	3100 3350 3100 3900
Wire Wire Line
	3200 5650 3200 6150
Wire Wire Line
	3400 6550 3400 7050
Wire Wire Line
	3800 3350 3800 3900
Connection ~ 3800 3800
Connection ~ 3400 6950
Wire Wire Line
	3400 5650 3400 6150
Wire Wire Line
	3800 4300 3800 4550
Wire Wire Line
	8100 3550 9150 3550
Wire Wire Line
	8100 3650 9150 3650
Connection ~ 3200 3500
Wire Wire Line
	3200 3350 3200 3900
Connection ~ 3300 3550
Wire Wire Line
	3300 3350 3300 3900
Connection ~ 3400 3600
Wire Wire Line
	3400 3350 3400 3900
Connection ~ 3500 3650
Wire Wire Line
	3500 3350 3500 3900
Connection ~ 3600 3700
Wire Wire Line
	3600 3350 3600 3900
Connection ~ 3700 3750
Wire Wire Line
	3700 3350 3700 3900
Wire Wire Line
	3200 4300 3200 4550
Wire Wire Line
	3300 4300 3300 4550
Wire Wire Line
	3400 4300 3400 4550
Wire Wire Line
	3500 4300 3500 4550
Wire Wire Line
	3600 4300 3600 4550
Wire Wire Line
	3700 4300 3700 4550
Wire Wire Line
	3100 3450 7450 3450
Wire Wire Line
	3200 3500 7450 3500
Wire Wire Line
	3300 3550 7450 3550
Wire Wire Line
	3400 3600 7450 3600
Wire Wire Line
	7450 3500 8100 3650
Wire Wire Line
	3500 3650 7450 3650
Wire Wire Line
	3600 3700 7450 3700
Wire Wire Line
	3700 3750 7450 3750
Wire Wire Line
	3800 3800 7450 3800
Wire Wire Line
	9150 3750 8100 3750
Wire Wire Line
	8100 3750 7450 3550
Wire Wire Line
	9150 3850 8100 3850
Wire Wire Line
	8100 3850 7450 3600
Wire Wire Line
	10950 2550 11000 2550
Wire Wire Line
	11000 2550 11000 1850
Wire Wire Line
	11000 1850 8900 1850
Wire Wire Line
	8900 1850 8900 3900
Wire Wire Line
	8900 3900 8100 3900
Wire Wire Line
	8100 3900 7450 3650
Wire Wire Line
	10950 2650 11050 2650
Wire Wire Line
	11050 2650 11050 1800
Wire Wire Line
	11050 1800 8850 1800
Wire Wire Line
	8850 1800 8850 3950
Wire Wire Line
	8850 3950 8100 3950
Wire Wire Line
	8100 3950 7450 3700
Wire Wire Line
	10950 2750 11100 2750
Wire Wire Line
	11100 2750 11100 1750
Wire Wire Line
	11100 1750 8800 1750
Wire Wire Line
	8800 1750 8800 4000
Wire Wire Line
	8800 4000 8100 4000
Wire Wire Line
	8100 4000 7450 3750
Wire Wire Line
	10950 2950 11150 2950
Wire Wire Line
	11150 2950 11150 1700
Wire Wire Line
	11150 1700 8750 1700
Wire Wire Line
	8750 1700 8750 4050
Wire Wire Line
	8750 4050 8100 4050
Wire Wire Line
	8100 4050 7450 3800
Text GLabel 3400 7450 0    60   Input ~ 0
GND
Wire Wire Line
	10950 3050 11500 3050
Wire Wire Line
	11500 3050 11500 6750
Wire Wire Line
	11500 6750 3200 6750
Wire Wire Line
	10950 3250 11450 3250
Wire Wire Line
	3300 6850 11450 6850
Wire Wire Line
	11450 6850 11450 3250
Wire Wire Line
	3400 6950 11400 6950
Wire Wire Line
	11400 6950 11400 3350
Wire Wire Line
	11400 3350 10950 3350
$Comp
L R_Pack08 RN1
U 1 1 5AC22563
P 8300 2800
F 0 "RN1" V 7800 2800 50  0000 C CNN
F 1 "10k" V 8700 2800 50  0000 C CNN
F 2 "Housings_DIP:DIP-16_W7.62mm" V 8775 2800 50  0001 C CNN
F 3 "" H 8300 2800 50  0001 C CNN
	1    8300 2800
	0    1    1    0   
$EndComp
Wire Wire Line
	9050 3450 9150 3450
$Comp
L R_Network03 RN3
U 1 1 5AC233CA
P 3300 7250
F 0 "RN3" V 3100 7250 50  0000 C CNN
F 1 "2k" V 3500 7250 50  0000 C CNN
F 2 "Resistors_THT:R_Array_SIP4" V 3575 7250 50  0001 C CNN
F 3 "" H 3300 7250 50  0001 C CNN
	1    3300 7250
	-1   0    0    1   
$EndComp
$Comp
L R_Pack03 RN2
U 1 1 5AC238AA
P 3300 6350
F 0 "RN2" V 3100 6350 50  0000 C CNN
F 1 "1k" V 3500 6350 50  0000 C CNN
F 2 "Housings_DIP:DIP-6_W10.16mm" V 3575 6350 50  0001 C CNN
F 3 "" H 3300 6350 50  0001 C CNN
	1    3300 6350
	1    0    0    -1  
$EndComp
Wire Wire Line
	3300 5650 3300 6150
$Comp
L R_Network08 RN4
U 1 1 5AC241C2
P 3500 3150
F 0 "RN4" V 3000 3150 50  0000 C CNN
F 1 "2k" V 3900 3150 50  0000 C CNN
F 2 "Resistors_THT:R_Array_SIP9" V 3975 3150 50  0001 C CNN
F 3 "" H 3500 3150 50  0001 C CNN
	1    3500 3150
	1    0    0    -1  
$EndComp
$Comp
L R_Pack08 RN5
U 1 1 5AC2483F
P 3500 4100
F 0 "RN5" V 3000 4100 50  0000 C CNN
F 1 "1k" V 3900 4100 50  0000 C CNN
F 2 "Housings_DIP:DIP-16_W10.16mm" V 3975 4100 50  0001 C CNN
F 3 "" H 3500 4100 50  0001 C CNN
	1    3500 4100
	1    0    0    -1  
$EndComp
Wire Wire Line
	3000 4450 3100 4300
$EndSCHEMATC
