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
LIBS:piheader
LIBS:gls29ee010
LIBS:nescic
LIBS:cd74fct245
LIBS:mc74vhct50a
LIBS:realbreakout-cache
EELAYER 25 0
EELAYER END
$Descr A4 11693 8268
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
L nescartheader U1
U 1 1 5AC25423
P 5950 2400
F 0 "U1" H 6900 2800 60  0000 C CNN
F 1 "nescartheader" H 6000 2800 60  0000 C CNN
F 2 "footprints:cartedge" H 5950 2400 60  0001 C CNN
F 3 "" H 5950 2400 60  0001 C CNN
	1    5950 2400
	1    0    0    -1  
$EndComp
$Comp
L Conn_01x36 J3
U 1 1 5AC254CB
P 5000 4200
F 0 "J3" H 5000 6000 50  0000 C CNN
F 1 "Conn_01x36" H 5000 2300 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x36_Pitch2.54mm" H 5000 4200 50  0001 C CNN
F 3 "" H 5000 4200 50  0001 C CNN
	1    5000 4200
	0    1    1    0   
$EndComp
$Comp
L Conn_01x36 J1
U 1 1 5AC258A5
P 4900 150
F 0 "J1" H 4900 1950 50  0000 C CNN
F 1 "Conn_01x36" H 4900 -1750 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x36_Pitch2.54mm" H 4900 150 50  0001 C CNN
F 3 "" H 4900 150 50  0001 C CNN
	1    4900 150 
	0    -1   -1   0   
$EndComp
$Comp
L Conn_01x36 J2
U 1 1 5AC25968
P 5000 3850
F 0 "J2" H 5000 5650 50  0000 C CNN
F 1 "Conn_01x36" H 5000 1950 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x36_Pitch2.54mm" H 5000 3850 50  0001 C CNN
F 3 "" H 5000 3850 50  0001 C CNN
	1    5000 3850
	0    1    1    0   
$EndComp
$Comp
L piheader U2
U 1 1 5AC26215
P 3800 5450
F 0 "U2" H 3800 5350 60  0000 C CNN
F 1 "piheader" H 3800 5450 60  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_2x20_Pitch2.54mm" H 3800 5350 60  0001 C CNN
F 3 "" H 3800 5350 60  0001 C CNN
	1    3800 5450
	0    1    1    0   
$EndComp
Text GLabel 5700 6050 3    60   Input ~ 0
GND
Text GLabel 6700 1350 2    60   Input ~ 0
5v
Text GLabel 5900 6050 3    60   Input ~ 0
5v
Text GLabel 3200 1350 0    60   Input ~ 0
GND
Text GLabel 6700 3650 2    60   Input ~ 0
GND
Text GLabel 5800 6050 3    60   Input ~ 0
5v
Text GLabel 4000 4700 1    60   Input ~ 0
GND
$Comp
L GLS29EE010 E1
U 1 1 5AC43D48
P 500 2150
F 0 "E1" H 500 2150 60  0000 C CNN
F 1 "GLS29EE010" H 500 2250 60  0000 C CNN
F 2 "Housings_DIP:DIP-32_W15.24mm" H 500 2150 60  0001 C CNN
F 3 "" H 500 2150 60  0001 C CNN
	1    500  2150
	0    1    1    0   
$EndComp
Text GLabel 750  1550 1    60   Input ~ 0
GND
Text GLabel 2250 2500 2    60   Input ~ 0
5V
$Comp
L nescic U3
U 1 1 5AC6E39D
P 10100 6050
F 0 "U3" H 10100 6050 60  0000 C CNN
F 1 "nescic" H 10100 6150 60  0000 C CNN
F 2 "Housings_DIP:DIP-16_W7.62mm" H 10100 6050 60  0001 C CNN
F 3 "" H 10100 6050 60  0001 C CNN
	1    10100 6050
	1    0    0    -1  
$EndComp
Text GLabel 9350 5200 0    60   Input ~ 0
GND
Text GLabel 10750 5500 2    60   Input ~ 0
GND
Text GLabel 10750 5350 2    60   Input ~ 0
GND
Text GLabel 10750 5050 2    60   Input ~ 0
GND
Text GLabel 10750 4750 2    60   Input ~ 0
5V
Text GLabel 10750 5200 2    60   Input ~ 0
GND
Text GLabel 10750 4900 2    60   Input ~ 0
GND
Text GLabel 9350 5800 0    60   Input ~ 0
GND
Text GLabel 9350 5050 0    60   Input ~ 0
GND
$Comp
L CD74FCT245 U4
U 1 1 5ADA99F5
P 6950 4100
F 0 "U4" H 6950 5700 60  0000 C CNN
F 1 "CD74FCT245" H 6950 4400 60  0000 C CNN
F 2 "Housings_DIP:DIP-20_W7.62mm" H 6950 4100 60  0001 C CNN
F 3 "" H 6950 4100 60  0001 C CNN
	1    6950 4100
	0    1    1    0   
$EndComp
Text GLabel 7450 3600 1    60   Input ~ 0
GND
Text GLabel 8350 4650 3    60   Input ~ 0
5v
Text GLabel 2150 1550 1    60   Input ~ 0
GND
Text GLabel 2050 1550 1    60   Input ~ 0
GND
Text GLabel 2150 2500 3    60   Input ~ 0
GND
Text GLabel 1250 2500 3    60   Input ~ 0
GND
Text GLabel 5900 4700 1    60   Input ~ 0
3v
Text GLabel 8350 3700 1    60   Input ~ 0
GND
$Comp
L MC74VHCT50A U6
U 1 1 5ADB7F2F
P 8950 1250
F 0 "U6" H 8950 2450 60  0000 C CNN
F 1 "MC74VHCT50A" H 8950 1350 60  0000 C CNN
F 2 "Housings_SOIC:SOIC-14_3.9x8.7mm_Pitch1.27mm" H 8950 1250 60  0001 C CNN
F 3 "" H 8950 1250 60  0001 C CNN
	1    8950 1250
	0    1    1    0   
$EndComp
$Comp
L MC74VHCT50A U5
U 1 1 5ADB7FE0
P 8900 2800
F 0 "U5" H 8900 4000 60  0000 C CNN
F 1 "MC74VHCT50A" H 8900 2900 60  0000 C CNN
F 2 "Housings_SOIC:SOIC-14_3.9x8.7mm_Pitch1.27mm" H 8900 2800 60  0001 C CNN
F 3 "" H 8900 2800 60  0001 C CNN
	1    8900 2800
	0    1    1    0   
$EndComp
Text GLabel 9900 1800 3    60   Input ~ 0
3v
Text GLabel 9850 3350 3    60   Input ~ 0
3v
Text GLabel 9250 2250 1    60   Input ~ 0
GND
Text GLabel 9150 800  0    60   Input ~ 0
GND
Wire Wire Line
	3200 2550 3200 4000
Wire Wire Line
	3300 2550 3300 4000
Wire Wire Line
	3400 2550 3400 4000
Wire Wire Line
	3500 2550 3500 4000
Wire Wire Line
	3600 2550 3600 4000
Wire Wire Line
	3700 2550 3700 4000
Wire Wire Line
	3800 2550 3800 4000
Wire Wire Line
	3900 2550 3900 4000
Wire Wire Line
	4000 2550 4000 4000
Wire Wire Line
	4100 2550 4100 4000
Wire Wire Line
	4200 2550 4200 4000
Wire Wire Line
	4300 2550 4300 4000
Wire Wire Line
	4400 2550 4400 4000
Wire Wire Line
	4500 2550 4500 4000
Wire Wire Line
	4600 2550 4600 4000
Wire Wire Line
	4700 2550 4700 4000
Wire Wire Line
	4800 2550 4800 4000
Wire Wire Line
	4900 2550 4900 4000
Wire Wire Line
	5000 2550 5000 4000
Wire Wire Line
	5100 2550 5100 4000
Wire Wire Line
	5200 2550 5200 4000
Wire Wire Line
	5300 2550 5300 4000
Wire Wire Line
	5400 2550 5400 4000
Wire Wire Line
	5500 2550 5500 4000
Wire Wire Line
	5600 2550 5600 4000
Wire Wire Line
	5700 2550 5700 4000
Wire Wire Line
	5800 2550 5800 4000
Wire Wire Line
	5900 2550 5900 4000
Wire Wire Line
	6000 2550 6000 4000
Wire Wire Line
	6100 2550 6100 4000
Wire Wire Line
	6200 2550 6200 4000
Wire Wire Line
	6300 2550 6300 4000
Wire Wire Line
	6400 2550 6400 4000
Wire Wire Line
	6500 2550 6500 4000
Wire Wire Line
	6600 2550 6600 4000
Wire Wire Line
	6700 2550 6700 4000
Connection ~ 3200 3650
Connection ~ 3300 3650
Connection ~ 3400 3650
Connection ~ 3500 3650
Connection ~ 3600 3650
Connection ~ 3700 3650
Connection ~ 3800 3650
Connection ~ 3900 3650
Connection ~ 4000 3650
Connection ~ 4100 3650
Connection ~ 4200 3650
Connection ~ 4300 3650
Connection ~ 4400 3650
Connection ~ 4500 3650
Connection ~ 4600 3650
Connection ~ 4700 3650
Connection ~ 4800 3650
Connection ~ 4900 3650
Connection ~ 5000 3650
Connection ~ 5100 3650
Connection ~ 5200 3650
Connection ~ 5300 3650
Connection ~ 5400 3650
Connection ~ 5500 3650
Connection ~ 5600 3650
Connection ~ 5700 3650
Connection ~ 5800 3650
Connection ~ 5900 3650
Connection ~ 6000 3650
Connection ~ 6100 3650
Connection ~ 6200 3650
Connection ~ 6300 3650
Connection ~ 6400 3650
Connection ~ 6500 3650
Connection ~ 6600 3650
Connection ~ 6700 3650
Wire Wire Line
	3200 350  3200 1450
Wire Wire Line
	3300 350  3300 1450
Wire Wire Line
	3400 350  3400 1450
Wire Wire Line
	3500 350  3500 1450
Wire Wire Line
	3600 350  3600 1450
Wire Wire Line
	3700 350  3700 1450
Wire Wire Line
	3800 350  3800 1450
Wire Wire Line
	3900 350  3900 1450
Wire Wire Line
	4000 350  4000 1450
Wire Wire Line
	4100 350  4100 1450
Wire Wire Line
	4200 350  4200 1450
Wire Wire Line
	4300 350  4300 1450
Wire Wire Line
	4400 350  4400 1450
Wire Wire Line
	4500 350  4500 1450
Wire Wire Line
	4600 350  4600 1450
Wire Wire Line
	4700 350  4700 1450
Wire Wire Line
	4800 350  4800 1450
Wire Wire Line
	4900 350  4900 1450
Wire Wire Line
	5000 350  5000 1450
Wire Wire Line
	5100 350  5100 1450
Wire Wire Line
	5200 350  5200 1450
Wire Wire Line
	5300 350  5300 1450
Wire Wire Line
	5400 350  5400 1450
Wire Wire Line
	5500 350  5500 1450
Wire Wire Line
	5600 350  5600 1450
Wire Wire Line
	5700 350  5700 1450
Wire Wire Line
	5800 350  5800 1450
Wire Wire Line
	5900 350  5900 1450
Wire Wire Line
	6000 350  6000 1450
Wire Wire Line
	6100 350  6100 1450
Wire Wire Line
	6200 350  6200 1450
Wire Wire Line
	6300 350  6300 1450
Wire Wire Line
	6400 350  6400 1450
Wire Wire Line
	6500 350  6500 1450
Wire Wire Line
	6600 350  6600 1450
Wire Wire Line
	6700 350  6700 1450
Connection ~ 3200 1350
Connection ~ 3300 1350
Connection ~ 3400 1350
Connection ~ 3500 1350
Connection ~ 3600 1350
Connection ~ 3700 1350
Connection ~ 3800 1350
Connection ~ 3900 1350
Connection ~ 4000 1350
Connection ~ 4100 1350
Connection ~ 4200 1350
Connection ~ 4300 1350
Connection ~ 4400 1350
Connection ~ 4500 1350
Connection ~ 4600 1350
Connection ~ 4700 1350
Connection ~ 4800 1350
Connection ~ 4900 1350
Connection ~ 5000 1350
Connection ~ 5100 1350
Connection ~ 5200 1350
Connection ~ 5300 1350
Connection ~ 5400 1350
Connection ~ 5500 1350
Connection ~ 5600 1350
Connection ~ 5700 1350
Connection ~ 5800 1350
Connection ~ 5900 1350
Connection ~ 6000 1350
Connection ~ 6100 1350
Connection ~ 6200 1350
Connection ~ 6300 1350
Connection ~ 6400 1350
Connection ~ 6500 1350
Connection ~ 6600 1350
Connection ~ 6700 1350
Connection ~ 3400 2750
Connection ~ 3500 2800
Connection ~ 3600 2850
Connection ~ 3700 2900
Connection ~ 3800 2950
Connection ~ 3900 3000
Connection ~ 4000 3050
Connection ~ 4100 3100
Connection ~ 4200 3150
Connection ~ 4300 3200
Connection ~ 4400 3250
Connection ~ 3300 1250
Connection ~ 3400 1200
Connection ~ 3500 1150
Connection ~ 3600 1100
Connection ~ 3700 1050
Connection ~ 3800 1000
Connection ~ 3900 950 
Connection ~ 4000 900 
Connection ~ 4100 850 
Connection ~ 4200 800 
Connection ~ 4300 750 
Connection ~ 4400 700 
Wire Wire Line
	3400 2750 2800 2750
Wire Wire Line
	2800 2750 2800 1450
Wire Wire Line
	2800 1450 1950 1450
Wire Wire Line
	1950 1450 1950 1550
Wire Wire Line
	1850 2800 3500 2800
Wire Wire Line
	1850 2500 1850 2800
Wire Wire Line
	1950 2850 3600 2850
Wire Wire Line
	1950 2500 1950 2850
Wire Wire Line
	1150 2900 3700 2900
Wire Wire Line
	1150 2500 1150 2900
Wire Wire Line
	1050 2950 3800 2950
Wire Wire Line
	1050 2500 1050 2950
Wire Wire Line
	950  3000 3900 3000
Wire Wire Line
	950  2500 950  3000
Wire Wire Line
	850  3050 4000 3050
Wire Wire Line
	850  2500 850  3050
Wire Wire Line
	750  3100 4100 3100
Wire Wire Line
	750  2500 750  3100
Wire Wire Line
	2450 3150 4200 3150
Wire Wire Line
	2450 1400 2450 3150
Wire Wire Line
	2450 1400 850  1400
Wire Wire Line
	850  1400 850  1550
Wire Wire Line
	4300 3200 2500 3200
Wire Wire Line
	2500 1350 950  1350
Wire Wire Line
	950  1350 950  1550
Wire Wire Line
	4400 3250 2550 3250
Wire Wire Line
	2550 3250 2550 1300
Wire Wire Line
	2550 1300 1050 1300
Wire Wire Line
	1050 1300 1050 1550
Wire Wire Line
	3300 1250 2750 1250
Wire Wire Line
	2750 1250 2750 2750
Wire Wire Line
	2750 2750 1550 2750
Wire Wire Line
	1550 2750 1550 2500
Wire Wire Line
	3400 1200 2700 1200
Wire Wire Line
	2700 1200 2700 2700
Wire Wire Line
	2700 2700 1350 2700
Wire Wire Line
	2500 3200 2500 1350
Wire Wire Line
	1350 2700 1350 2500
Wire Wire Line
	3500 1150 2650 1150
Wire Wire Line
	2650 1150 2650 2650
Wire Wire Line
	2650 2650 1650 2650
Wire Wire Line
	1650 2650 1650 2500
Wire Wire Line
	3600 1100 2600 1100
Wire Wire Line
	2600 1100 2600 2600
Wire Wire Line
	2600 2600 1750 2600
Wire Wire Line
	1750 2600 1750 2500
Wire Wire Line
	3700 1050 1850 1050
Wire Wire Line
	1850 1050 1850 1550
Wire Wire Line
	3800 1000 1750 1000
Wire Wire Line
	1750 1000 1750 1550
Wire Wire Line
	3900 950  1650 950 
Wire Wire Line
	1650 950  1650 1550
Wire Wire Line
	4000 900  1550 900 
Wire Wire Line
	1550 900  1550 1550
Wire Wire Line
	4100 850  1450 850 
Wire Wire Line
	1450 850  1450 1550
Wire Wire Line
	4200 800  1350 800 
Wire Wire Line
	1350 800  1350 1550
Wire Wire Line
	4300 750  1250 750 
Wire Wire Line
	1250 750  1250 1550
Wire Wire Line
	4400 700  1150 700 
Wire Wire Line
	1150 700  1150 1550
Connection ~ 6600 1200
Wire Wire Line
	6600 1200 11100 1200
Connection ~ 6500 1250
Wire Wire Line
	6500 1250 11050 1250
Wire Wire Line
	11050 1250 11050 4100
Connection ~ 6600 2600
Connection ~ 6500 2650
Wire Wire Line
	11100 1200 11100 4150
Wire Wire Line
	6600 2600 8800 2600
Wire Wire Line
	6500 2650 8750 2650
Wire Wire Line
	8750 5650 9350 5650
Connection ~ 6100 1150
Connection ~ 6200 1100
Connection ~ 6300 1050
Connection ~ 6400 1000
Wire Wire Line
	6100 1150 6900 1150
Wire Wire Line
	6900 1150 6900 3100
Wire Wire Line
	6900 3100 7550 3100
Wire Wire Line
	7550 3100 7550 3700
Wire Wire Line
	6200 1100 6950 1100
Wire Wire Line
	6950 1100 6950 3050
Wire Wire Line
	6950 3050 7650 3050
Wire Wire Line
	7650 3050 7650 3700
Wire Wire Line
	6300 1050 7000 1050
Wire Wire Line
	7000 1050 7000 3000
Wire Wire Line
	7000 3000 7750 3000
Wire Wire Line
	7750 3000 7750 3700
Wire Wire Line
	6400 1000 7050 1000
Wire Wire Line
	7050 1000 7050 2950
Wire Wire Line
	7050 2950 7850 2950
Wire Wire Line
	7850 2950 7850 3700
Connection ~ 6400 2850
Connection ~ 6300 2800
Connection ~ 6200 2750
Connection ~ 6100 2700
Wire Wire Line
	6400 2850 7950 2850
Wire Wire Line
	7950 2850 7950 3700
Wire Wire Line
	8050 3700 8050 2800
Wire Wire Line
	8050 2800 6300 2800
Wire Wire Line
	8150 3700 8150 2750
Wire Wire Line
	8150 2750 6200 2750
Wire Wire Line
	6100 2700 8250 2700
Wire Wire Line
	8250 2700 8250 3700
Connection ~ 4500 3300
Wire Wire Line
	4500 3300 1450 3300
Wire Wire Line
	1450 3300 1450 2500
Wire Wire Line
	5800 4700 5800 4600
Wire Wire Line
	5800 4600 6650 4600
Wire Wire Line
	6650 4600 6650 4950
Wire Wire Line
	6650 4950 8150 4950
Wire Wire Line
	8150 4950 8150 4550
Wire Wire Line
	8050 4550 8050 4900
Wire Wire Line
	8050 4900 6700 4900
Wire Wire Line
	6700 4900 6700 4550
Wire Wire Line
	6700 4550 5700 4550
Wire Wire Line
	5700 4550 5700 4700
Wire Wire Line
	7950 4550 7950 4850
Wire Wire Line
	7950 4850 6750 4850
Wire Wire Line
	6750 4850 6750 4500
Wire Wire Line
	6750 4500 5600 4500
Wire Wire Line
	5600 4500 5600 4700
Wire Wire Line
	7850 4550 7850 4800
Wire Wire Line
	7850 4800 6800 4800
Wire Wire Line
	6800 4800 6800 4450
Wire Wire Line
	6800 4450 4500 4450
Wire Wire Line
	4500 4450 4500 4700
Wire Wire Line
	7750 4550 7750 4750
Wire Wire Line
	7750 4750 6850 4750
Wire Wire Line
	6850 4750 6850 4400
Wire Wire Line
	6850 4400 4400 4400
Wire Wire Line
	4400 4400 4400 4700
Wire Wire Line
	7650 4550 7650 6400
Wire Wire Line
	7650 6400 4700 6400
Wire Wire Line
	4700 6400 4700 6050
Wire Wire Line
	7550 4550 7550 6350
Wire Wire Line
	7550 6350 4800 6350
Wire Wire Line
	4800 6350 4800 6050
Wire Wire Line
	7450 4550 7450 4700
Wire Wire Line
	7450 4700 6900 4700
Wire Wire Line
	6900 4700 6900 4350
Wire Wire Line
	6900 4350 4900 4350
Wire Wire Line
	4900 4350 4900 4700
Wire Wire Line
	8750 2650 8750 5650
Wire Wire Line
	8800 2600 8800 5500
Wire Wire Line
	8800 5500 9350 5500
Wire Wire Line
	11100 4150 9250 4150
Wire Wire Line
	9250 4150 9250 4750
Wire Wire Line
	9250 4750 9350 4750
Wire Wire Line
	9350 4900 9200 4900
Wire Wire Line
	9200 4900 9200 4100
Wire Wire Line
	9200 4100 11050 4100
Connection ~ 6000 950 
Connection ~ 5900 900 
Connection ~ 5800 850 
Connection ~ 5700 800 
Connection ~ 5600 750 
Connection ~ 5500 700 
Connection ~ 5400 650 
Connection ~ 5200 550 
Wire Wire Line
	6000 950  8450 950 
Wire Wire Line
	8450 950  8450 700 
Wire Wire Line
	8450 700  9900 700 
Wire Wire Line
	9900 700  9900 800 
Wire Wire Line
	5900 900  8400 900 
Wire Wire Line
	8400 900  8400 650 
Wire Wire Line
	8400 650  9700 650 
Wire Wire Line
	9700 650  9700 800 
Wire Wire Line
	5800 850  8350 850 
Wire Wire Line
	8350 850  8350 600 
Wire Wire Line
	8350 600  9500 600 
Wire Wire Line
	9500 600  9500 800 
Wire Wire Line
	5700 800  8500 800 
Wire Wire Line
	8500 800  8500 1750
Wire Wire Line
	8500 1750 9400 1750
Wire Wire Line
	9400 1750 9400 1700
Wire Wire Line
	5600 750  8550 750 
Wire Wire Line
	8550 750  8550 1800
Wire Wire Line
	8550 1800 9600 1800
Wire Wire Line
	9600 1800 9600 1700
Wire Wire Line
	5500 700  8300 700 
Wire Wire Line
	8300 700  8300 1850
Wire Wire Line
	8300 1850 9800 1850
Wire Wire Line
	9800 1850 9800 1700
Wire Wire Line
	5400 650  8250 650 
Wire Wire Line
	8250 650  8250 1900
Wire Wire Line
	8250 1900 9850 1900
Wire Wire Line
	9850 1900 9850 2350
Wire Wire Line
	5200 550  10250 550 
Text GLabel 9650 3250 3    60   Input ~ 0
3vRD
Text GLabel 9800 800  1    60   Input ~ 0
3vA0
Text GLabel 9600 800  1    60   Input ~ 0
3vA1
Text GLabel 9400 800  1    60   Input ~ 0
3vA2
Text GLabel 9300 1700 3    60   Input ~ 0
3vA3
Text GLabel 9500 1700 3    60   Input ~ 0
3vA4
Text GLabel 9700 1700 3    60   Input ~ 0
3vA5
Text GLabel 9750 2350 1    60   Input ~ 0
3vA6
Connection ~ 5400 3200
Connection ~ 5500 3250
Connection ~ 5600 3300
Connection ~ 6000 3400
Wire Wire Line
	5400 3200 8350 3200
Wire Wire Line
	8350 3200 8350 2000
Wire Wire Line
	8350 2000 9650 2000
Wire Wire Line
	9650 2000 9650 2350
Wire Wire Line
	5500 3250 8400 3250
Wire Wire Line
	8400 3250 8400 2050
Wire Wire Line
	8400 2050 9450 2050
Wire Wire Line
	9450 2050 9450 2350
Wire Wire Line
	6000 3400 9550 3400
Wire Wire Line
	9550 3400 9550 3250
Text GLabel 9550 2350 1    60   Input ~ 0
3vA7
Text GLabel 9350 2350 1    60   Input ~ 0
3vA8
Wire Wire Line
	5600 3300 9350 3300
Wire Wire Line
	9350 3300 9350 3250
Text GLabel 9250 3250 3    60   Input ~ 0
3vA9
Text GLabel 9450 3250 3    60   Input ~ 0
3vA13
Text GLabel 5600 6050 3    60   Input ~ 0
3vA0
Text GLabel 5500 6050 3    60   Input ~ 0
3vA1
Text GLabel 4200 6050 3    60   Input ~ 0
3vA2
Text GLabel 5400 4700 1    60   Input ~ 0
3vA3
Text GLabel 5400 6050 3    60   Input ~ 0
3vA4
Text GLabel 4200 4700 1    60   Input ~ 0
3vA5
Text GLabel 4100 6050 3    60   Input ~ 0
3vA6
Text GLabel 4000 6050 3    60   Input ~ 0
3vA7
Text GLabel 5200 4700 1    60   Input ~ 0
3vA8
Text GLabel 5200 6050 3    60   Input ~ 0
3vA9
Text GLabel 4100 4700 1    60   Input ~ 0
3vA13
Text GLabel 4400 6050 3    60   Input ~ 0
3vRD
Connection ~ 5200 2650
Text GLabel 5200 2650 2    60   Input ~ 0
5v
Text GLabel 4700 4700 1    60   Input ~ 0
GND
Text GLabel 5100 4700 1    60   Input ~ 0
3v
Text GLabel 8250 5400 3    60   Input ~ 0
3vRD
Wire Wire Line
	10250 550  10250 3450
Wire Wire Line
	10250 3450 9750 3450
Wire Wire Line
	9750 3450 9750 3250
Text Label 10250 2050 0    60   ~ 0
5vRD
Text Label 1900 3300 0    60   ~ 0
ROMSEL
$Comp
L Conn_01x01 J5
U 1 1 5ADD43A7
P 5200 6800
F 0 "J5" H 5200 6900 50  0000 C CNN
F 1 "BCM24" H 5200 6700 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 5200 6800 50  0001 C CNN
F 3 "" H 5200 6800 50  0001 C CNN
	1    5200 6800
	0    1    1    0   
$EndComp
$Comp
L Conn_01x01 J4
U 1 1 5ADD45DE
P 4800 6800
F 0 "J4" H 4800 6900 50  0000 C CNN
F 1 "BCM25" H 4800 6700 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 4800 6800 50  0001 C CNN
F 3 "" H 4800 6800 50  0001 C CNN
	1    4800 6800
	0    1    1    0   
$EndComp
Wire Wire Line
	4900 6050 4900 6500
Wire Wire Line
	4900 6500 4800 6500
Wire Wire Line
	4800 6500 4800 6600
Wire Wire Line
	5100 6050 5100 6500
Wire Wire Line
	5100 6500 5200 6500
Wire Wire Line
	5200 6500 5200 6600
$Comp
L Conn_01x01 J6
U 1 1 5ADD4C20
P 6350 4900
F 0 "J6" H 6350 5000 50  0000 C CNN
F 1 "BCM27" H 6350 4800 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 6350 4900 50  0001 C CNN
F 3 "" H 6350 4900 50  0001 C CNN
	1    6350 4900
	1    0    0    -1  
$EndComp
Wire Wire Line
	6150 4900 6150 4300
Wire Wire Line
	6150 4300 5300 4300
Wire Wire Line
	5300 4300 5300 4700
$Comp
L Conn_01x01 J7
U 1 1 5ADD4DE4
P 6450 5900
F 0 "J7" H 6450 6000 50  0000 C CNN
F 1 "3V" H 6450 5800 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 6450 5900 50  0001 C CNN
F 3 "" H 6450 5900 50  0001 C CNN
	1    6450 5900
	0    1    1    0   
$EndComp
$Comp
L Conn_01x01 J8
U 1 1 5ADD4E5F
P 6850 5900
F 0 "J8" H 6850 6000 50  0000 C CNN
F 1 "GND" H 6850 5800 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 6850 5900 50  0001 C CNN
F 3 "" H 6850 5900 50  0001 C CNN
	1    6850 5900
	0    1    1    0   
$EndComp
Text GLabel 6850 5700 1    60   Input ~ 0
GND
Text GLabel 6450 5700 1    60   Input ~ 0
3v
$Comp
L Conn_01x01 J11
U 1 1 5ADD5E6F
P 3650 4950
F 0 "J11" H 3650 5050 50  0000 C CNN
F 1 "BCM13" H 3650 4850 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 3650 4950 50  0001 C CNN
F 3 "" H 3650 4950 50  0001 C CNN
	1    3650 4950
	0    1    1    0   
$EndComp
Wire Wire Line
	3650 4750 3650 4350
Wire Wire Line
	3650 4350 4300 4350
Wire Wire Line
	4300 4350 4300 4700
$Comp
L Conn_01x01 J9
U 1 1 5ADD629E
P 3150 4600
F 0 "J9" H 3150 4700 50  0000 C CNN
F 1 "BCM10" H 3150 4500 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 3150 4600 50  0001 C CNN
F 3 "" H 3150 4600 50  0001 C CNN
	1    3150 4600
	-1   0    0    1   
$EndComp
$Comp
L Conn_01x01 J10
U 1 1 5ADD6490
P 3150 4950
F 0 "J10" H 3150 5050 50  0000 C CNN
F 1 "BCM11" H 3150 4850 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 3150 4950 50  0001 C CNN
F 3 "" H 3150 4950 50  0001 C CNN
	1    3150 4950
	-1   0    0    1   
$EndComp
Wire Wire Line
	3350 4950 3450 4950
Wire Wire Line
	3450 4950 3450 4300
Wire Wire Line
	3450 4300 4800 4300
Wire Wire Line
	4800 4300 4800 4700
Wire Wire Line
	3350 4600 5000 4600
Wire Wire Line
	5000 4600 5000 4700
$Comp
L C .1uf2
U 1 1 5AE8FA00
P 10350 2800
F 0 ".1uf2" H 10375 2900 50  0000 L CNN
F 1 "C" H 10375 2700 50  0000 L CNN
F 2 "Capacitors_THT:C_Disc_D5.0mm_W2.5mm_P5.00mm" H 10388 2650 50  0001 C CNN
F 3 "" H 10350 2800 50  0001 C CNN
	1    10350 2800
	1    0    0    -1  
$EndComp
Wire Wire Line
	9850 3350 9850 3250
Connection ~ 9850 3300
Wire Wire Line
	9850 3300 10350 3300
Wire Wire Line
	10350 3300 10350 2950
Wire Wire Line
	9250 2250 9250 2350
Connection ~ 9250 2300
Wire Wire Line
	9250 2300 10350 2300
Wire Wire Line
	10350 2300 10350 2650
Wire Wire Line
	9150 800  9300 800 
Connection ~ 9200 800 
$Comp
L C .1uf3
U 1 1 5AE9096D
P 10450 1000
F 0 ".1uf3" H 10475 1100 50  0000 L CNN
F 1 "C" H 10475 900 50  0000 L CNN
F 2 "Capacitors_THT:C_Disc_D5.0mm_W2.5mm_P5.00mm" H 10488 850 50  0001 C CNN
F 3 "" H 10450 1000 50  0001 C CNN
	1    10450 1000
	1    0    0    -1  
$EndComp
Wire Wire Line
	9900 1800 9900 1700
Connection ~ 9900 1750
Wire Wire Line
	9900 1750 10450 1750
Wire Wire Line
	10450 1750 10450 1150
Wire Wire Line
	10450 850  10450 750 
Wire Wire Line
	10450 750  9200 750 
Wire Wire Line
	9200 750  9200 800 
Wire Wire Line
	8350 4650 8350 4550
Wire Wire Line
	7450 3700 7450 3600
Connection ~ 7450 3650
Connection ~ 8350 4600
$Comp
L C .1uf1
U 1 1 5AE9161B
P 8950 4100
F 0 ".1uf1" H 8975 4200 50  0000 L CNN
F 1 "C" H 8975 4000 50  0000 L CNN
F 2 "Capacitors_THT:C_Disc_D5.0mm_W2.5mm_P5.00mm" H 8988 3950 50  0001 C CNN
F 3 "" H 8950 4100 50  0001 C CNN
	1    8950 4100
	1    0    0    -1  
$EndComp
Wire Wire Line
	7450 3650 8950 3650
Wire Wire Line
	8950 3650 8950 3950
Wire Wire Line
	8350 4600 8950 4600
Wire Wire Line
	8950 4600 8950 4250
$Comp
L Conn_01x01 J12
U 1 1 5AE92E68
P 8500 5050
F 0 "J12" H 8500 5150 50  0000 C CNN
F 1 "/OE" H 8500 4950 50  0000 C CNN
F 2 "Pin_Headers:Pin_Header_Straight_1x01_Pitch2.54mm" H 8500 5050 50  0001 C CNN
F 3 "" H 8500 5050 50  0001 C CNN
	1    8500 5050
	1    0    0    -1  
$EndComp
Wire Wire Line
	8250 4550 8250 5400
Wire Wire Line
	8300 5050 8250 5050
Connection ~ 8250 5050
$EndSCHEMATC
