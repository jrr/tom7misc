EESchema Schematic File Version 4
EELAYER 26 0
EELAYER END
$Descr A4 8268 11693 portrait
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
L MCU_ST_STM32F3:STM32F303RDTx U1
U 1 1 5C7160A4
P 2050 3300
F 0 "U1" H 2050 1414 50  0000 C CNN
F 1 "STM32F303RDTx" H 2050 1323 50  0000 C CNN
F 2 "Package_QFP:LQFP-64_10x10mm_P0.5mm" H 1450 1600 50  0001 R CNN
F 3 "http://www.st.com/st-web-ui/static/active/en/resource/technical/document/datasheet/DM00118585.pdf" H 2050 3300 50  0001 C CNN
	1    2050 3300
	1    0    0    -1  
$EndComp
$Comp
L Connector:Raspberry_Pi_2_3 J7
U 1 1 5C71FEBF
P 6550 8200
F 0 "J7" H 6550 9678 50  0000 C CNN
F 1 "Raspberry_Pi_2_3" H 6550 9587 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x20_P2.54mm_Vertical" H 6550 8200 50  0001 C CNN
F 3 "https://www.raspberrypi.org/documentation/hardware/raspberrypi/schematics/rpi_SCH_3bplus_1p0_reduced.pdf" H 6550 8200 50  0001 C CNN
	1    6550 8200
	1    0    0    -1  
$EndComp
Text GLabel 7350 7300 2    50   Input ~ 0
a0
Text GLabel 7350 7400 2    50   Input ~ 0
a1
Text GLabel 7350 7600 2    50   Input ~ 0
a2
Text GLabel 7350 7700 2    50   Input ~ 0
b0
Text GLabel 7350 7900 2    50   Input ~ 0
b1
Text GLabel 7350 8000 2    50   Input ~ 0
b2
Text GLabel 7350 8100 2    50   Input ~ 0
c0
Text GLabel 7350 8300 2    50   Input ~ 0
c1
Text GLabel 7350 8400 2    50   Input ~ 0
c2
Text GLabel 7350 8500 2    50   Input ~ 0
d0
Text GLabel 7350 8600 2    50   Input ~ 0
d1
Text GLabel 7350 8700 2    50   Input ~ 0
d2
Text GLabel 7350 9000 2    50   Input ~ 0
e1
Text GLabel 5750 7300 0    50   Input ~ 0
e2
Text GLabel 5750 7400 0    50   Input ~ 0
f0
Text GLabel 5750 7600 0    50   Input ~ 0
f1
Text GLabel 5750 7700 0    50   Input ~ 0
f2
Text GLabel 5750 7800 0    50   Input ~ 0
g0
Text GLabel 5750 8000 0    50   Input ~ 0
g1
Text GLabel 5750 8100 0    50   Input ~ 0
g2
Text GLabel 5750 8200 0    50   Input ~ 0
h0
Text GLabel 5750 8400 0    50   Input ~ 0
h1
Text GLabel 5750 8500 0    50   Input ~ 0
h2
Text GLabel 7350 8900 2    50   Input ~ 0
e0
Text GLabel 5750 8600 0    50   Input ~ 0
i0
Text GLabel 5750 8700 0    50   Input ~ 0
i1
Text GLabel 5750 8800 0    50   Input ~ 0
i2
Text GLabel 6650 6900 1    50   Input ~ 0
3v3
Text GLabel 6750 6900 1    50   Input ~ 0
3v3
NoConn ~ 6350 6900
NoConn ~ 6450 6900
Text GLabel 6850 9650 3    50   Input ~ 0
GND
Wire Wire Line
	6850 9650 6850 9550
Wire Wire Line
	6850 9550 6750 9550
Wire Wire Line
	6150 9550 6150 9500
Connection ~ 6850 9550
Wire Wire Line
	6850 9550 6850 9500
Wire Wire Line
	6250 9550 6250 9500
Connection ~ 6250 9550
Wire Wire Line
	6250 9550 6150 9550
Wire Wire Line
	6350 9550 6350 9500
Connection ~ 6350 9550
Wire Wire Line
	6350 9550 6250 9550
Wire Wire Line
	6450 9550 6450 9500
Connection ~ 6450 9550
Wire Wire Line
	6450 9550 6350 9550
Wire Wire Line
	6550 9550 6550 9500
Connection ~ 6550 9550
Wire Wire Line
	6550 9550 6450 9550
Wire Wire Line
	6650 9550 6650 9500
Connection ~ 6650 9550
Wire Wire Line
	6650 9550 6550 9550
Wire Wire Line
	6750 9550 6750 9500
Connection ~ 6750 9550
Wire Wire Line
	6750 9550 6650 9550
Text GLabel 3050 1700 2    50   Input ~ 0
a0
Text GLabel 3050 1800 2    50   Input ~ 0
a1
Text GLabel 3050 1900 2    50   Input ~ 0
a2
Wire Wire Line
	3050 1700 2850 1700
Wire Wire Line
	2850 1700 2850 2000
Wire Wire Line
	2850 2000 2750 2000
Connection ~ 2850 1700
Wire Wire Line
	2850 1700 2750 1700
Wire Wire Line
	2900 1800 2900 2100
Wire Wire Line
	2900 2100 2750 2100
Connection ~ 2900 1800
Wire Wire Line
	2900 1800 2750 1800
Wire Wire Line
	2950 1900 2950 2200
Wire Wire Line
	2950 2200 2750 2200
Connection ~ 2950 1900
Wire Wire Line
	2950 1900 2750 1900
Text GLabel 3050 2600 2    50   Input ~ 0
c0
Text GLabel 3050 2700 2    50   Input ~ 0
c1
Text GLabel 3050 2800 2    50   Input ~ 0
c2
Wire Wire Line
	2950 1900 3050 1900
Wire Wire Line
	2900 1800 3050 1800
Wire Wire Line
	3050 2600 2800 2600
Wire Wire Line
	2800 2600 2800 2900
Wire Wire Line
	2800 2900 2750 2900
Connection ~ 2800 2600
Wire Wire Line
	2800 2600 2750 2600
Text GLabel 3050 3800 2    50   Input ~ 0
d0
Text GLabel 3050 3900 2    50   Input ~ 0
d1
Text GLabel 3050 4000 2    50   Input ~ 0
d2
Wire Wire Line
	3050 3800 2850 3800
Wire Wire Line
	3050 3900 2900 3900
Wire Wire Line
	3050 4000 2950 4000
Wire Wire Line
	2850 3800 2850 4100
Wire Wire Line
	2850 4100 2750 4100
Connection ~ 2850 3800
Wire Wire Line
	2850 3800 2750 3800
Wire Wire Line
	2900 3900 2900 4200
Wire Wire Line
	2900 4200 2750 4200
Connection ~ 2900 3900
Wire Wire Line
	2900 3900 2750 3900
Wire Wire Line
	2950 4000 2950 4300
Wire Wire Line
	2950 4300 2750 4300
Connection ~ 2950 4000
Wire Wire Line
	2950 4000 2750 4000
Text GLabel 1050 4300 0    50   Input ~ 0
f0
Text GLabel 1050 4400 0    50   Input ~ 0
f1
Text GLabel 1050 4500 0    50   Input ~ 0
f2
Wire Wire Line
	1050 4400 1200 4400
Wire Wire Line
	1050 4500 1150 4500
Wire Wire Line
	1250 4300 1250 4600
Wire Wire Line
	1250 4600 1350 4600
Connection ~ 1250 4300
Wire Wire Line
	1250 4300 1350 4300
Text GLabel 1050 3400 0    50   Input ~ 0
b0
Text GLabel 1050 3500 0    50   Input ~ 0
b1
Text GLabel 1050 3600 0    50   Input ~ 0
b2
Text GLabel 1050 3700 0    50   Input ~ 0
e0
Text GLabel 1050 3800 0    50   Input ~ 0
e1
Text GLabel 1050 3900 0    50   Input ~ 0
e2
Wire Wire Line
	1050 3400 1350 3400
Wire Wire Line
	1050 3500 1350 3500
Wire Wire Line
	1050 3600 1350 3600
Wire Wire Line
	1050 3700 1350 3700
Wire Wire Line
	1050 3800 1350 3800
Wire Wire Line
	1050 3900 1350 3900
Wire Wire Line
	1050 4300 1250 4300
Wire Wire Line
	1200 4400 1200 4700
Wire Wire Line
	1200 4700 1350 4700
Connection ~ 1200 4400
Wire Wire Line
	1200 4400 1350 4400
Wire Wire Line
	1150 4500 1150 4800
Wire Wire Line
	1150 4800 1350 4800
Connection ~ 1150 4500
Wire Wire Line
	1150 4500 1350 4500
$Comp
L MCU_ST_STM32F3:STM32F303RDTx U3
U 1 1 5C747FA6
P 5150 3300
F 0 "U3" H 5150 1414 50  0000 C CNN
F 1 "STM32F303RDTx" H 5150 1323 50  0000 C CNN
F 2 "Package_QFP:LQFP-64_10x10mm_P0.5mm" H 4550 1600 50  0001 R CNN
F 3 "http://www.st.com/st-web-ui/static/active/en/resource/technical/document/datasheet/DM00118585.pdf" H 5150 3300 50  0001 C CNN
	1    5150 3300
	1    0    0    -1  
$EndComp
Wire Wire Line
	3900 3400 4450 3400
Wire Wire Line
	3850 3500 4450 3500
Wire Wire Line
	3800 3350 3800 3600
Wire Wire Line
	3800 3600 4450 3600
Wire Wire Line
	2750 3500 3750 3500
Wire Wire Line
	3750 3500 3750 3700
Wire Wire Line
	3750 3700 4450 3700
Wire Wire Line
	2750 3600 3700 3600
Wire Wire Line
	3700 3600 3700 3800
Wire Wire Line
	3700 3800 4450 3800
Wire Wire Line
	2750 3700 3650 3700
Wire Wire Line
	3650 3700 3650 3900
Wire Wire Line
	3650 3900 4450 3900
Wire Wire Line
	4450 4000 4250 4000
Wire Wire Line
	4100 4000 4100 4300
Wire Wire Line
	4100 4300 4450 4300
Wire Wire Line
	4450 4100 4300 4100
Wire Wire Line
	4150 4100 4150 4400
Wire Wire Line
	4150 4400 4450 4400
Wire Wire Line
	4450 4200 4350 4200
Wire Wire Line
	4200 4200 4200 4500
Wire Wire Line
	4200 4500 4450 4500
Wire Wire Line
	4250 4000 4250 4600
Wire Wire Line
	4250 4600 4450 4600
Connection ~ 4250 4000
Wire Wire Line
	4250 4000 4100 4000
Wire Wire Line
	4300 4100 4300 4700
Wire Wire Line
	4300 4700 4450 4700
Connection ~ 4300 4100
Wire Wire Line
	4300 4100 4150 4100
Wire Wire Line
	4350 4200 4350 4800
Wire Wire Line
	4350 4800 4450 4800
Connection ~ 4350 4200
Wire Wire Line
	4350 4200 4200 4200
Text Notes 4000 4500 0    118  ~ 0
x
Text Notes 3950 3350 0    118  ~ 0
~a
Text Notes 3700 4100 0    118  ~ 0
~c
Wire Wire Line
	2750 4400 3250 4400
Wire Wire Line
	3250 4400 3250 800 
Wire Wire Line
	3250 800  6000 800 
Wire Wire Line
	6000 800  6000 1700
Wire Wire Line
	6000 1700 5850 1700
Wire Wire Line
	2750 4500 3300 4500
Wire Wire Line
	3300 4500 3300 850 
Wire Wire Line
	3300 850  6050 850 
Wire Wire Line
	6050 850  6050 1800
Wire Wire Line
	6050 1800 5850 1800
Wire Wire Line
	2750 4600 3350 4600
Wire Wire Line
	3350 4600 3350 900 
Wire Wire Line
	3350 900  6100 900 
Wire Wire Line
	6100 900  6100 1900
Wire Wire Line
	6100 1900 5850 1900
Wire Wire Line
	2750 4700 3450 4700
Wire Wire Line
	3450 4700 3450 700 
Wire Wire Line
	3450 700  6200 700 
Wire Wire Line
	6200 700  6200 2000
Wire Wire Line
	6200 2000 5850 2000
Wire Wire Line
	2750 4800 3500 4800
Wire Wire Line
	3500 4800 3500 650 
Wire Wire Line
	3500 650  6250 650 
Wire Wire Line
	6250 650  6250 2100
Wire Wire Line
	6250 2100 5850 2100
Wire Wire Line
	2750 4900 3550 4900
Wire Wire Line
	3550 4900 3550 600 
Wire Wire Line
	3550 600  6300 600 
Wire Wire Line
	6300 600  6300 2200
Wire Wire Line
	6300 2200 5850 2200
Text Notes 3600 4850 0    118  ~ 0
~f
Text Notes 3600 1400 0    118  ~ 0
~d
Wire Wire Line
	5850 2300 6100 2300
Wire Wire Line
	6250 2300 6250 2600
Wire Wire Line
	6250 2600 5850 2600
Wire Wire Line
	5850 2400 6050 2400
Wire Wire Line
	6200 2400 6200 2700
Wire Wire Line
	6200 2700 5850 2700
Wire Wire Line
	5850 2500 6000 2500
Wire Wire Line
	6150 2500 6150 2800
Wire Wire Line
	6150 2800 5850 2800
Wire Wire Line
	6100 2300 6100 2900
Wire Wire Line
	6100 2900 5850 2900
Connection ~ 6100 2300
Wire Wire Line
	6100 2300 6250 2300
Wire Wire Line
	6050 2400 6050 3200
Wire Wire Line
	6050 3200 5850 3200
Connection ~ 6050 2400
Wire Wire Line
	6050 2400 6200 2400
Wire Wire Line
	6000 2500 6000 3400
Wire Wire Line
	6000 3400 5850 3400
Connection ~ 6000 2500
Wire Wire Line
	6000 2500 6150 2500
Text Notes 6150 2900 0    118  ~ 0
y
Wire Wire Line
	5850 4700 6100 4700
Wire Wire Line
	6100 4700 6100 4100
Wire Wire Line
	6100 4100 5850 4100
Wire Wire Line
	5850 4800 6150 4800
Wire Wire Line
	6150 4800 6150 4200
Wire Wire Line
	6150 4200 5850 4200
Wire Wire Line
	5850 4900 6200 4900
Wire Wire Line
	6200 4900 6200 4300
Wire Wire Line
	6200 4300 5850 4300
Wire Wire Line
	5850 4000 5950 4000
Wire Wire Line
	5950 4000 5950 3700
Wire Wire Line
	5950 3700 5850 3700
Wire Wire Line
	5850 3900 6000 3900
Wire Wire Line
	6000 3900 6000 3600
Wire Wire Line
	6000 3600 5850 3600
Wire Wire Line
	5850 3800 6050 3800
Wire Wire Line
	6050 3800 6050 3500
Wire Wire Line
	6050 3500 5850 3500
$Comp
L MCU_ST_STM32F3:STM32F303RDTx U2
U 1 1 5C843D5C
P 2050 7800
F 0 "U2" H 2050 5914 50  0000 C CNN
F 1 "STM32F303RDTx" H 2050 5823 50  0000 C CNN
F 2 "Package_QFP:LQFP-64_10x10mm_P0.5mm" H 1450 6100 50  0001 R CNN
F 3 "http://www.st.com/st-web-ui/static/active/en/resource/technical/document/datasheet/DM00118585.pdf" H 2050 7800 50  0001 C CNN
	1    2050 7800
	1    0    0    -1  
$EndComp
Wire Wire Line
	5850 4400 6500 4400
Wire Wire Line
	6500 4400 6500 5700
Wire Wire Line
	6500 5700 3050 5700
Wire Wire Line
	3050 5700 3050 6200
Wire Wire Line
	3050 6200 2900 6200
Wire Wire Line
	5850 4500 6450 4500
Wire Wire Line
	6450 4500 6450 5750
Wire Wire Line
	6450 5750 3100 5750
Wire Wire Line
	3100 5750 3100 6300
Wire Wire Line
	3100 6300 2950 6300
Wire Wire Line
	5850 4600 6400 4600
Wire Wire Line
	6400 4600 6400 5800
Wire Wire Line
	6400 5800 3150 5800
Wire Wire Line
	3150 5800 3150 6400
Wire Wire Line
	3150 6400 3000 6400
Text Notes 2900 5900 0    118  ~ 0
w
Wire Wire Line
	2900 6200 2900 6500
Wire Wire Line
	2900 6500 2750 6500
Connection ~ 2900 6200
Wire Wire Line
	2900 6200 2750 6200
Wire Wire Line
	2950 6300 2950 6600
Wire Wire Line
	2950 6600 2750 6600
Connection ~ 2950 6300
Wire Wire Line
	2950 6300 2750 6300
Wire Wire Line
	3000 6400 3000 6700
Wire Wire Line
	3000 6700 2750 6700
Connection ~ 3000 6400
Wire Wire Line
	3000 6400 2750 6400
Wire Wire Line
	1350 4000 700  4000
Wire Wire Line
	700  4000 700  7900
Wire Wire Line
	700  7900 1250 7900
Wire Wire Line
	1350 4100 750  4100
Wire Wire Line
	750  4100 750  8000
Wire Wire Line
	750  8000 1200 8000
Wire Wire Line
	1350 4200 800  4200
Wire Wire Line
	800  4200 800  8100
Wire Wire Line
	800  8100 1150 8100
Wire Wire Line
	1250 7900 1250 8200
Wire Wire Line
	1250 8200 1350 8200
Connection ~ 1250 7900
Wire Wire Line
	1250 7900 1350 7900
Wire Wire Line
	1200 8000 1200 8300
Wire Wire Line
	1200 8300 1350 8300
Connection ~ 1200 8000
Wire Wire Line
	1200 8000 1350 8000
Wire Wire Line
	1150 8100 1150 8400
Wire Wire Line
	1150 8400 1350 8400
Connection ~ 1150 8100
Wire Wire Line
	1150 8100 1350 8100
Text Notes 850  7800 0    118  ~ 0
z
Text GLabel 1050 8500 0    49   Input ~ 0
nz0
Text GLabel 1050 8600 0    49   Input ~ 0
nz1
Text GLabel 1050 8700 0    49   Input ~ 0
nz2
Wire Wire Line
	1050 8500 1350 8500
Wire Wire Line
	1050 8600 1350 8600
Wire Wire Line
	1050 8700 1350 8700
Text GLabel 3100 7400 2    49   Input ~ 0
nz0
Text GLabel 3100 7500 2    49   Input ~ 0
nz1
Text GLabel 3100 7600 2    49   Input ~ 0
nz2
Wire Wire Line
	3100 7400 2750 7400
Wire Wire Line
	3100 7500 3000 7500
Wire Wire Line
	3000 7500 3000 7700
Wire Wire Line
	3000 7700 2750 7700
Wire Wire Line
	3100 7600 3050 7600
Wire Wire Line
	3050 7600 3050 7900
Wire Wire Line
	3050 7900 2750 7900
Wire Wire Line
	2750 6800 2900 6800
Wire Wire Line
	2900 6800 2900 7100
Wire Wire Line
	2900 7100 2750 7100
Wire Wire Line
	2750 6900 2950 6900
Wire Wire Line
	2950 6900 2950 7200
Wire Wire Line
	2950 7200 2750 7200
Wire Wire Line
	2750 7000 3000 7000
Wire Wire Line
	3000 7000 3000 7300
Wire Wire Line
	3000 7300 2750 7300
Wire Wire Line
	2750 8000 2850 8000
Wire Wire Line
	3100 8000 3100 8300
Wire Wire Line
	3100 8300 2750 8300
Wire Wire Line
	2750 8100 2900 8100
Wire Wire Line
	3150 8100 3150 8400
Wire Wire Line
	3150 8400 2750 8400
Wire Wire Line
	2750 8200 2950 8200
Wire Wire Line
	3200 8200 3200 8500
Wire Wire Line
	3200 8500 2750 8500
Wire Wire Line
	2850 8000 2850 8600
Wire Wire Line
	2850 8600 2750 8600
Connection ~ 2850 8000
Wire Wire Line
	2850 8000 3100 8000
Wire Wire Line
	2900 8100 2900 8700
Wire Wire Line
	2900 8700 2750 8700
Connection ~ 2900 8100
Wire Wire Line
	2900 8100 3150 8100
Wire Wire Line
	2950 8200 2950 8800
Wire Wire Line
	2950 8800 2750 8800
Connection ~ 2950 8200
Wire Wire Line
	2950 8200 3200 8200
Text GLabel 3100 8900 2    49   Input ~ 0
i0
Text GLabel 3100 9000 2    49   Input ~ 0
i1
Text GLabel 3100 9100 2    49   Input ~ 0
i2
Wire Wire Line
	2750 8900 3100 8900
Wire Wire Line
	2750 9000 3100 9000
Wire Wire Line
	2750 9100 3100 9100
Wire Wire Line
	1350 8800 900  8800
Wire Wire Line
	900  8800 900  10150
Wire Wire Line
	900  10150 1500 10150
Wire Wire Line
	1500 10150 1500 10250
Wire Wire Line
	1350 8900 950  8900
Wire Wire Line
	950  8900 950  10100
Wire Wire Line
	950  10100 1600 10100
Wire Wire Line
	1600 10100 1600 10250
Wire Wire Line
	1350 9000 1000 9000
Wire Wire Line
	1700 10050 1700 10250
Wire Wire Line
	1350 9100 1100 9100
Wire Wire Line
	1100 9100 1100 9950
Wire Wire Line
	1100 9950 1800 9950
Wire Wire Line
	1800 9950 1800 10250
Wire Wire Line
	1350 9200 1150 9200
Wire Wire Line
	1150 9200 1150 9900
Wire Wire Line
	1150 9900 1900 9900
Wire Wire Line
	1900 9900 1900 10250
Wire Wire Line
	1350 9300 1200 9300
Wire Wire Line
	1200 9300 1200 9850
Wire Wire Line
	1200 9850 2000 9850
Wire Wire Line
	2000 9850 2000 10250
Wire Wire Line
	1000 9000 1000 10050
Wire Wire Line
	1000 10050 1700 10050
Wire Wire Line
	1350 9400 1250 9400
Wire Wire Line
	1250 9400 1250 9800
Wire Wire Line
	1250 9800 2100 9800
Wire Wire Line
	2100 9800 2100 10250
Wire Wire Line
	2750 9200 2900 9200
Wire Wire Line
	2900 9200 2900 9900
Wire Wire Line
	2900 9900 2200 9900
Wire Wire Line
	2200 9900 2200 10250
Wire Wire Line
	2750 9300 2850 9300
Wire Wire Line
	2850 9300 2850 9850
Wire Wire Line
	2850 9850 2300 9850
Wire Wire Line
	2300 9850 2300 10250
Wire Wire Line
	2750 9400 2800 9400
Wire Wire Line
	2800 9400 2800 9800
Wire Wire Line
	2800 9800 2400 9800
Wire Wire Line
	2400 9800 2400 10250
Wire Wire Line
	1350 4900 1250 4900
Wire Wire Line
	1250 4900 1250 5350
Wire Wire Line
	1250 5350 3400 5350
Wire Wire Line
	3400 5350 3400 10050
Wire Wire Line
	3400 10050 2500 10050
Wire Wire Line
	2500 10050 2500 10250
Wire Wire Line
	3500 5350 3500 10100
Wire Wire Line
	3500 10100 2600 10100
Wire Wire Line
	2600 10100 2600 10250
Text GLabel 2250 5100 3    49   Input ~ 0
GND
Text GLabel 2150 5100 3    49   Input ~ 0
GND
Text GLabel 2050 5100 3    49   Input ~ 0
GND
Text GLabel 1950 5100 3    49   Input ~ 0
GND
Text GLabel 1850 5100 3    49   Input ~ 0
GND
Text GLabel 4950 5100 3    49   Input ~ 0
GND
Text GLabel 5050 5100 3    49   Input ~ 0
GND
Text GLabel 5150 5100 3    49   Input ~ 0
GND
Text GLabel 5250 5100 3    49   Input ~ 0
GND
Text GLabel 5350 5100 3    49   Input ~ 0
GND
Text GLabel 1850 9600 3    49   Input ~ 0
GND
Text GLabel 1950 9600 3    49   Input ~ 0
GND
Text GLabel 2050 9600 3    49   Input ~ 0
GND
Text GLabel 2150 9600 3    49   Input ~ 0
GND
Text GLabel 2250 9600 3    49   Input ~ 0
GND
$Comp
L Device:C_Small C6
U 1 1 5CE4FA0B
P 1900 5700
F 0 "C6" H 1992 5746 50  0000 L CNN
F 1 ".1uf" H 1992 5655 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 1900 5700 50  0001 C CNN
F 3 "~" H 1900 5700 50  0001 C CNN
	1    1900 5700
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C8
U 1 1 5CE4FAE2
P 2050 5700
F 0 "C8" H 2142 5746 50  0000 L CNN
F 1 ".1uf" H 2142 5655 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2050 5700 50  0001 C CNN
F 3 "~" H 2050 5700 50  0001 C CNN
	1    2050 5700
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C10
U 1 1 5CE4FB45
P 2200 5700
F 0 "C10" H 2292 5746 50  0000 L CNN
F 1 ".1uf" H 2292 5655 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2200 5700 50  0001 C CNN
F 3 "~" H 2200 5700 50  0001 C CNN
	1    2200 5700
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C12
U 1 1 5CE4FBA0
P 2350 5700
F 0 "C12" H 2442 5746 50  0000 L CNN
F 1 ".1uf" H 2442 5655 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2350 5700 50  0001 C CNN
F 3 "~" H 2350 5700 50  0001 C CNN
	1    2350 5700
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C14
U 1 1 5CE4FBF9
P 2500 5700
F 0 "C14" H 2592 5746 50  0000 L CNN
F 1 ".1uf" H 2592 5655 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2500 5700 50  0001 C CNN
F 3 "~" H 2500 5700 50  0001 C CNN
	1    2500 5700
	1    0    0    -1  
$EndComp
Text GLabel 1750 5900 0    49   Input ~ 0
3v3
Wire Wire Line
	1750 5900 1850 5900
Wire Wire Line
	2350 5900 2350 6000
Wire Wire Line
	2250 5900 2250 6000
Connection ~ 2250 5900
Wire Wire Line
	2250 5900 2350 5900
Wire Wire Line
	2150 5900 2150 6000
Connection ~ 2150 5900
Wire Wire Line
	2150 5900 2250 5900
Wire Wire Line
	2050 5900 2050 6000
Connection ~ 2050 5900
Wire Wire Line
	2050 5900 2150 5900
Wire Wire Line
	1950 5900 1950 6000
Connection ~ 1950 5900
Wire Wire Line
	1950 5900 2050 5900
Wire Wire Line
	1850 5900 1850 6000
Connection ~ 1850 5900
Wire Wire Line
	1850 5900 1950 5900
Wire Wire Line
	1950 5900 1950 5850
Wire Wire Line
	1950 5850 1900 5850
Wire Wire Line
	1900 5850 1900 5800
Wire Wire Line
	2050 5900 2050 5800
Wire Wire Line
	2150 5900 2150 5850
Wire Wire Line
	2150 5850 2200 5850
Wire Wire Line
	2200 5850 2200 5800
Wire Wire Line
	2250 5900 2250 5850
Wire Wire Line
	2250 5850 2350 5850
Wire Wire Line
	2350 5850 2350 5800
Wire Wire Line
	2350 5900 2500 5900
Wire Wire Line
	2500 5900 2500 5800
Connection ~ 2350 5900
Text GLabel 2700 5550 2    49   Input ~ 0
GND
Wire Wire Line
	2700 5550 2500 5550
Wire Wire Line
	1900 5550 1900 5600
Wire Wire Line
	2050 5550 2050 5600
Connection ~ 2050 5550
Wire Wire Line
	2050 5550 1900 5550
Wire Wire Line
	2200 5550 2200 5600
Connection ~ 2200 5550
Wire Wire Line
	2200 5550 2050 5550
Wire Wire Line
	2350 5550 2350 5600
Connection ~ 2350 5550
Wire Wire Line
	2350 5550 2200 5550
Wire Wire Line
	2500 5550 2500 5600
Connection ~ 2500 5550
Wire Wire Line
	2500 5550 2350 5550
$Comp
L Device:C_Small C5
U 1 1 5D141B16
P 1900 1150
F 0 "C5" H 1992 1196 50  0000 L CNN
F 1 ".1uf" H 1992 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 1900 1150 50  0001 C CNN
F 3 "~" H 1900 1150 50  0001 C CNN
	1    1900 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C7
U 1 1 5D141B1C
P 2050 1150
F 0 "C7" H 2142 1196 50  0000 L CNN
F 1 ".1uf" H 2142 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2050 1150 50  0001 C CNN
F 3 "~" H 2050 1150 50  0001 C CNN
	1    2050 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C9
U 1 1 5D141B22
P 2200 1150
F 0 "C9" H 2292 1196 50  0000 L CNN
F 1 ".1uf" H 2292 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2200 1150 50  0001 C CNN
F 3 "~" H 2200 1150 50  0001 C CNN
	1    2200 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C11
U 1 1 5D141B28
P 2350 1150
F 0 "C11" H 2442 1196 50  0000 L CNN
F 1 ".1uf" H 2442 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2350 1150 50  0001 C CNN
F 3 "~" H 2350 1150 50  0001 C CNN
	1    2350 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C13
U 1 1 5D141B2E
P 2500 1150
F 0 "C13" H 2592 1196 50  0000 L CNN
F 1 ".1uf" H 2592 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 2500 1150 50  0001 C CNN
F 3 "~" H 2500 1150 50  0001 C CNN
	1    2500 1150
	1    0    0    -1  
$EndComp
Text GLabel 1750 1350 0    49   Input ~ 0
3v3
Wire Wire Line
	1750 1350 1850 1350
Wire Wire Line
	2350 1350 2350 1500
Wire Wire Line
	2250 1350 2250 1500
Connection ~ 2250 1350
Wire Wire Line
	2250 1350 2350 1350
Wire Wire Line
	2150 1350 2150 1500
Connection ~ 2150 1350
Wire Wire Line
	2150 1350 2250 1350
Wire Wire Line
	2050 1350 2050 1500
Connection ~ 2050 1350
Wire Wire Line
	2050 1350 2150 1350
Wire Wire Line
	1950 1350 1950 1500
Connection ~ 1950 1350
Wire Wire Line
	1950 1350 2050 1350
Wire Wire Line
	1850 1350 1850 1500
Connection ~ 1850 1350
Wire Wire Line
	1850 1350 1950 1350
Wire Wire Line
	1950 1350 1950 1300
Wire Wire Line
	1950 1300 1900 1300
Wire Wire Line
	1900 1300 1900 1250
Wire Wire Line
	2050 1350 2050 1250
Wire Wire Line
	2150 1350 2150 1300
Wire Wire Line
	2150 1300 2200 1300
Wire Wire Line
	2200 1300 2200 1250
Wire Wire Line
	2250 1350 2250 1300
Wire Wire Line
	2250 1300 2350 1300
Wire Wire Line
	2350 1300 2350 1250
Wire Wire Line
	2350 1350 2500 1350
Wire Wire Line
	2500 1350 2500 1250
Connection ~ 2350 1350
Text GLabel 2700 1000 2    49   Input ~ 0
GND
Wire Wire Line
	2700 1000 2500 1000
Wire Wire Line
	1900 1000 1900 1050
Wire Wire Line
	2050 1000 2050 1050
Connection ~ 2050 1000
Wire Wire Line
	2050 1000 1900 1000
Wire Wire Line
	2200 1000 2200 1050
Connection ~ 2200 1000
Wire Wire Line
	2200 1000 2050 1000
Wire Wire Line
	2350 1000 2350 1050
Connection ~ 2350 1000
Wire Wire Line
	2350 1000 2200 1000
Wire Wire Line
	2500 1000 2500 1050
Connection ~ 2500 1000
Wire Wire Line
	2500 1000 2350 1000
$Comp
L Device:C_Small C17
U 1 1 5D3C923D
P 5000 1150
F 0 "C17" H 5092 1196 50  0000 L CNN
F 1 ".1uf" H 5092 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 5000 1150 50  0001 C CNN
F 3 "~" H 5000 1150 50  0001 C CNN
	1    5000 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C18
U 1 1 5D3C9243
P 5150 1150
F 0 "C18" H 5242 1196 50  0000 L CNN
F 1 ".1uf" H 5242 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 5150 1150 50  0001 C CNN
F 3 "~" H 5150 1150 50  0001 C CNN
	1    5150 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C19
U 1 1 5D3C9249
P 5300 1150
F 0 "C19" H 5392 1196 50  0000 L CNN
F 1 ".1uf" H 5392 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 5300 1150 50  0001 C CNN
F 3 "~" H 5300 1150 50  0001 C CNN
	1    5300 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C20
U 1 1 5D3C924F
P 5450 1150
F 0 "C20" H 5542 1196 50  0000 L CNN
F 1 ".1uf" H 5542 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 5450 1150 50  0001 C CNN
F 3 "~" H 5450 1150 50  0001 C CNN
	1    5450 1150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C21
U 1 1 5D3C9255
P 5600 1150
F 0 "C21" H 5692 1196 50  0000 L CNN
F 1 ".1uf" H 5692 1105 50  0000 C CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 5600 1150 50  0001 C CNN
F 3 "~" H 5600 1150 50  0001 C CNN
	1    5600 1150
	1    0    0    -1  
$EndComp
Text GLabel 4850 1350 0    49   Input ~ 0
3v3
Wire Wire Line
	4850 1350 4950 1350
Wire Wire Line
	5450 1350 5450 1500
Wire Wire Line
	5350 1350 5350 1500
Connection ~ 5350 1350
Wire Wire Line
	5350 1350 5450 1350
Wire Wire Line
	5250 1350 5250 1500
Connection ~ 5250 1350
Wire Wire Line
	5250 1350 5350 1350
Wire Wire Line
	5150 1350 5150 1500
Connection ~ 5150 1350
Wire Wire Line
	5150 1350 5250 1350
Wire Wire Line
	5050 1350 5050 1500
Connection ~ 5050 1350
Wire Wire Line
	5050 1350 5150 1350
Wire Wire Line
	4950 1350 4950 1500
Connection ~ 4950 1350
Wire Wire Line
	4950 1350 5050 1350
Wire Wire Line
	5050 1350 5050 1300
Wire Wire Line
	5050 1300 5000 1300
Wire Wire Line
	5000 1300 5000 1250
Wire Wire Line
	5150 1350 5150 1250
Wire Wire Line
	5250 1350 5250 1300
Wire Wire Line
	5250 1300 5300 1300
Wire Wire Line
	5300 1300 5300 1250
Wire Wire Line
	5350 1350 5350 1300
Wire Wire Line
	5350 1300 5450 1300
Wire Wire Line
	5450 1300 5450 1250
Wire Wire Line
	5450 1350 5600 1350
Wire Wire Line
	5600 1350 5600 1250
Connection ~ 5450 1350
Text GLabel 5700 1000 2    49   Input ~ 0
GND
Wire Wire Line
	5700 1000 5600 1000
Wire Wire Line
	5000 1000 5000 1050
Wire Wire Line
	5150 1000 5150 1050
Connection ~ 5150 1000
Wire Wire Line
	5150 1000 5000 1000
Wire Wire Line
	5300 1000 5300 1050
Connection ~ 5300 1000
Wire Wire Line
	5300 1000 5150 1000
Wire Wire Line
	5450 1000 5450 1050
Connection ~ 5450 1000
Wire Wire Line
	5450 1000 5300 1000
Wire Wire Line
	5600 1000 5600 1050
Connection ~ 5600 1000
Wire Wire Line
	5600 1000 5450 1000
Wire Wire Line
	4450 4900 4350 4900
Wire Wire Line
	4350 4900 4350 5350
Wire Wire Line
	4350 5350 3500 5350
$Comp
L Connector_Generic:Conn_01x13 J1
U 1 1 5D8EF54B
P 2100 10450
F 0 "J1" V 2224 10446 50  0000 C CNN
F 1 "Conn_01x13" V 2315 10446 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_1x13_P2.54mm_Vertical" H 2100 10450 50  0001 C CNN
F 3 "~" H 2100 10450 50  0001 C CNN
	1    2100 10450
	0    1    1    0   
$EndComp
Wire Wire Line
	2700 10250 2700 9950
Wire Wire Line
	2700 9950 3300 9950
Wire Wire Line
	3300 9950 3300 8900
Wire Wire Line
	3300 8900 5750 8900
$Comp
L Device:Crystal Y2
U 1 1 5DBA2C33
P 1050 7150
F 0 "Y2" H 1050 7418 50  0000 C CNN
F 1 "8 MHz" H 1050 7327 50  0000 C BNN
F 2 "Crystal:Crystal_HC49-U_Vertical" H 1050 7150 50  0001 C CNN
F 3 "~" H 1050 7150 50  0001 C CNN
	1    1050 7150
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C2
U 1 1 5DBA2DB8
P 900 6950
F 0 "C2" H 992 6996 50  0000 L CNN
F 1 "18pf" H 992 6905 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 900 6950 50  0001 C CNN
F 3 "~" H 900 6950 50  0001 C CNN
	1    900  6950
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C4
U 1 1 5DBA2E52
P 1200 6950
F 0 "C4" H 1292 6996 50  0000 L CNN
F 1 "18pf" H 1292 6905 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 1200 6950 50  0001 C CNN
F 3 "~" H 1200 6950 50  0001 C CNN
	1    1200 6950
	1    0    0    -1  
$EndComp
Wire Wire Line
	1200 7050 1200 7100
Wire Wire Line
	1200 7100 1300 7100
Wire Wire Line
	1300 7100 1300 7400
Wire Wire Line
	1300 7400 1350 7400
Connection ~ 1200 7100
Wire Wire Line
	1200 7100 1200 7150
Wire Wire Line
	900  7050 900  7100
Wire Wire Line
	900  7100 850  7100
Wire Wire Line
	850  7100 850  7500
Wire Wire Line
	850  7500 1350 7500
Connection ~ 900  7100
Wire Wire Line
	900  7100 900  7150
Text GLabel 1050 6650 1    49   Input ~ 0
GND
Wire Wire Line
	900  6850 900  6750
Wire Wire Line
	900  6750 1050 6750
Wire Wire Line
	1200 6750 1200 6850
Wire Wire Line
	1050 6750 1050 6650
Connection ~ 1050 6750
Wire Wire Line
	1050 6750 1200 6750
$Comp
L Device:Crystal Y1
U 1 1 5DDD933F
P 1000 2650
F 0 "Y1" H 1000 2918 50  0000 C CNN
F 1 "8 MHz" H 1000 2827 50  0000 C BNN
F 2 "Crystal:Crystal_HC49-U_Vertical" H 1000 2650 50  0001 C CNN
F 3 "~" H 1000 2650 50  0001 C CNN
	1    1000 2650
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C1
U 1 1 5DDD9345
P 850 2450
F 0 "C1" H 942 2496 50  0000 L CNN
F 1 "18pf" H 942 2405 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 850 2450 50  0001 C CNN
F 3 "~" H 850 2450 50  0001 C CNN
	1    850  2450
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C3
U 1 1 5DDD934B
P 1150 2450
F 0 "C3" H 1242 2496 50  0000 L CNN
F 1 "18pf" H 1242 2405 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 1150 2450 50  0001 C CNN
F 3 "~" H 1150 2450 50  0001 C CNN
	1    1150 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	1150 2550 1150 2600
Wire Wire Line
	1150 2600 1250 2600
Wire Wire Line
	1250 2600 1250 2900
Wire Wire Line
	1250 2900 1350 2900
Connection ~ 1150 2600
Wire Wire Line
	1150 2600 1150 2650
Wire Wire Line
	850  2550 850  2600
Wire Wire Line
	850  2600 800  2600
Wire Wire Line
	800  2600 800  3000
Wire Wire Line
	800  3000 1350 3000
Connection ~ 850  2600
Wire Wire Line
	850  2600 850  2650
Text GLabel 1000 2150 1    49   Input ~ 0
GND
Wire Wire Line
	850  2350 850  2250
Wire Wire Line
	850  2250 1000 2250
Wire Wire Line
	1150 2250 1150 2350
Wire Wire Line
	1000 2250 1000 2150
Connection ~ 1000 2250
Wire Wire Line
	1000 2250 1150 2250
Wire Wire Line
	3800 3350 3600 3350
Wire Wire Line
	3600 3350 3600 2500
Wire Wire Line
	3600 2500 2750 2500
Wire Wire Line
	3850 3300 3650 3300
Wire Wire Line
	3650 2400 2750 2400
Wire Wire Line
	3850 3300 3850 3500
Wire Wire Line
	3650 2400 3650 3300
Wire Wire Line
	3900 3250 3700 3250
Wire Wire Line
	3700 3250 3700 2300
Wire Wire Line
	3700 2300 2750 2300
Wire Wire Line
	3900 3250 3900 3400
$Comp
L Device:Crystal Y3
U 1 1 5E7F902A
P 4150 2650
F 0 "Y3" H 4150 2918 50  0000 C CNN
F 1 "8 MHz" H 4150 2827 50  0000 C BNN
F 2 "Crystal:Crystal_HC49-U_Vertical" H 4150 2650 50  0001 C CNN
F 3 "~" H 4150 2650 50  0001 C CNN
	1    4150 2650
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C15
U 1 1 5E7F9030
P 4000 2450
F 0 "C15" H 4092 2496 50  0000 L CNN
F 1 "18pf" H 4092 2405 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 4000 2450 50  0001 C CNN
F 3 "~" H 4000 2450 50  0001 C CNN
	1    4000 2450
	1    0    0    -1  
$EndComp
$Comp
L Device:C_Small C16
U 1 1 5E7F9036
P 4300 2450
F 0 "C16" H 4392 2496 50  0000 L CNN
F 1 "18pf" H 4392 2405 50  0000 L CNN
F 2 "Capacitor_THT:C_Disc_D6.0mm_W2.5mm_P5.00mm" H 4300 2450 50  0001 C CNN
F 3 "~" H 4300 2450 50  0001 C CNN
	1    4300 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	4300 2550 4300 2600
Wire Wire Line
	4300 2600 4400 2600
Wire Wire Line
	4400 2600 4400 2900
Wire Wire Line
	4400 2900 4450 2900
Connection ~ 4300 2600
Wire Wire Line
	4300 2600 4300 2650
Wire Wire Line
	4000 2550 4000 2600
Wire Wire Line
	4000 2600 3950 2600
Wire Wire Line
	3950 2600 3950 3000
Wire Wire Line
	3950 3000 4450 3000
Connection ~ 4000 2600
Wire Wire Line
	4000 2600 4000 2650
Text GLabel 4150 2150 1    49   Input ~ 0
GND
Wire Wire Line
	4000 2350 4000 2250
Wire Wire Line
	4000 2250 4150 2250
Wire Wire Line
	4300 2250 4300 2350
Wire Wire Line
	4150 2250 4150 2150
Connection ~ 4150 2250
Wire Wire Line
	4150 2250 4300 2250
Connection ~ 4450 2900
Wire Wire Line
	4450 2900 4500 2900
Connection ~ 4450 3000
Wire Wire Line
	4450 3000 4500 3000
$Comp
L boardparts:PTS645SJH73LFS SW1
U 1 1 5E8B971D
P 1000 1800
F 0 "SW1" H 1025 2223 49  0000 C CNN
F 1 "PTS645SJH73LFS" H 1025 2133 49  0000 C CNN
F 2 "footprints:PTS645SJH73LFS_FP" H 1000 1800 49  0001 C CNN
F 3 "" H 1000 1800 49  0001 C CNN
	1    1000 1800
	1    0    0    -1  
$EndComp
Wire Wire Line
	1150 1700 1200 1700
$Comp
L Device:R R1
U 1 1 5E93AB6C
P 1300 1400
F 0 "R1" H 1370 1446 50  0000 L CNN
F 1 "100k" H 1370 1355 50  0000 L CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P7.62mm_Horizontal" V 1230 1400 50  0001 C CNN
F 3 "~" H 1300 1400 50  0001 C CNN
	1    1300 1400
	1    0    0    -1  
$EndComp
Text GLabel 1300 1250 1    49   Input ~ 0
3v3
Wire Wire Line
	1300 1550 1300 1700
Connection ~ 1300 1700
Wire Wire Line
	1300 1700 1350 1700
Wire Wire Line
	900  1600 850  1600
Wire Wire Line
	800  1600 800  1750
Text GLabel 800  1750 3    49   Input ~ 0
GND
$Comp
L boardparts:PTS645SJH73LFS SW2
U 1 1 5E9BD0B5
P 4100 1800
F 0 "SW2" H 4125 2223 49  0000 C CNN
F 1 "PTS645SJH73LFS" H 4125 2133 49  0000 C CNN
F 2 "footprints:PTS645SJH73LFS_FP" H 4100 1800 49  0001 C CNN
F 3 "" H 4100 1800 49  0001 C CNN
	1    4100 1800
	1    0    0    -1  
$EndComp
Wire Wire Line
	4250 1700 4300 1700
$Comp
L Device:R R3
U 1 1 5E9BD0BC
P 4400 1400
F 0 "R3" H 4470 1446 50  0000 L CNN
F 1 "100k" H 4470 1355 50  0000 L CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P7.62mm_Horizontal" V 4330 1400 50  0001 C CNN
F 3 "~" H 4400 1400 50  0001 C CNN
	1    4400 1400
	1    0    0    -1  
$EndComp
Text GLabel 4400 1250 1    49   Input ~ 0
3v3
Wire Wire Line
	4400 1550 4400 1700
Connection ~ 4400 1700
Wire Wire Line
	4400 1700 4450 1700
Wire Wire Line
	4000 1600 3950 1600
Wire Wire Line
	3900 1600 3900 1750
Text GLabel 3900 1750 3    49   Input ~ 0
GND
$Comp
L boardparts:PTS645SJH73LFS SW3
U 1 1 5E9FFDCB
P 1100 6150
F 0 "SW3" H 1125 6573 49  0000 C CNN
F 1 "PTS645SJH73LFS" H 1125 6483 49  0000 C CNN
F 2 "footprints:PTS645SJH73LFS_FP" H 1100 6150 49  0001 C CNN
F 3 "" H 1100 6150 49  0001 C CNN
	1    1100 6150
	1    0    0    -1  
$EndComp
Wire Wire Line
	1250 6050 1300 6050
$Comp
L Device:R R2
U 1 1 5E9FFDD2
P 1400 5750
F 0 "R2" H 1470 5796 50  0000 L CNN
F 1 "100k" H 1470 5705 50  0000 L CNN
F 2 "Resistor_THT:R_Axial_DIN0207_L6.3mm_D2.5mm_P7.62mm_Horizontal" V 1330 5750 50  0001 C CNN
F 3 "~" H 1400 5750 50  0001 C CNN
	1    1400 5750
	1    0    0    -1  
$EndComp
Text GLabel 1400 5600 1    49   Input ~ 0
3v3
Wire Wire Line
	1400 5900 1400 6050
Wire Wire Line
	1000 5950 950  5950
Wire Wire Line
	900  5950 900  6100
Text GLabel 900  6100 3    49   Input ~ 0
GND
Wire Wire Line
	1300 6050 1300 6200
Wire Wire Line
	1300 6200 1350 6200
Connection ~ 1300 6050
Wire Wire Line
	1300 6050 1350 6050
NoConn ~ 1350 7700
NoConn ~ 1350 3200
NoConn ~ 4450 3200
$Comp
L Connector_Generic:Conn_02x02_Counter_Clockwise J4
U 1 1 5EB58DE9
P 3800 7750
F 0 "J4" H 3850 7967 50  0000 C CNN
F 1 "SWD3" H 3850 7876 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x02_P2.54mm_Vertical" H 3800 7750 50  0001 C CNN
F 3 "~" H 3800 7750 50  0001 C CNN
	1    3800 7750
	1    0    0    -1  
$EndComp
Wire Wire Line
	3600 7850 2850 7850
Wire Wire Line
	2850 7850 2850 7600
Wire Wire Line
	2850 7600 2750 7600
Wire Wire Line
	2750 7500 2900 7500
Wire Wire Line
	2900 7500 2900 7750
Wire Wire Line
	2900 7750 3600 7750
Text GLabel 4100 7750 2    49   Input ~ 0
BOOT3
Text GLabel 4100 7850 2    49   Input ~ 0
NRST3
Text GLabel 1350 6400 0    49   Input ~ 0
BOOT3
Text GLabel 1300 6200 0    49   Input ~ 0
NRST3
$Comp
L Connector_Generic:Conn_02x02_Counter_Clockwise J6
U 1 1 5ED3E7A7
P 6550 3000
F 0 "J6" H 6600 3217 50  0000 C CNN
F 1 "SWD2" H 6600 3126 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x02_P2.54mm_Vertical" H 6550 3000 50  0001 C CNN
F 3 "~" H 6550 3000 50  0001 C CNN
	1    6550 3000
	1    0    0    -1  
$EndComp
Wire Wire Line
	6350 3100 5850 3100
Wire Wire Line
	5850 3000 6350 3000
Text GLabel 6850 3000 2    49   Input ~ 0
BOOT2
Text GLabel 6850 3100 2    49   Input ~ 0
NRST2
Text GLabel 4450 1900 0    49   Input ~ 0
BOOT2
Text GLabel 4300 1800 0    49   Input ~ 0
NRST2
Wire Wire Line
	4300 1800 4350 1800
Wire Wire Line
	4350 1800 4350 1700
Connection ~ 4350 1700
Wire Wire Line
	4350 1700 4400 1700
$Comp
L Connector_Generic:Conn_02x02_Counter_Clockwise J2
U 1 1 5EEEA7E1
P 3450 5100
F 0 "J2" H 3500 5317 50  0000 C CNN
F 1 "SWD1" H 3500 5226 50  0000 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_2x02_P2.54mm_Vertical" H 3450 5100 50  0001 C CNN
F 3 "~" H 3450 5100 50  0001 C CNN
	1    3450 5100
	1    0    0    -1  
$EndComp
Text GLabel 3750 5100 2    49   Input ~ 0
BOOT1
Text GLabel 3750 5200 2    49   Input ~ 0
NRST1
Text GLabel 3250 5100 0    49   Input ~ 0
SWDIO1
Text GLabel 2750 3000 2    49   Input ~ 0
SWDIO1
Text GLabel 2750 3100 2    49   Input ~ 0
SWCLK1
Text GLabel 3250 5200 0    49   Input ~ 0
SWCLK1
Text GLabel 1350 1900 0    49   Input ~ 0
BOOT1
Text GLabel 1200 1800 0    49   Input ~ 0
NRST1
Wire Wire Line
	1200 1800 1250 1800
Wire Wire Line
	1250 1800 1250 1700
Connection ~ 1250 1700
Wire Wire Line
	1250 1700 1300 1700
Wire Wire Line
	2750 2800 3000 2800
Wire Wire Line
	3000 2800 3100 2900
Connection ~ 3000 2800
Wire Wire Line
	3000 2800 3050 2800
Wire Wire Line
	2750 3400 3200 3400
Wire Wire Line
	3200 3400 3200 2900
Wire Wire Line
	3200 2900 3100 2900
Wire Wire Line
	2750 2700 2850 2700
Wire Wire Line
	2850 2700 3100 2950
Connection ~ 2850 2700
Wire Wire Line
	2850 2700 3050 2700
Wire Wire Line
	3150 2950 3150 3200
Wire Wire Line
	2750 3200 3150 3200
Wire Wire Line
	3150 2950 3100 2950
$Comp
L Connector_Generic:Conn_01x01 J3
U 1 1 5F258F49
P 3750 6050
F 0 "J3" V 3716 5962 50  0000 R CNN
F 1 "gnd" V 3625 5962 50  0000 R CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_1x01_P2.54mm_Vertical" H 3750 6050 50  0001 C CNN
F 3 "~" H 3750 6050 50  0001 C CNN
	1    3750 6050
	0    -1   -1   0   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 J5
U 1 1 5F2590C0
P 4100 6050
F 0 "J5" V 4066 5962 50  0000 R CNN
F 1 "3v3" V 3975 5962 50  0000 R CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_1x01_P2.54mm_Vertical" H 4100 6050 50  0001 C CNN
F 3 "~" H 4100 6050 50  0001 C CNN
	1    4100 6050
	0    -1   -1   0   
$EndComp
Text GLabel 3750 6250 3    49   Input ~ 0
GND
Text GLabel 4100 6250 3    49   Input ~ 0
3v3
Text Notes 3700 10400 0    315  ~ 0
META FPU
Text Notes 4400 10750 0    79   ~ 0
by tom7
Text GLabel 3850 9250 0    49   Input ~ 0
g0
Text GLabel 3850 9350 0    49   Input ~ 0
g1
Text GLabel 3850 9450 0    49   Input ~ 0
g2
Text GLabel 4100 9150 1    49   Input ~ 0
GND
Text Notes 3700 9150 0    49   ~ 0
nan
Text GLabel 4450 9250 2    49   Input ~ 0
h0
Text GLabel 4450 9350 2    49   Input ~ 0
h1
Text GLabel 4450 9450 2    49   Input ~ 0
h2
Wire Wire Line
	4100 9250 4300 9250
Text Notes 4500 9150 0    49   ~ 0
inf
Wire Wire Line
	3850 9350 4000 9350
Wire Wire Line
	3850 9250 4100 9250
Connection ~ 4100 9250
Wire Wire Line
	4100 9150 4100 9250
Text GLabel 4200 9600 3    49   Input ~ 0
3v3
Wire Wire Line
	4200 9600 4200 9350
Connection ~ 4200 9350
Wire Wire Line
	4200 9350 4450 9350
Wire Wire Line
	4300 9250 4300 9450
Wire Wire Line
	4300 9450 4450 9450
Connection ~ 4300 9250
Wire Wire Line
	4300 9250 4450 9250
Wire Wire Line
	4000 9350 4000 9450
Wire Wire Line
	4000 9450 3850 9450
Connection ~ 4000 9350
Wire Wire Line
	4000 9350 4200 9350
Wire Wire Line
	4250 1600 4300 1600
Wire Wire Line
	4300 1600 4300 1550
Wire Wire Line
	4300 1550 3950 1550
Wire Wire Line
	3950 1550 3950 1600
Connection ~ 3950 1600
Wire Wire Line
	3950 1600 3900 1600
Wire Wire Line
	4300 1700 4300 1750
Wire Wire Line
	4300 1750 4000 1750
Wire Wire Line
	4000 1750 4000 1700
Connection ~ 4300 1700
Wire Wire Line
	4300 1700 4350 1700
Wire Wire Line
	1150 1600 1150 1550
Wire Wire Line
	1150 1550 850  1550
Wire Wire Line
	850  1550 850  1600
Connection ~ 850  1600
Wire Wire Line
	850  1600 800  1600
Wire Wire Line
	900  1700 900  1750
Wire Wire Line
	900  1750 1200 1750
Wire Wire Line
	1200 1750 1200 1700
Connection ~ 1200 1700
Wire Wire Line
	1200 1700 1250 1700
Wire Wire Line
	1250 5950 1250 5900
Wire Wire Line
	1250 5900 950  5900
Wire Wire Line
	950  5900 950  5950
Connection ~ 950  5950
Wire Wire Line
	950  5950 900  5950
Wire Wire Line
	1000 6050 1000 6100
Wire Wire Line
	1000 6100 1350 6100
Wire Wire Line
	1350 6100 1350 6050
Connection ~ 1350 6050
Wire Wire Line
	1350 6050 1400 6050
$Comp
L power:+3.3V #PWR0101
U 1 1 5FD4B8D0
P 4450 6050
F 0 "#PWR0101" H 4450 5900 50  0001 C CNN
F 1 "+3.3V" H 4465 6223 50  0000 C CNN
F 2 "" H 4450 6050 50  0001 C CNN
F 3 "" H 4450 6050 50  0001 C CNN
	1    4450 6050
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0102
U 1 1 5FD4B957
P 4700 6050
F 0 "#PWR0102" H 4700 5800 50  0001 C CNN
F 1 "GND" H 4705 5877 50  0000 C CNN
F 2 "" H 4700 6050 50  0001 C CNN
F 3 "" H 4700 6050 50  0001 C CNN
	1    4700 6050
	1    0    0    -1  
$EndComp
Text GLabel 4700 6050 1    49   Input ~ 0
GND
Text GLabel 4450 6050 3    49   Input ~ 0
3v3
$EndSCHEMATC
