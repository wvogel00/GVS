from serial import Serial
from time import sleep

def gvsSerial(com):
	gvs = Serial(
	port = com,
	baudrate = 115200,
	bytesize = 8,
	parity = 'N',
	stopbits = 1,
	timeout = None,
	xonxoff = 0,
	rtscts = 0,
	writeTimeout = None,
	dsrdtr = None )
	
	return gvs

def gvsCommand(gvs,channel,value):
	#send1 8bit : channel(3bit) + pole(1bit) + value(4bit)
	#send2 8bit : value(8bit)
	pole = 16 if value >= 0 else 0	#set the pole of currency
	value = abs(value)
	send1 = (channel << 5) + pole + ((value >> 8) & 0b1111 )	#set GVS-channel and value(4bit from 12bit)
	send2 = (value & 0b11111111)	#set the rest value(8bit, total 12bit)
	
	data = [send1,send2]
	gvs.write(''.join(chr(v) for v in data))		#convert to byte array
	sleep(0.001)	#delay 1 ms

#interval(sec)
def MakeRecWave(gvs, channel, value, interval):
	for i in range(20):
		gvsCommand(gvs,channel,value)
		sleep(interval)
		gvsCommand(gvs,channel,0)
		sleep(interval)

def main():

	a = "hello"
	print a
	gvs = gvsSerial("COM9")		#setup GVS serial port
	print gvs.portstr
	MakeRecWave(gvs,0,4000,0.05);	#generate GVS stimulus with rectagular wave
	print ("end")

if __name__ == '__main__':
	main()


# python -m pip <module>