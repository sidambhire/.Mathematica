#!/bin/bash
# see comment section in accompanying file init.m for instructions
# created February 2008 by Sascha Kratky, kratky@unisoftwareplus.com

cd "`dirname \"$0\"`"

LOGFILE="`basename \"$0\" .sh`.log"

echo `hostname` `date` >> $LOGFILE
echo $0 $@ >> $LOGFILE

# check if we are being called as SSH_ASKPASS helper script
if [ -n "$REMOTE_KERNEL_PASSWORD" -a "$0" == "$SSH_ASKPASS" ]
then
# just output the password that has been passed as an environment variable
	echo $REMOTE_KERNEL_PASSWORD
	exit 0
fi

# check arguments
if [ "$#" -ne "3" ]
then
	echo "Usage: $0 [user[:password]@]host[:port] path_to_mathematica_kernel linkname" >> $LOGFILE
	exit 1
fi

if [ ! -x "/usr/bin/ssh" ]
then
	echo "Error: OpenSSH client /usr/bin/ssh does not exist." >> $LOGFILE
	exit 1
fi

REMOTE_KERNEL_HOST=$1
REMOTE_KERNEL_PATH=$2
LINK_NAME=$3

# parse port link name port numbers, e.g., 53994@127.0.0.1,39359@127.0.0.1
MAIN_LINK_DATA_PORT=`echo $LINK_NAME | awk -F "[,@]" '{print $1}'`
MAIN_LINK_HOST=`echo $LINK_NAME | awk -F "[,@]" '{print $2}'`
MAIN_LINK_MESSAGE_PORT=`echo $LINK_NAME | awk -F "[,@]" '{print $3}'`

if [ -z "$MAIN_LINK_DATA_PORT" -o -z "$MAIN_LINK_MESSAGE_PORT" ]
then
	echo "Error: $LINK_NAME is not a properly formatted MathLink TCPIP protocol link name!" >> $LOGFILE
	exit 1
fi

if [ "$MAIN_LINK_HOST" != "127.0.0.1" ]
then
	echo "Error: $LINK_NAME does not use the loopback IP address 127.0.0.1!" >> $LOGFILE
	exit 1
fi

# ssh options
# ssh must not run as a background process (don't use option -f)
SSH_OPTS="-C -v -x -n"

# parse user credentials from host name
REMOTE_KERNEL_USER=`echo $REMOTE_KERNEL_HOST | awk -F "[@]" '{print $1}'`
REMOTE_KERNEL_HOST=`echo $REMOTE_KERNEL_HOST | awk -F "[@]" '{print $2}'`
if [ -z "$REMOTE_KERNEL_HOST" ]
then
	REMOTE_KERNEL_HOST=$REMOTE_KERNEL_USER
	REMOTE_KERNEL_USER=""
fi

# parse password from user credentials
if [ -n "$REMOTE_KERNEL_USER" ]
then
	REMOTE_KERNEL_PASSWORD=`echo $REMOTE_KERNEL_USER | awk -F "[:]" '{print $2}'`
	REMOTE_KERNEL_USER=`echo $REMOTE_KERNEL_USER | awk -F "[:]" '{print $1}'`
fi

# parse SSH port number from host name
REMOTE_KERNEL_PORT=`echo $REMOTE_KERNEL_HOST | awk -F "[:]" '{print $2}'`
REMOTE_KERNEL_HOST=`echo $REMOTE_KERNEL_HOST | awk -F "[:]" '{print $1}'`

# add optional command line options
if [ -n "$REMOTE_KERNEL_USER" ]
then
	SSH_OPTS="$SSH_OPTS -l $REMOTE_KERNEL_USER"
fi
if [ -n "$REMOTE_KERNEL_PASSWORD" ]
then
# login password cannot be specified as a command line option to ssh, use SSH_ASKPASS trick
	export DISPLAY=none:0.0
	export SSH_ASKPASS=$0
	export REMOTE_KERNEL_PASSWORD
fi
if [ -n "$REMOTE_KERNEL_PORT" ]
then
	SSH_OPTS="$SSH_OPTS -p $REMOTE_KERNEL_PORT"
fi

# compute port numbers to be used for preemptive and service links
let "BASE_PORT = MAIN_LINK_DATA_PORT > MAIN_LINK_MESSAGE_PORT ? MAIN_LINK_DATA_PORT : MAIN_LINK_MESSAGE_PORT"
let "PREEMPTIVE_LINK_DATA_PORT = BASE_PORT + 1"
let "PREEMPTIVE_LINK_MESSAGE_PORT = BASE_PORT + 2"
let "SERVICE_LINK_DATA_PORT = BASE_PORT + 3"
let "SERVICE_LINK_MESSAGE_PORT = BASE_PORT + 4"

# log everything
echo "REMOTE_KERNEL_HOST=$REMOTE_KERNEL_HOST" >> $LOGFILE
echo "REMOTE_KERNEL_PATH=$REMOTE_KERNEL_PATH" >> $LOGFILE
echo "REMOTE_KERNEL_USER=$REMOTE_KERNEL_USER" >> $LOGFILE
echo "REMOTE_KERNEL_PASSWORD=$REMOTE_KERNEL_PASSWORD" >> $LOGFILE
echo "REMOTE_KERNEL_PORT=$REMOTE_KERNEL_PORT" >> $LOGFILE
echo "MAIN_LINK_DATA_PORT=$MAIN_LINK_DATA_PORT" >> $LOGFILE
echo "MAIN_LINK_MESSAGE_PORT=$MAIN_LINK_MESSAGE_PORT" >> $LOGFILE
echo "PREEMPTIVE_LINK_DATA_PORT=$PREEMPTIVE_LINK_DATA_PORT" >> $LOGFILE
echo "PREEMPTIVE_LINK_MESSAGE_PORT=$PREEMPTIVE_LINK_MESSAGE_PORT" >> $LOGFILE
echo "SERVICE_LINK_DATA_PORT=$SERVICE_LINK_DATA_PORT" >> $LOGFILE
echo "SERVICE_LINK_MESSAGE_PORT=$SERVICE_LINK_MESSAGE_PORT" >> $LOGFILE
echo "SSH_OPTS=$SSH_OPTS" >> $LOGFILE

/usr/bin/ssh \
 $SSH_OPTS \
 -R 127.0.0.1:$MAIN_LINK_DATA_PORT:127.0.0.1:$MAIN_LINK_DATA_PORT \
 -R 127.0.0.1:$MAIN_LINK_MESSAGE_PORT:127.0.0.1:$MAIN_LINK_MESSAGE_PORT \
 -L $PREEMPTIVE_LINK_DATA_PORT:127.0.0.1:$PREEMPTIVE_LINK_DATA_PORT \
 -L $PREEMPTIVE_LINK_MESSAGE_PORT:127.0.0.1:$PREEMPTIVE_LINK_MESSAGE_PORT \
 -L $SERVICE_LINK_DATA_PORT:127.0.0.1:$SERVICE_LINK_DATA_PORT \
 -L $SERVICE_LINK_MESSAGE_PORT:127.0.0.1:$SERVICE_LINK_MESSAGE_PORT \
 $REMOTE_KERNEL_HOST \
 $REMOTE_KERNEL_PATH -mathlink -LinkMode Connect -LinkProtocol TCPIP -LinkName $LINK_NAME \
 >> $LOGFILE 2>&1
