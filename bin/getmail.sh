#!/bin/sh

# On my desktop, I only want mail to be checked if I'm using the
# machine. Otherwise I want it to leave the mail on the server so my
# laptop can fetch it.

# idletime gets touched every three minutes while idle
xautolock -time 3 -locker "touch ~/.idle" &

# keep the mail on the server if i'm on my laptop
if [ `hostname` = "vannevar" ] ; then
    KEEP='-k'
fi

while [ 1 ]
do
    touch ~/.three.minutes.old
    sleep 3m

    if [ ~/.idle -ot ~/.three.minutes.old ]; then
	# not idle!
	fetchmail -k >/dev/null 2>&1

	if [ "$?" = "0" ]; then
	    notify-send -u low "New mail"
	fi
    fi
done

