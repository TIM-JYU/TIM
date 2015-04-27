#!/bin/bash
cd /opt/cs
RUNPATH=/tmp/uhome/run
USERROOT=/tmp/uhome
mkdir $USERROOT 
mkdir $RUNPATH
sudo chmod 777 $RUNPATH
rm -f $RUNPATH/*

inotifywait -m $RUNPATH -e create  |
    while read path action file; do
        # echo "The file '$file' appeared in directory '$path' via '$action'"
        NAME=$file
        # read the file and start docker from directory written to file
        data=`cat $RUNPATH/$NAME`
        IFS=" " && arr=($data) 
        USERPATH="${arr[0]}"  # take path and command from data
        CMDNAME="${arr[1]}"
        echo "`date +%Y%m%d-%H%M%S` $USERPATH run: $CMDNAME"
        #if [[ $USERPATH =~ (|user/)(cs|[0-9a-f]+) ] -a [ $CMDNAME =~ (|run/)[-0-9a-f]+\.sh ]]; then
        if ! [[ $CMDNAME =~ ^(|run/)([-0-9a-f]+)\.sh$ ]]; then
            echo "CMD incorrect format"
        elif ! [[ $USERPATH =~ ^(|tmp/|user/)(cs|[-0-9a-f]+)$ ]]; then
            echo "PATH is incorrect"
        else
            USERPATH="$USERROOT/$USERPATH"

            # Ajetaan docker yhden cs-ajon ajaksi ja liitetään siihen käyttäjän hakemisto hakemistoksi /home/me
            ( #docker run -t -i -v /opt/cs:/cs/:ro  -v $USERPATH:/home/agent/ -w /home/agent cs3 /cs/rcmd.sh $CMDNAME 
              ./docker-run-timeout.sh 10s -v /opt/cs:/cs/:ro  -v $USERPATH:/home/agent/ -w /home/agent cs3 /cs/rcmd.sh $CMDNAME 
              #DOCKERPID=$!
              #wait $DOCKERPID && rm "$RUNPATH/$NAME"
              # rm "$RUNPATH/$NAME"  
            ) & 
        fi
        # echo "Now remove $RUNPATH/$NAME"
        rm "$RUNPATH/$NAME"
    done
    