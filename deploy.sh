scala-cli --power package modly.scala -f -o modly --assembly
chmod +x modly
scp modly bjornyx:~/servers/modly/.
ssh bjornyx "set -e; screen -S modly -X quit || true; screen -S modly -dm /home/bjornr/servers/modly/modly"