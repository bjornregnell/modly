scala-cli --power package modly.scala -f -o tmp/modly.jar --assembly
chmod +x tmp/modly.jar
scp tmp/modly.jar bjornyx:~/servers/modly/.
ssh bjornyx "set -e; screen -S modly -X quit || true; screen -S modly -dm /home/bjornr/servers/modly/modly.jar"