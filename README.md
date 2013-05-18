synchronizer
============

file synchronizer by ssh

## install as windows service
erlsrv add synchronizer -w "C:\app\synchronizer" -c "file synchronizer by ssh" -ar "-pa C:\app\synchronizer\ebin -boot start_sasl -config c:/app/synchronizer/local -eval \"synchronizer:start()\""
