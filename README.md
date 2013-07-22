synchronizer
============

file synchronizer by ssh

## install as windows service (need adminstrator privilege)
erlsrv remove synchronizer
erlsrv add synchronizer -w "C:\app\synchronizer" -c "file synchronizer by ssh" -ar "-pa C:\app\synchronizer\ebin -boot start_sasl -config c:/app/synchronizer/local_prod -eval \"synchronizer:start()\""
