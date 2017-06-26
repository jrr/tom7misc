
MS=/usr0/src/sml-lib/mlton/mysql/

# MC=mysql_config
MB=/usr1/mysql-standard-5.0.7-beta-linux-i686
MC=${MB}/bin/mysql_config

LO=`echo "-L${MB}/lib -lmysqlclient_r -lz -lpthread -lcrypt -lnsl -lm -lpthread -lc -lnss_files -lnss_dns -lresolv -lc -lnss_files -lnss_dns -lresolv" | sed -e 's/ / -link-opt /g'`

# -expert true -enable-ann 'allowPrim true' -default-ann 'allowPrim true'
#  -const 'Exn.keepHistory true'
periodbot : *.sml ${MS}mysql.o periodbot.mlb
	mlton -default-ann 'allowImport true' -link-opt ${LO} periodbot.mlb ${MS}mysql.o


${MS}mysql.o : ${MS}mysql.c
	gcc -o $@ -c -O `${MC} --cflags` `${MC} --include` $<