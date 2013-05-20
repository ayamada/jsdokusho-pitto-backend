# NB: test/repl実行時とhttpd実行時では微妙にcwdが違う、要注意！


SRC_FILE := ./st.scm
DST_FILE := htdocs/st

test:
	rlgosh -E 'load "$(SRC_FILE)"' -E 'use gauche.test' -E "test-start 'st" -E "test-module 'st" -E 'test-end :exit-on-failure #t' -Eexit

repl:
	rlgosh -E 'load "$(SRC_FILE)"' -E 'select-module st' -i


deploy:
	cp -a $(SRC_FILE) $(DST_FILE)


backup-data:
	rm -f data.tgz
	tar cvfz data.tgz /var/nekoie/ams/data

backup-data-clean:
	rm -f data.tgz

backup-src:
	./dbxapi-st sync . dropbox:/superthon/yamada/from_server -d

#backup-all: backup-data backup-src backup-data-clean
backup-all: backup-data backup-src


# TODO: st-boot.sh 内にて、daemonizeが必要！nohupでできるか？
transfer-src-only:
	ssh yamada@999.999.999.999 sh st-kill.sh
	ssh yamada@999.999.999.999 bin/drop st_work/ams
	scp -r /var/www/ams \
	  yamada@999.999.999.999:/home/yamada/st_work/
	@echo !!! ssh yamada@999.999.999.999
	@echo !!! sh st-boot.sh

transfer-all:
	ssh yamada@999.999.999.999 sh st-kill.sh
	ssh yamada@999.999.999.999 sh st-drop.sh
	scp -r /var/www/ams \
	  /var/nekoie/ams/data \
	  /var/nekoie/ams/img_st \
	  yamada@999.999.999.999:/home/yamada/st_work/
	@echo !!! ssh yamada@999.999.999.999
	@echo !!! sh st-boot.sh


        
# vim:set ft=make:

