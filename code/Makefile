.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = area_server_final \
	area_server0 \
	area_server1 \
	attrs \
	clock \
	edemo1 \
	extract \
	fac \
	geometory \
	hello \
	lib_find \
	lib_misc \
	m1 \
	mod_name_server \
	mp3_sync \
	mp3_v1 \
	processes \
	shop \
	shop1 \
	stimer \
	try_test \
     	scavenge_urls \
     	socket_examples \
     	udp_test \
	mp3_manager \
	lib_files_find \
	broadcast \
	shout \
	ets_test \
	lib_trigrams \
	lib_filenames_dets \
	event_handler \
	server1 \
	name_server \
	server2 \
	server3 \
	name_server1 \
	new_name_server \
	server4 \
	server5 \
	my_fac_server \
	my_bank \
	test_mnesia \
	motor_controller \
	my_alarm_handler \
	prime_server \
	area_server \
	sellaprime_supervisor \
	lib_primes \
	lib_lin \
	sellaprime_app \
	ptests \
	phofs \
	test_mapreduce

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:
	cd ports; make
	cd socket_dist; make
	cd index; make

clean:
	rm -fr *.beam erl_crash.dump
	cd ports; make clean
	cd socket_dist; make clean
	cd index; make clean
