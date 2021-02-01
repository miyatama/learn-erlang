.SUFFIXES: .erl .beam .yml

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = indexer \
	indexer_porter \
	indexer_server \
	indexer_filenames_dets \
	indexer_checkpoint \
	indexer_trigrams \
	indexer_misc \
	indexer_words \
	indexer_dir_crawler

all: compile

compile: ${MODS:%=%.beam} trigramsS.tab
	@echo "make clean - clean up"

trigramsS.tab: ../354984si.ngl.gz indexer_trigrams.beam
	@erl -noshell -boot start_clean -s indexer_trigrams make_tables\
		-s init stop
clean:
	rm *.beam erl_crash.dump
	rm -fr trigramsS.tab
