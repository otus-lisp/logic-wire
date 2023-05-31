%.png : %.xpm
	S=10; \
	w=`identify $^ |cut -d' ' -f 3 |cut -d'x' -f1`; \
	h=`identify $^ |cut -d' ' -f 3 |cut -d'x' -f2`; \
	W=`expr $${w} \* $${S} + 1`; \
	H=`expr $${h} \* $${S} + 1`; \
	rows=`echo $$(seq 0 $${S} $${H}) |sed "s/\([0123456789]\+\)/M 0,\1 L $${W},\1/g"`; \
	cols=`echo $$(seq 0 $${S} $${W}) |sed "s/\([0123456789]\+\)/M \1,0 L \1,$${H}/g"`; \
	convert $^ -scale `expr $${S}`00% \
	   -gravity northwest -background snow4 -extent $${W}x$${H} \
	   -stroke snow4 -strokewidth 1 \
	   -draw "stroke-opacity 1 path '$${rows}'" \
	   -draw "stroke-opacity 1 path '$${cols}'" \
	   $@

all: $(patsubst %.xpm,%.png,$(wildcard media/*.xpm)) ol.wasm

define COMPILE_OL_CODE
	(import (src main)
		(lang embed)
		(otus fasl))

	(bytestream->port (fasl-encode (make-entry
	(lambda (args)
		; just a vector of pinned functions
		(halt [
			; xpm3
			(vm:pin load-xpm3)
			(vm:pin xpm3-width)
			(vm:pin xpm3-height)
			; board
			(vm:pin xpm3->board)
			(vm:pin board-texture)
			; layout
			(vm:pin load-layout)
		])))) stdout)
endef
export COMPILE_OL_CODE
src/main.inc: src/main.scm
	echo "$$COMPILE_OL_CODE" |ol - |xxd --include - >src/main.inc

ol.wasm: src/olvm.c src/main.c src/main.inc
	emcc src/main.c \
	     src/olvm.c -DOLVM_NOMAIN \
	     -O3 -o ol.html \
	         -Isrc/includes \
	   -DHAS_DLOPEN=0 -DHAS_SOCKETS=0 \
	   -s ASSERTIONS=0 \
	   -s ALLOW_MEMORY_GROWTH=1 \
	   -s FORCE_FILESYSTEM=0 \
	   -s EXPORTED_FUNCTIONS=_malloc,getValue \
	   -s WASM=1 && \
	# fix bugs in emscripten code \
	sed -i -r -e 's/(if\(result===undefined&&bytesRead===0\)\{)(throw)/\1bytesRead=-1;\2/g' \
	          -e 's/(Input: "\);if\(result!==null)/\1\&\&result!==undefined/' \
	          -e 's/(if\(!result\)\{return )null/\1result/' \
	    ol.js
