PYLEX=pylex
PYPARSE=pyparse
PYTRANS=pytrans
PYDESUGAR_S=pydesugar
PYDESUGAR=./pydesugar
PYCPS_S=pycps
PYCPS=./pycps

PYTHON=python3
RACKET=racket

all: pycps pydesugar 

desugars:
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR_S)

runs:
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR_S) | $(PYCPS_S)

desugar: pydesugar
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR)

cpss:
	@$(PYCPS_S)

cps: pycps
	@ $(PYCPS)

run: pydesugar pycps 
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS) | $(PYDESUGAR) | $(PYCPS)

pytrans:
	@ $(PYLEX) | $(PYPARSE) | $(PYTRANS)

pycps: pycps.rkt
	raco exe pycps.rkt

pydesugar: pydesugar.rkt
	raco exe pydesugar.rkt


tests/%.lxd: tests/%.py
	$(PYLEX) < $< > $@

tests/%.ast: tests/%.lxd
	$(PYPARSE) < $< > $@

tests/%.hir: tests/%.ast
	$(PYTRANS) < $< > $@

tests/%.lir: tests/%.hir pydesugar
	$(PYDESUGAR) < $< > $@

tests/%.cps: tests/%.lir pycps
	$(PYCPS) < $< > $@

tests/%.lrkt: tests/%.lir lir-header.rkt
	cat lir-header.rkt $< > $@

tests/%.crkt: tests/%.cps cps-header.rkt
	cat cps-header.rkt $< > $@

tests/%.expected: tests/%.py
	$(PYTHON) $< > $@


tests/%.lresult: tests/%.lrkt
	$(RACKET) $< > $@

tests/%.cresult: tests/%.crkt
	$(RACKET) $< > $@


tests/%.ldiff: tests/%.lresult tests/%.expected
	@echo "diffing " $(basename $@)
	diff $(basename $@).expected $(basename $@).lresult >> $@


tests/%.cdiff: tests/%.cresult tests/%.expected
	@echo "diffing " $(basename $@)
	diff $(basename $@).expected $(basename $@).cresult >> $@



cps-tests: tests/0.cps tests/1.cps tests/2.cps tests/3.cps tests/4.cps tests/5.cps tests/6.cps tests/7.cps tests/8.cps tests/9.cps tests/10.cps tests/11.cps tests/12.cps tests/13.cps tests/14.cps tests/15.cps tests/16.cps tests/17.cps tests/18.cps tests/19.cps tests/20.cps tests/21.cps tests/23.cps tests/24.cps tests/26.cps tests/27.cps tests/28.cps tests/29.cps tests/30.cps tests/31.cps tests/32.cps tests/33.cps tests/34.cps tests/35.cps tests/36.cps tests/37.cps tests/38.cps tests/39.cps tests/40.cps tests/41.cps tests/42.cps tests/43.cps tests/45.cps tests/46.cps tests/47.cps tests/48.cps tests/49.cps tests/50.cps tests/51.cps tests/52.cps tests/53.cps tests/54.cps tests/55.cps tests/56.cps tests/57.cps tests/58.cps tests/59.cps tests/60.cps tests/61.cps tests/62.cps tests/63.cps tests/64.cps tests/65.cps tests/66.cps tests/67.cps tests/69.cps tests/70.cps tests/71.cps tests/72.cps tests/73.cps

lir-tests: tests/0.lir tests/1.lir tests/2.lir tests/3.lir tests/4.lir tests/5.lir tests/6.lir tests/7.lir tests/8.lir tests/9.lir tests/10.lir tests/11.lir tests/12.lir tests/13.lir tests/14.lir tests/15.lir tests/16.lir tests/17.lir tests/18.lir tests/19.lir tests/20.lir tests/21.lir tests/23.lir tests/24.lir tests/26.lir tests/27.lir tests/28.lir tests/29.lir tests/30.lir tests/31.lir tests/32.lir tests/33.lir tests/34.lir tests/35.lir tests/36.lir tests/37.lir tests/38.lir tests/39.lir tests/40.lir tests/41.lir tests/42.lir tests/43.lir tests/45.lir tests/46.lir tests/47.lir tests/48.lir tests/49.lir tests/50.lir tests/51.lir tests/52.lir tests/53.lir tests/54.lir tests/55.lir tests/56.lir tests/57.lir tests/58.lir tests/59.lir tests/60.lir tests/61.lir tests/62.lir tests/63.lir tests/64.lir tests/65.lir tests/66.lir tests/67.lir tests/69.lir tests/70.lir tests/71.lir tests/72.lir tests/73.lir

test-cps: tests/0.cdiff tests/1.cdiff tests/2.cdiff tests/3.cdiff tests/4.cdiff tests/5.cdiff tests/6.cdiff tests/7.cdiff tests/8.cdiff tests/9.cdiff tests/10.cdiff tests/11.cdiff tests/12.cdiff tests/13.cdiff tests/14.cdiff tests/15.cdiff tests/16.cdiff tests/17.cdiff tests/18.cdiff tests/19.cdiff tests/20.cdiff tests/21.cdiff tests/23.cdiff tests/24.cdiff tests/26.cdiff tests/27.cdiff tests/28.cdiff tests/29.cdiff tests/30.cdiff tests/31.cdiff tests/32.cdiff tests/33.cdiff tests/34.cdiff tests/35.cdiff tests/36.cdiff tests/37.cdiff tests/38.cdiff tests/39.cdiff tests/40.cdiff tests/41.cdiff tests/42.cdiff tests/43.cdiff tests/45.cdiff tests/46.cdiff tests/47.cdiff tests/48.cdiff tests/49.cdiff tests/50.cdiff tests/51.cdiff tests/52.cdiff tests/53.cdiff tests/54.cdiff tests/55.cdiff tests/56.cdiff tests/57.cdiff tests/58.cdiff tests/59.cdiff tests/60.cdiff tests/61.cdiff tests/62.cdiff tests/63.cdiff tests/64.cdiff tests/65.cdiff tests/66.cdiff tests/67.cdiff tests/69.cdiff tests/70.cdiff tests/71.cdiff tests/72.cdiff tests/73.cdiff

test-lir: tests/0.ldiff tests/1.ldiff tests/2.ldiff tests/3.ldiff tests/4.ldiff tests/5.ldiff tests/6.ldiff tests/7.ldiff tests/8.ldiff tests/9.ldiff tests/10.ldiff tests/11.ldiff tests/12.ldiff tests/13.ldiff tests/14.ldiff tests/15.ldiff tests/16.ldiff tests/17.ldiff tests/18.ldiff tests/19.ldiff tests/20.ldiff tests/21.ldiff tests/23.ldiff tests/24.ldiff tests/26.ldiff tests/27.ldiff tests/28.ldiff tests/29.ldiff tests/30.ldiff tests/31.ldiff tests/32.ldiff tests/33.ldiff tests/34.ldiff tests/35.ldiff tests/36.ldiff tests/37.ldiff tests/38.ldiff tests/39.ldiff tests/40.ldiff tests/41.ldiff tests/42.ldiff tests/43.ldiff tests/45.ldiff tests/46.ldiff tests/47.ldiff tests/48.ldiff tests/49.ldiff tests/50.ldiff tests/51.ldiff tests/52.ldiff tests/53.ldiff tests/54.ldiff tests/55.ldiff tests/56.ldiff tests/57.ldiff tests/58.ldiff tests/59.ldiff tests/60.ldiff tests/61.ldiff tests/62.ldiff tests/63.ldiff tests/64.ldiff tests/65.ldiff tests/66.ldiff tests/67.ldiff tests/69.ldiff tests/70.ldiff tests/71.ldiff tests/72.ldiff tests/73.ldiff

clean:
	rm -rf stub
	rm -rf compiled
	rm -f pydesugar pycps pycps_rkt.zo pydesugar_rkt.zo
	rm -f tests/*.{lxd,ast,lir,hir,crkt,lrkt,cdiff,ldiff,expected,cresult,lresult} stub.tar.gz
