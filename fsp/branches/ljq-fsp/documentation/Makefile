# Flo				-- Hikitech
# YaMakefile

# $@ is the name of the file to be made.
# $? is the names of the changed dependents. 
# $< the name of the related file that caused the action.
# $* the prefix shared by target and dependent files. 


IT = main

RERUN = "(There were undefined references|Rerun to get (cross-references|the bars) right)"
RERUNBIB = "No file.*\.bbl|Citation.*undefined"

dvi: 
	latex $(IT)
	egrep -c $(RERUNBIB) $(IT).log && (bibtex $(IT) ; latex $(IT)) ; true
	egrep -c $(RERUN) $(IT).log && latex $(IT) ; true
	egrep -c $(RERUN) $(IT).log && latex $(IT) ; true
	# Display relevant warnings
	egrep -i "(Reference|Citation).*undefined" $(IT).log ; true

ps4: $(IT).dvi
	dvips -t a4 -o $(IT).ps $<

psl: $(IT).dvi
	dvips -t letter -o $(IT).ps $<

# propre et sec
clean:
	rm *.aux *.bbl *.blg *.log *.dvi *.ps

veryclean: clean
	rm *~

# ?
help:
	@echo Potential targets: [dvi, ps4, psl]
