bids:
	datalad get -J 8 input/bids/*/*/func/*events*.tsv
	
.PHONY: rest
rest:
	datalad get -J 8 input/decoding/sub-*/decoding/*scheme-7*_time_shift-4*decoding*
	datalad get -J 8 input/decoding/sub-*/decoding/*scheme-9*_time_shift-4*decoding*
