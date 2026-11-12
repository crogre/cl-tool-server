CC ?= cc
CFLAGS ?= -O2 -Wall -Wextra

bin/toolc: client/toolc.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f bin/toolc

.PHONY: clean
