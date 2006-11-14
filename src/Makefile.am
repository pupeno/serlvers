erlsrcdir = $(ERLANG_INSTALL_LIB_DIR_serlvers)/src
erlsrc_DATA = dns_msg.erl gen_chargen.erl gen_daytime.erl gen_dns.erl gen_echo.erl gen_time.erl launcher.erl
EXTRA_DIST = $(erlsrc_DATA)

erlbeamdir = $(ERLANG_INSTALL_LIB_DIR_serlvers)/ebin
erlbeam_DATA = dns_msg.beam gen_chargen.beam gen_daytime.beam gen_dns.beam gen_echo.beam gen_time.beam launcher.beam
CLEANFILES = $(erlbeam_DATA)

SUFFIXES = .erl .beam
.erl.beam:
	$(ERLC) $(ERLCFLAGS) -b beam $<
