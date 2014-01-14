###
###   This Makefile will not build anything unless a
###   specific target is supplied.  To discover all
###   top-level targets, please supply a question-mark as a
###   target, and a list of targets will be revealed. Each
###   of these targets can be examined by appending a
###   question-mark.
###

MAKE_GOAL ?= ?

.DEFAULT_GOAL := $(MAKE_GOAL)

Repo_URL := file:///home/phm/Documents/Git+Makefile/Remote-Repositories

###
###   Deduce the default $ERL and $ERLC in the $PATH, or
###   use the ones declared in the environment.
###
ifndef ERL
ERL ?= $(shell which erl)
ERL := $(realpath $(ERL))
endif

ifndef ERLC
ERLC ?= $(shell which erlc)
ERLC := $(realpath $(ERLC))
endif

empty:=
comma:=,
space:=$(empty) $(empty)
bullet:=$(space)$(space)*$(space)

Compilation_Flags ?= \
-D'$$Erlang: $(notdir $(wildcard $(ERLC:%/bin/erlc=%/erts-*))) $$'
Include_Dirs = $(wildcard lib/*/include)

###
###   Helper functions for the implicit rules
###

pattern = $(*)
module = $(notdir $(pattern))
outdir = $(dir $(pattern))
appvsn = $(subst -, ,$(firstword $(subst /, ,$(outdir))))
app = $(firstword $(appvsn))
vsn = $(lastword $(appvsn))
erlfile = $(outdir:%/ebin/=lib/%/src/$(module:%=%.erl))
asn1file = $(outdir:%/asn1/=lib/%/asn1/$(module:%=%.asn1))
asn1erlfile = $(outdir:%/ebin/=asn1/%/asn1/$(module:%=%.erl))

summary = $(warning Building $@ from:)$(foreach in,$^,$(info $(bullet)'$(in)'))

###
###   The resulting `.beam` files are precious, and are not erased.
###

.PRECIOUS: xform/%.beam

.PRECIOUS: lib/%.beam

.PRECIOUS: asn1/%.erl

.PRECIOUS: lib/%.app

.PHONY: clean
clean: clean_files = $(foreach in,tar.gz boot script,releases/*/*.$(in))
clean:
	rm -rf lib/*/ebin xform $(clean_files)

###
###   Add missing submodules from repositories, unless the
###   submodule is already part of the project, in which
###   case the submodules are updated.
###

repo_app_vsn = $(firstword $(subst /src/, ,$(*)))
repo_name = $(firstword $(subst -, ,$(repo_app_vsn)))
repo_branch = $(lastword $(subst -, ,$(repo_app_vsn)))

.PRECIOUS: lib/%/src
lib/%/src:
	$(warning Pulling branch $(repo_branch) from $(repo_name))
	if [ -d "$(*:%=lib/%)" ] ; \
	then git submodule update --init $(*:%=lib/%) ; \
	else git submodule add --branch $(repo_branch:%=version/%) \
	     $(Repo_URL:%=%/$(repo_name)) $(*:%=lib/%) ; \
	fi

.PRECIOUS: lib/%.app.src
.SECONDEXPANSION:
lib/%.app.src: $$(dir lib/%) ;


###
###   Compile ASN.1 source into `.erl` files, then into `.beam` files.
###

.SECONDEXPANSION:
asn1/%.erl: $$(asn1file)
	@mkdir -p $(dir $@)
	$(ERLC) \
	$(Compilation_Flags) \
	-DVSN='"$(lastword $(appvsn))"' \
	-I lib \
	-o $(dir $@) \
	$<


.SECONDEXPANSION:
lib/%.beam: $$(asn1erlfile)
	@mkdir -p $(dir $@)
	$(ERLC) \
	$(Compilation_Flags) \
	-DVSN='"$(lastword $(appvsn))"' \
	-I lib \
	-o $(dir $@) \
	$<


app_depend = $(subst /ebin/,/src/,$(*:%=lib/%.app.src))

.SECONDEXPANSION:
lib/%.app: $$(app_depend)
	$(summary)
	@mkdir -p $(dir $@)
	sed 's/%VSN%/$(lastword $(appvsn))/' < "$^" > "$@"

###
###   Extract the `parse_transform` dependecies from the
###   `.erl` source files.
###
###   TODO: the `proper.hrl` file also contains a
###   `parse_transform` compiler option.  The `*.hrl` files
###   must be also examined for dependencies.
###

transform_regexp := 's/^-compile({parse_transform,[ ]*\([^}]*\)}).*$$/\1/p'
transform_module = $(shell sed -n $(transform_regexp) < $(erlfile))
transform_source = $(wildcard $(transform_module:%=lib/*-*/src/%.erl))
transform_depend = $(subst /src/,/ebin/,$(transform_source:lib/%.erl=xform/%.beam))
xform_dir = $(dir $(transform_depend))

###
###   Extract the `-include_lib` dependencies from the
###   `.erl` source files.  The wildcard excludes the OTP
###   includes.  Any missing `.hrl` files will be picked up
###   by the `erlc` compiler.
###

include_lib_regexp := 's/^-include_lib("\([^"]*\).*$$/\1/p'
include_lib = $(sort $(shell sed -n $(include_lib_regexp) < $(erlfile)))
include_app = $(firstword $(subst /ebin/, ,$(*)))
include_hrl = $(lastword $(subst /include/, ,$(in)))

include_lib_dep = $(foreach in,$(include_lib),\
$(wildcard $(include_hrl:%=lib/*/include/%)))



.SECONDEXPANSION:
xform/%.beam: $$(erlfile) $$(include_lib_dep) $$(include_lib_dep)
	@mkdir -p $(dir $@)
	$(ERLC) \
	$(Compilation_Flags) \
	-DVSN='"$(lastword $(appvsn))"' \
	$(Include_Dirs:%/include=-pa %) \
	-o $(dir $@) \
	$<
	mkdir -p $(dir $(@:xform/%=lib/%))
	ln $@ $(@:xform/%=lib/%)


.SECONDEXPANSION:
lib/%.beam: $$(erlfile) $$(include_lib_dep) $$(transform_depend)
	@mkdir -p $(dir $@)
	$(ERLC) \
	$(Compilation_Flags) \
	-DVSN='"$(lastword $(appvsn))"' \
	$(Include_Dirs:%/include=-pa %) \
	$(Include_Dirs:%/include=-pa %/ebin) \
	$(xform_dir:%=-pa %) \
	-o $(dir $@) \
	$<

###
###   Short targets of the form `$(BEAM).beam`
###

module_erl_file = $(wildcard lib/*-*/src/$(*).erl)
module_beam_file = $(subst /src/,/ebin/,$(module_erl_file:%.erl=%.beam))

module_depend = $(erlfile) $(include_lib_dep)

%.beam?:
	$(info $*.beam depends on:)
	$(foreach value,$(module_erl_file),\
	  $(info $(bullet)'$(value)' ) )
	$(foreach pattern,$(module_beam_file:lib/%.beam=%),\
	  $(foreach value,$(include_lib_dep), \
	    $(info $(bullet)'$(value)' ) ) )
	$(foreach pattern,$(module_beam_file:lib/%.beam=%),\
	  $(foreach value,$(transform_depend), \
	    $(info $(bullet)'$(value)' ) ) )

.SECONDEXPANSION:
%.beam: $$(module_beam_file) ;

###
###   Short targets of the form `$(APP)-$(VSN).app` The
###   targets can be suffixed with a `?` to query the
###   dependecies.
###

name_vsn = $(subst -, ,$(*))
name = $(firstword $(name_vsn))
vsn = $(lastword $(name_vsn))
has_vsn = $(filter-out $(name),$(vsn))
app_source = $(if $(has_vsn),\
  $(*:%=lib/%/src/$(name:%=%.app.src)),\
  $(wildcard $(name:%=lib/%-*/src/$(name:%=%.app.src))) )
app_target = $(*:%=lib/%/ebin/$(name:%=%.app))

app_modules_regexp := 's/^.*{modules,\[\([^]]*\).*$$/\1/p'
app_extract = $(shell (tr -d ' \n' < $(in) && echo) | sed -n $(app_modules_regexp) )
app_module_names = $(subst $(comma), ,$(app_extract))
app_src_dir = $(dir $(in))
app_ebin_dir = $(app_src_dir:%/src/=%/ebin/)
app_module_beams = $(app_module_names:%=$(app_ebin_dir)%.beam)
app_beams = $(foreach in,$(app_source),$(app_module_beams))
app_target = $(*:%=lib/%/ebin/$(name:%=%.app))

%.app?:
	$(info $*.app depends on:)
	$(foreach x,$(app_target) $(app_beams),\
	  $(info $(bullet)'$(x)' ))

###
###   Building a short-name application goal using
###   recursive make.  TODO: Not sure yet how well this
###   behaves with make --jobs option.
###

.SECONDEXPANSION:
%.app: $$(app_target) $$(app_beams) ; $(summary)

###
###   Short targets of the form `$(REL)-$(VSN).rel` The
###   targets can be suffixed with a `?` to query the
###   dependecies.
###

rel_vsn_regexp := '/erts/,$$s/^[^%]*{\([^,"]*\)[^"]*"\([^"]*\).*}.*$$/\1-\2/p'

###
###   Note: the value of $(use_rel_file) is set according
###   to the goal specified in the make command.  If the
###   goal specifies, either explicitely or implicitely,
###   one of the release (*.rel) files, then the compiler
###   matching the version of 'erts' (Erlang Run-Time
###   System) is used.  If the specific version of erts
###   cannot be found the build fails, as the release
###   dependecies cannot be honoured.
###

given_target = $(notdir $(MAKECMDGOALS))
given_rel = $(filter %.rel,$(given_target))
given_boot = $(filter %.boot,$(given_target))
given_tar_gz = $(filter %.tar.gz,$(given_target))
given_release = $(strip \
  $(given_rel:%.rel=%) \
  $(given_boot:%.boot=%) \
  $(given_tar_gz:%.tar.gz=%) )
given_rel_file = $(foreach in,$(given_release),releases/$(in)/$(in).rel)

use_rel_file = $(given_rel_file)

use_rel_apps = $(foreach in,$(use_rel_file),\
$(shell sed -n $(rel_vsn_regexp) < $(in)))
use_erts_vsn = $(filter erts-%,$(use_rel_apps))

use_root_dir = $(dir $(ERL:%/lib/erlang/bin/erl=%))
use_directory = $(dir $(wildcard $(use_root_dir:%=%*/lib/erlang/$(use_erts_vsn))))
use_otp_apps = $(if $(use_directory),\
$(notdir $(wildcard $(use_directory:%=%lib/*))),\
$(error No suitable version of erlc))
use_compiler = $(use_directory:%=%bin/erlc)
use_runtime = $(use_directory:%=%bin/erl)

use_erlc = $(sort $(use_compiler))

use_apps = $(filter-out $(use_otp_apps),$(use_rel_apps))
use_include = $(wildcard $(use_apps:%=lib/%/include))

use_targets = $(use_apps:%=%.app)

%.rel?: use_rel_file = $(*:%=releases/$(*)/%.rel)
%.rel?:
	$(info $*.rel depends on release applications:)
	$(foreach value,$(use_targets),\
	  $(info $(bullet)'$(value)' ))
	$(info $*.rel depends on OTP library applications:)
	$(foreach in,$(filter $(use_otp_apps),$(use_rel_apps)),\
	  $(info $(bullet)'$(in)' ))

###
###   Short build target "$REL-$VSN.rel"
###

%.rel!: Include_Dirs = $(use_include)
%.rel!: ERLC = $(use_erlc)
%.rel!: $$(use_targets) ;
	$(summary)

###
###   Short build target, uses recursive make, the ERLC
###   value is exported correctly, but the Include_Dirs
###   does not work properly. BUGBUG
###

%.rel: export Include_Dirs = $(use_include)
%.rel: export ERLC = $(use_erlc)
%.rel:
	$(summary)
	$(MAKE) $(use_targets)

###
###   Build boot file
###

.SECONDEXPANSION:
releases/%.boot: boot_includes = $(use_apps:%=lib/%/ebin)
releases/%.boot: ERLC = $(use_erlc)
releases/%.boot: $$(use_targets)
	$(info Building $@ from:)
	$(foreach value,$(use_rel_file) $(boot_includes),\
	  $(info $(bullet)'$(value)' ))
	$(use_compiler) -b boot \
	+"{variables,[{\"ROOT\",\"$(realpath .)\"}]}" \
	$(boot_includes:%=-I %) \
	-o $@ $(use_rel_file)

###
###   Build release tar file
###



.SECONDEXPANSION:
releases/%.tar.gz: use_boot_file = $(*:%=releases/%.boot)
releases/%.tar.gz: ERLC = $(use_erlc)
releases/%.tar.gz: $$(use_boot_file)
	$(info Building $@ from:)
	$(foreach value,$(use_rel_file) $(use_apps),\
	  $(info $(bullet)'$(value)' ))
	$(use_runtime) \
	  -noshell \
	  -eval "{ok,[[Root]]}=init:get_argument(root),\
		 io:format(\"erlang env Root=~p~n\",[Root]),\
		 c:cd(\"$(dir $(use_rel_file))\"),\
		 {ok,systools_make,_}\
		   = systools:make_tar(\"$(notdir $*)\",\
			[{path,[\"../../lib/*/ebin\"]},{erts,Root}, silent]),\
		 init:stop()."

###
###   Debugging tool to reveal the value of a variable when
###   given that variable name with the `?` suffix, e.g. ERL?
###

%?:
	$(info $(*) )
	$(foreach value,$($(*)), \
	  $(info $(bullet)'$(value)' ) )


?:
	$(info "make" will build the following targets:)
	$(foreach in,$(wildcard releases/*-*/*-*.rel),\
          $(info $(bullet) $(notdir $(in))))


