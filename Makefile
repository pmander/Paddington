#
# NOTE: The use of wildcard and realpath depend on the paths actually
#       existing in the filesystem at the time of invocation. This precludes
#       these functions from use in deferred variable assignment, where the
#       paths are targets of rules that may not have fired yet.
#
# NOTE: This version of the Makefile implements the GNUmake remake feature,
#       which generates dependency files to describe which include files in
#       addition to the source file, are needed to compile the target.
#
#       http://make.paulandlesley.org/autodep.html
#       http://stackoverflow.com/questions/554742/reevaluate-makefile-variables
#
# NOTE: By setting the environment variable $MAKE_GOAL, the target can be
#       changed from the default goal 'all'. This trick enables the use of
#       the Unix 'script' with the $SHELL environment set to SHELL=make, and
#       with MAKE_GOAL=run it starts an Erlang shell with the -pa paths set.
#       All interaction and output is then logged using 'script'. E.g:
#
#       MAKE_GOAL=run SNAME=unit_test SHELL=$(which make) script -a "$LOG"
#
# NOTE: Support for -compile({parse_transform,$(MODULE_NAME)}).
#
#       The PropEr compilation is an example where an Erlang
#       `parse_transform` is used.  PropEr requires that the `vararg.beam`
#       module is compiled first of all, enabling all the other modules with
#       the above `-compile({parse_transform,vararg}).` attribute to be
#       compiled with the addition of the compile flag
#       `-pa lib/proper-$(VSN)/ebin`
#

LC_CTYPE = C
LANG = C
export LC_CTYPE
export LANG

MAKE_GOAL ?= all

.DEFAULT_GOAL := $(MAKE_GOAL)

comma:=,
empty:=
space:=$(empty) $(empty)

#
# NOTE: The environment_file contains exported variables, the most useful is
#       the ERL environment variable, which stores the path to the erl
#       executable.
#
hostname = $(shell hostname)
environment_file := $(wildcard $(hostname:%=environment/%.environment))
ifndef ERL
$(foreach file,\
          $(environment_file),\
          $(info Loading environment from $(file)))
-include $(environment_file)
endif

#
# NOTE: With either the given ERL environment, or the erl executable found
#       in the default PATH, deduce the location of the erlc compiler, and
#       obtain a list of OTP applications and their versions.
#
ifndef ERL
ERL ?= $(shell which erl)
ERL := $(realpath $(ERL))
endif
ERL_BIN_DIR := $(dir $(ERL))
ERLC := $(ERL:%=%c)
$(info ERL = $(ERL))

#
# NOTE: The ERLDIR environment variable is used by the make-package script.
#
erlang_dir := $(lastword $(subst /erlang/, ,$(ERL_BIN_DIR)))

ERLDIR := $(subst $(erlang_dir),,$(ERL_BIN_DIR))
export ERLDIR

OTP_SRC_DIRS \
  := $(realpath \
       $(wildcard \
         $(ERL_BIN_DIR:%/$(erlang_dir)=%/lib/*/src) ) )

otp_apps := $(notdir $(wildcard $(ERL_BIN_DIR:%/bin/=%/erts-*))) \
            $(notdir $(OTP_SRC_DIRS:%/src=%))
otp_name_vsn = $(subst -, ,$(otp))
otp_vsn = $(lastword $(otp_name_vsn))
otp_name = $(firstword $(otp_name_vsn))

$(foreach otp,$(otp_apps),$(eval $(otp_name)_otp_vsn := $(otp_vsn)) )

OTP_NAMES := $(sort $(foreach otp,$(otp_apps),$(otp_name)))

OTP_VERSIONS := \
  $(foreach otp_name,\
            $(OTP_NAMES),\
            $(otp_name:%=%-$($(otp_name)_otp_vsn)) )

otp_major_minor := $(strip $(subst .,$(space),$(erts_otp_vsn)))
major_minor = $(subst $(space),_,$(wordlist 1,2,$(otp_major_minor)))

map_5_5_to_release := R11B
map_5_6_to_release := R12B
map_5_7_to_release := R13B
map_5_8_to_release := R14B
map_5_9_to_release := R15B
map_5_10_to_release := R16B
map_6_0_to_release := R17B
map_6_1_to_release := R17B
map_6_2_to_release := R17B
map_7_1_to_release := R18B

otp_release := $($(major_minor:%=map_%_to_release))
ERLC_FLAGS ?=
Compilation_Flags := $(ERLC_FLAGS) -W -v +debug_info
Compilation_Flags += -DOTP_RELEASE_$(otp_release)
Compilation_Flags += -D"\$$Erlang: $(otp_release) (erts-$(erts_otp_vsn)) $$"


#
# NOTE: Create the name of the file which will contain the freshly compiled
#       beam files. This list of beams is loaded at your option into a
#       running Erlang runtime that shares the same -sname as the release.
#
date_now := $(shell date -u '+%Y-%m-%dT%H:%M:%SZ')
reload_list_file := tmp/reload-lists/reload-list-$(date_now)

#
# NOTE: Application and Release versions are stored in src/vsn.mk files.
#       This regexp extracts the version value from the file.
#
vsn_regexp := 's|VSN[^0-9]*\([0-9]*\)|\1|p'

include_regexp := 's/^-include("\([^"]*\).*$$/\1/p'

include_lib_regexp := 's/^-include_lib("\([^"]*\).*$$/\1/p'

parse_transform_regexp := 's/^-compile({parse_transform,[ ]*\([^}]*\)}).*$$/\1/p'

c_makefiles := $(wildcard lib/*/c_src/Makefile)

#
# NOTE: Recursive wildcard function to extract a list of files contained in
#       a given directory. This is a variation of an original implementation
#       found at:
#
#       http://www.mail-archive.com/help-make@gnu.org/msg05279.html
#       http://www.mailinglistarchive.com/help-make@gnu.org/msg00400.html
#
#       Re: use of wildcard function recursively
#       John Graham-Cumming
#
#       rwildcard = \
#         $(foreach d,\
#                   $(wildcard $1*),\
#                   $(call rwildcard,$d/)$(filter $(subst *,%,$2),$d))
#
#       This version uses deferred assignments, my attempt at making the
#       algorithm more legible than the original.
#
collect_dirs = $(filter %/,$(wildcard $(1:%/=%)/*/))
dirs_only = $(collect_dirs:%/=%)
files_only = $(filter-out %/,$(wildcard $(1:%/=%)/*/))

rwildcard = \
  $(foreach dir,\
            $(dirs_only),\
            $(call rwildcard,$(dir)) ) \
  $(files_only)

#
# NOTE: Compile a list of applications and their versions. There are two
#       lists, App_Names lists all the application names, and applications
#       lists the application names with a dash-version suffix.
#
#       Another set of variables, one for each application, are identified
#       by the application name with the suffix _has_vsn. These variable
#       store the application version, and enable mapping the application
#       name to the corresponding version.
#

a_app_vsn_mk = $(wildcard lib/$(app_name)/src/vsn.mk)
a_app_version = $(if $(a_app_vsn_mk),\
  $(shell sed -n $(vsn_regexp) < $(a_app_vsn_mk) ),\
  $(error The application $(app_name) is missing a vsn.mk file) )

a_app_src_files = $(wildcard lib/*/src/*.app.src)
App_Names := $(notdir $(a_app_src_files:%.app.src=%))

app_asn1_dir = $(app_name:%=lib/%/asn1/)
app_asn1_files = $(wildcard $(app_asn1_dir:%=%*.asn1))
app_asn1_modules = $(notdir $(app_asn1_files:%.asn1=%))

app_src_dir = $(app_name:%=lib/%/src/)
a_app_erl_files = $(wildcard $(app_src_dir:%=%*.erl))
a_app_modules = $(notdir $(a_app_erl_files:%.erl=%))

#
# NOTE: Map the application name to:
#        * the modules
#        * the version
#
$(foreach app_name,\
          $(App_Names),\
          $(eval $(app_name)_asn1 := $(app_asn1_modules)) \
          $(eval $(app_name)_modules := $(a_app_modules)) \
          $(eval $(app_name)_has_vsn := $(a_app_version)) )

app_modules = $($(app_name)_modules)
app_vsn = $($(app_name)_has_vsn)
app_asn1 = $($(app_name)_asn1)


app_ebin_dir = $(app_name:%=lib/%-$(app_vsn)/ebin/)
app_priv_src = $(app_name:%=lib/%/priv/)
app_priv_dir = $(app_name:%=lib/%-$(app_vsn)/priv/)
app_mibs_dir = $(app_name:%=$(app_priv_dir)mibs/)

a_mib_src_files = $(wildcard $(app_src_dir:%=%*.mib))
a_mib_bin_files = $(a_mib_src_files:$(app_src_dir)%.mib=$(app_mibs_dir)%.bin)
a_mibs = $(notdir $(a_mib_src_files:%.mib=%))

a_priv_source = $(filter-out %~,$(call rwildcard,$(app_priv_src)))
a_priv_files = $(a_priv_source:$(app_priv_src)%=%)

#
# NOTE: Search for modules that implement parse_transform functions, and
#       make the applications in which they are defined a primary
#       dependency, to force the build to finish these applications first,
#       and add them to the -pa paths of dependent applications.
#
xform_search = $(shell grep -l '^parse_transform' lib/*/src/*.erl )
xform_app_names = \
  $(sort $(foreach x,\
            $(xform_search:lib/%=%),\
            $(firstword $(subst /,$(space),$(x)))))

xform_erls = $(notdir $(xform_search))
xform_mods = $(xform_erls:%.erl=%)
xform_beams = $(foreach module,$(xform_mods),$($(module:%=%_module_beam)) )

xform_flags := $(foreach app_name,$(xform_app_names),$(app_ebin_dir:%/=-pa %))

Compilation_Flags += $(xform_flags)

$(info Compilation_Flags = $(Compilation_Flags) )

#
# NOTE: Map the application name to:
#        * the files in the priv directory
#        * the bin files in the priv/mibs directory
#
$(foreach app_name,\
          $(App_Names),\
          $(eval $(app_name)_mibs := $(a_mibs)) \
          $(eval $(app_name)_priv_files := $(a_priv_files)) \
          $(eval $(app_name)_mib_bin_files := $(a_mib_bin_files)) )


app_priv_files = $($(app_name)_priv_files:%=$(app_priv_dir)%)
app_mib_bin_files = $($(app_name)_mib_bin_files)
app_mibs = $($(app_name)_mibs)

app_asn1_beam_files = $(app_asn1:%=$(app_ebin_dir)%.beam)

app_beam_files = $(app_modules:%=$(app_ebin_dir)%.beam)
app_erl_files = $(app_modules:%=$(app_src_dir)%.erl)
app_file = $(app_name:%=$(app_ebin_dir)%.app)
app_src_file = $(app_name:%=$(app_src_dir)%.app.src)
app_c_src = $(app_name:%=%_c_src)

a_dependencies = $(sort \
  $(app_asn1_beam_files) \
  $(app_beam_files) \
  $(app_file) \
  $(app_mib_bin_files) \
  $(app_priv_files) )




$(foreach app_name,\
          $(App_Names),\
          $(eval $(app_name)_app_dependencies := $(a_dependencies)) \
          $(eval $(app_name)_src_file := $(app_src_file)) \
          $(eval $(app_name)_file := $(app_file)) )

app_dependencies = $($(app_name)_app_dependencies)



m_mod_erl = $(app_src_dir:%=%$(mod_name:%=%.erl))
m_mod_beam = $(app_ebin_dir:%=%$(mod_name:%=%.beam))
#
# NOTE: Map the module name to: 
#        * the application and version it is member of
#        * the path and file name of the erl source file
#        * the path and file name of the beam target file.
#
$(foreach app_name,\
          $(App_Names),\
          $(foreach mod_name,\
                    $(app_modules),\
                    $(eval $(mod_name)_app := $(app_name)) \
                    $(eval $(mod_name)_app_vsn := $(app_vsn)) \
                    $(eval $(mod_name)_module_erl := $(m_mod_erl)) \
                    $(eval $(mod_name)_module_beam := $(m_mod_beam)) ) )





#
# NOTE: If there is a ./lib/$(app_name)/c_src/Makefile, include it into the
#       build.
#
c_src = $(mk:%/Makefile=%)
c_app = $(notdir $(mk:%/c_src/Makefile=%))
c_app_vsn = $(c_app:%=%-$($(c_app)_has_vsn))
c_priv = $(c_app_vsn:%=./lib/%/priv)
c_priv_lib = $(c_app_vsn:%=./lib/%/priv/lib)
define include_makefile
-include $(mk)
endef

$(foreach mk,$(c_makefiles),$(eval $(include_makefile)))









#
# NOTE: Compile a list of releases and their versions. Similar to the
#       application stuff just above: rel_names lists all the release names,
#       and releases lists the release names with a dash-version suffix.
#
r_rel_vsn_mk = $(wildcard releases/$(rel_name)/src/vsn.mk)
r_rel_version = $(shell sed -n $(vsn_regexp) < $(r_rel_vsn_mk) )

r_rel_src_files := $(wildcard releases/*/src/vsn.mk)
Rel_Names := $(notdir $(r_rel_src_files:%/src/vsn.mk=%))







r_rel_src_dir = $(rel_name:%=releases/%/src/)
$(foreach rel_name,\
          $(Rel_Names),\
          $(eval $(rel_name)_rel_vsn := $(r_rel_version)) )

r_rel_src_file = $(rel_name:%=$(r_rel_src_dir)$(rel_name:%=%.rel.src))

release_regexp := 's/^[^%]*{\([^"][^,]*\).*}.*$$/\1/p'
r_rel_apps = $(shell sed -n $(release_regexp) < $(r_rel_src_file) | tr -d \' )

$(foreach rel_name,\
          $(Rel_Names),\
          $(eval $(rel_name)_rel_src_file := $(r_rel_src_file)) \
          $(eval $(rel_name)_rel_apps := $(r_rel_apps)) )

rel_apps = $($(rel_name)_rel_apps)
rel_non_OTP = $(filter $(App_Names),$(rel_apps))
rel_OTP = $(filter-out $(App_Names),$(rel_apps))


$(foreach rel_name,\
          $(Rel_Names),\
          $(eval $(rel_name)_rel_depend_apps := $(rel_non_OTP)) \
          $(eval $(rel_name)_rel_library_apps := $(rel_OTP)) )

rel_ebin_dirs = $(foreach app_name,\
                          $($(rel_name)_rel_depend_apps),\
                          $(app_ebin_dir) )

$(foreach rel_name,\
          $(Rel_Names),\
          $(eval $(rel_name)_rel_ebin_dirs := $(rel_ebin_dirs)) )



rel_vsn = $($(rel_name)_rel_vsn)
rel_name_vsn = $(rel_name:%=%-$(rel_vsn))
rel_target_dir = $(rel_name_vsn:%=releases/%/)
rel_tar_gzip = $(rel_name_vsn:%=$(rel_target_dir)%.tar.gz)

$(foreach rel_name,\
          $(Rel_Names),\
          $(eval $(rel_name)_rel_tar_gzip := $(rel_tar_gzip)) )

r_rel_boot = $(rel_target_dir:%=%$(rel_name_vsn).boot)
r_rel_file = $(rel_target_dir:%=%$(rel_name_vsn).rel)
r_rel_start = $(rel_target_dir:%=%start)
r_rel_config = $(rel_target_dir:%=%sys.config)

rel_depend_files = \
  $(r_rel_start) \
  $(r_rel_config)

$(foreach rel_name,\
          $(Rel_Names),\
          $(eval $(rel_name)_boot_file := $(r_rel_boot)) \
          $(eval $(rel_name)_rel_file := $(r_rel_file)) \
          $(eval $(rel_name)_rel_target_dir := $(rel_target_dir)) )

#
# NOTE: Map the application name to the release it appears in.
#
$(foreach rel_name,\
          $(Rel_Names),\
          $(foreach app_name,\
                    $($(rel_name)_rel_apps), \
                    $(eval $(app_name)_is_in_release += $(rel_name)) ) )






p_match = $*
p_name_app = $(notdir $(p_match))
p_ebin = $(dir $(p_match:%=lib/%))

p_name_vsn = $(p_ebin:lib/%/ebin/=%)
p_split = $(subst -, ,$(p_name_vsn))
p_name = $(firstword $(p_split))
p_vsn = $(lastword $(p_split))

p_app_src = $(p_name_app:%=lib/$(p_name)/src/$(p_name_app:%=%.app.src))
p_app_tgt = $(p_match:%=lib/%.app)
p_modules = $($(p_name)_modules)
p_ins_mod = $(subst $(space),$(comma),$(p_modules:%=\'%\'))

lib/%.app:
	@mkdir -p $(dir $@)
	sed -e \
	"s|%APP%|$(p_name)|g;s|%MODULES%|$(p_ins_mod)|g;s|%VSN%|$(p_vsn)|g" \
	< "$(p_app_src)" \
	> "$(p_app_tgt)"




mib_regexp := '/IMPORTS/,/;/{;s/^.*FROM \(.*\)$$/\1/p;}'
bin_file = $(mib_name:%=$(app_mibs_dir)%.bin)
mib_file = $(mib_name:%=$(app_src_dir)%.mib)
mib_depend = $(filter $(app_mibs),\
                      $(shell sed -n $(mib_regexp) < $(mib_file)) )

mib_dep_files = $(mib_depend:%=$(app_mibs_dir)%.bin)

#
# NOTE: Rule to compile MIB files into .bin files. The mib_dep_files are
#       obtained using sed and the mib_regexp above. Create one rule for
#       each MIB file.
#
define app_mib_rule
$(bin_file): $(mib_file) $(mib_dep_files)
	@mkdir -p $$(dir $$@)
	$$(ERLC) -I $$(dir $$@) -o $$(dir $$@) $$<
endef
$(foreach app_name,\
          $(App_Names),\
          $(foreach mib_name,\
                    $(app_mibs),\
                    $(eval $(app_mib_rule)) ) )



dep_dir := tmp/dependencies/
dep_erl_suffix := .erl.d
dep_app_suffix := .app.d
dep_rel_suffix := .rel.d
dep_pack_suffix := .pack.d

pack_dep_file = $(rel_name:%=$(dep_dir)%$(dep_pack_suffix))
rel_dep_file = $(rel_name:%=$(dep_dir)%$(dep_rel_suffix))
erl_dep_file = $(mod_name:%=$(dep_dir)%$(dep_erl_suffix))
app_dep_file = $(app_name:%=$(dep_dir)%$(dep_app_suffix))
app_plt_file = $(app_name:%=$(dep_dir)app.%.plt.d)
application_dep_files = \
  $(foreach mod_name,\
            $(app_modules),\
            $(erl_dep_file) ) \
  $(app_dep_file) \
  $(app_plt_file)





#
# NOTE: If any file is added or deleted within the src directory, update the
#       dependency files. This works for local include files that are added
#       or removed.
#
# BUGBUG: This won't work if the source depends on an include in a different
#         application. There is no back-link to the source file, and no way
#         to regenerate the .erl.d file.
#
# FIX: The d_include_lib paths can be clipped and sorted:
#
#      $(sort $(dir $(d_include_lib)))
#
#      This will make the .erl.d depend on the include directories as well
#      as the individual header files.
#

define refresh_dep
$(application_dep_files): $(app_name:%=lib/%/src)
endef

$(foreach app_name,\
          $(App_Names),\
          $(eval $(refresh_dep)) )

d_module = $*
d_erl_dep = $(d_module:%=$(dep_dir)%$(dep_erl_suffix))

d_erl = $($(d_module:%=%_module_erl))
d_beam = $($(d_module:%=%_module_beam))

d_dir = $(dir $(d_erl))
d_src_headers = $(shell sed -n $(include_regexp) < $(d_erl))
d_include = $(d_src_headers:%=$(d_dir)%)

d_lib_headers = $(shell sed -n $(include_lib_regexp) < $(d_erl))
d_include_lib = $(wildcard $(d_lib_headers:%=lib/%))

d_include_files = $(d_include) $(d_include_lib)

d_parse_xform_mod = $(shell sed -n $(parse_transform_regexp) < $(d_erl))
d_parse_xform_beams = \
  $(foreach mod_name,\
            $(d_parse_xform_mod),\
            $($(mod_name:%=%_module_beam)))


d_parse_xform_apps = \
  $(filter-out $($($(d_module)_app)_app_dependencies),\
  $(xform_beams))
d_parse_xform_dep = \
  $(d_beam): $(d_erl) $(d_include_files) $(d_parse_xform_beams) $(d_parse_xform_apps)

d_parse_xform_path = $(d_beam:%=%: Compilation_Flags += $(xform_flags))

#
# DEBUGGING TOOL 
#
?%.erl.d:
	$(info ==== $@ ==== )
	$(if $(d_parse_xform_apps),\
	     $(info d_parse_xform_apps ),\
	     $(info d_parse_xform_apps is empty) )
	$(foreach value,$(d_parse_xform_apps),$(info $(bullet)$(value)) )
	$(info d_parse_xform_dep $(d_parse_xform_dep))
	$(info ======== )
#
# NOTE: the %.erl.d file contents depends entirely on what files the .erl
#        file includes. If the header files change or disappear, it is no
#        matter for the %.erl.d file: only changes in the .erl file matter
#        to it.
#
# BUGBUG: If a .erl source file is moved from one application to another,
#         the dependency change is not detected. Trying to force a refresh
#         of the .erl.d file with `make depend` fails with:
#
#         "cannot stat 'XXX.erl': No such file or directory"
#
#         The $(d_dir) dependency does not cope with the removal and
#         relocation of a .erl file. Deleting the .erl.d file is a remedy.
#
#         Also related: the .app file of application where the .erl file is
#         moved to is not regenerated.  This may be due to %MODULES% not
#         changing, even if a module is added to the application.  Remedy:
#         Deleting the relevant lib/*/ebin/*.app file will force a rebuild.
#
tmp/dependencies/%.erl.d:
	@mkdir -p $(dir $@)
	@printf "$(d_erl_dep): $(d_erl) $(d_dir)\n\n" > $(d_erl_dep)
	@printf "$(d_parse_xform_path)\n" >> $(d_erl_dep)
	@printf "$(d_parse_xform_dep)\n\n" >> $(d_erl_dep)
	@printf ".PHONY: $(notdir $(d_beam))\n" >> $(d_erl_dep)
	@printf "$(notdir $(d_beam)): $(d_beam)\n\n" >> $(d_erl_dep)

d_app_name = $*
d_app_dep = $(d_app_name:%=$(dep_dir)%$(dep_app_suffix))
d_app_prereq = $($(d_app_name)_app_dependencies)
d_app_target = $(d_app_name:%=%.app)
d_app_src_dir = $(d_app_name:%=lib/%/src/)
d_app_svn_mk = $(d_app_src_dir:%=%vsn.mk)
d_app_src_file = $($(d_app_name)_src_file)
d_app_file = $($(d_app_name)_file)

d_app_priv_files = $($(d_app_name)_priv_files)
d_app_priv_dir = $(d_app_name:%=lib/%/priv/)
d_app_vsn = $($(d_app_name)_has_vsn)
d_app_priv_vsn = $(d_app_name:%=lib/%-$(d_app_vsn)/priv/)

d_priv_depend = \
$(subst :,: ,$(subst $(space),\\n\\n,$(strip \
   $(foreach priv,\
             $(d_app_priv_files),\
             $(d_app_priv_vsn)$(priv):$(d_app_priv_dir)$(priv)))))

#
# BUGBUG: the $(d_app_dep): $(d_app_prereq) rule is far too broad
#         In the .erl.d file, the .beam prerequisites are handled.
#
# NOTE: The %.app.d ought to depend on the .erl.d files and the MIBS, priv
#       files etc. I.e. source files, not the beam files.
#
tmp/dependencies/%.app.d:
	@mkdir -p $(dir $@)
	@printf "$(d_app_dep): $(d_app_src_dir) $(d_app_svn_mk)\n\n" > $(d_app_dep)
	@printf ".PHONY: $(d_app_target)\n" >> $(d_app_dep)
	@printf "$(d_app_target): $(d_app_prereq)\n\n" >> $(d_app_dep)
	@printf "$(d_app_file): $(d_app_src_file)\n\n" >> $(d_app_dep)
	@printf "$(d_priv_depend)\n\n" >> $(d_app_dep)

d_plt_dep = $(d_app_name:%=$(dep_dir)app.%.plt.d)
d_app_plt = $(d_app_name:%=tmp/app.%.plt)
d_app_modules = $(foreach app_name,$(d_app_name),$(app_beam_files))

tmp/dependencies/app.%.plt.d:
	@mkdir -p $(dir $@)
	@printf "$(d_app_plt): $(d_app_modules)\n" >> $(d_plt_dep)


d_rel_name = $*
d_rel_dep = $(d_rel_name:%=$(dep_dir)%$(dep_rel_suffix))

d_rel_tar_gzip = $($(d_rel_name)_rel_tar_gzip)

d_rel_depend_apps = $($(d_rel_name)_rel_depend_apps)
d_rel_src_dir = $(d_rel_name:%=releases/%/src/)
d_rel_source = $(d_rel_name:%=$(d_rel_src_dir)%.rel.src)
d_rel_vsn_mk = $(d_rel_src_dir:%=%vsn.mk)
d_rel_app_vsn_mk = $(d_rel_depend_apps:%=$(dep_dir)%$(dep_app_suffix))
d_rel_dependencies = \
  $(d_rel_source) \
  $(foreach app_name,\
            $(d_rel_depend_apps),\
            $(app_dependencies) )

d_rel_target_dir = $($(d_rel_name)_rel_target_dir)

d_rel_file = $($(d_rel_name)_rel_file)
d_rel_boot = $($(d_rel_name)_boot_file)
d_rel_target = $(d_rel_name:%=%.rel)

d_boot_prereq = $(d_rel_file) $(d_rel_dependencies)
d_tar_prereq = \
  $(d_rel_file) \
  $(d_rel_boot) \
  $(d_rel_target_dir:%=%sys.config)

d_all_prereq = $(d_rel_tar_gzip) $(d_boot_prereq) $(d_tar_prereq)

d_rel_dialyzer = $(d_rel_name:%=%.dialyzer)
d_rel_dia_log = $(d_rel_name:%=tmp/%-dialyzer.log)
d_rel_library_apps = $($(d_rel_name)_rel_library_apps)
d_rel_app_plts = $(d_rel_depend_apps:%=tmp/app.%.plt)
d_rel_otp_plts = $(d_rel_library_apps:%=tmp/otp.%.plt)
d_rel_plt_files = $(d_rel_app_plts) $(d_rel_otp_plts)



#
# BUGBUG: the $(d_rel_dep): $(d_all_prereq) rule is far too broad
#

tmp/dependencies/%.rel.d:
	@mkdir -p $(dir $@)
	@printf "\n$(d_rel_dep): $(d_rel_src_dir) $(d_rel_vsn_mk) $(d_rel_app_vsn_mk)\n\n" > $(d_rel_dep)
	@printf ".PHONY: $(d_rel_target)\n" >> $(d_rel_dep)
	@printf "$(d_rel_target): $(d_rel_tar_gzip)\n\n" >> $(d_rel_dep)
	@printf "$(d_rel_file): $(d_rel_source) $(d_rel_vsn_mk) $(d_rel_app_vsn_mk)\n\n" >> $(d_rel_dep)
	@printf "$(d_rel_boot): $(d_boot_prereq)\n\n" >> $(d_rel_dep)
	@printf "$(d_rel_tar_gzip): $(d_tar_prereq)\n\n" >> $(d_rel_dep)
	@printf ".PHONY: $(d_rel_name)\n" >> $(d_rel_dep)
	@printf "$(d_rel_name): $(d_rel_dependencies) $(d_rel_file) $(d_rel_boot)\n\n" >> $(d_rel_dep)
	@printf ".PHONY: $(d_rel_dialyzer)\n" >> $(d_rel_dep)
	@printf "$(d_rel_dialyzer): $(d_rel_dia_log)\n\n" >> $(d_rel_dep)
	@printf "$(d_rel_dia_log): $(d_rel_plt_files)\n\n" >> $(d_rel_dep)


d_pack_name = $*
d_pack_dep = $(d_pack_name:%=$(dep_dir)%$(dep_pack_suffix))
d_pack_tar_gzip = $($(d_pack_name)_rel_tar_gzip)
d_pack_target = $(d_pack_name:%=%.package)

d_pack_rel_vsn = $($(d_pack_name)_rel_vsn)
d_pack_file = $(d_pack_name:%=%-$(d_pack_rel_vsn).package)

tmp/dependencies/%.pack.d:
	@mkdir -p $(dir $@)
	@printf "\n$(d_pack_dep): support bin\n\n" >> $(d_pack_dep)
	@printf "$(d_pack_file): $(d_pack_tar_gzip) support/Install-script.src bin/erl_heart.src\n\n" >> $(d_pack_dep)
	@printf ".PHONY: $(d_pack_target)\n" >> $(d_pack_dep)
	@printf "$(d_pack_target): $(d_pack_file)\n\n" >> $(d_pack_dep)




all_dep_files = \
  $(foreach rel_name,\
            $(Rel_Names),\
            $(rel_dep_file) $(pack_dep_file)) \
  $(foreach app_name,\
            $(App_Names),\
            $(application_dep_files) )

-include $(all_dep_files)

.PHONY: depend
depend: $(all_dep_files)
	@printf "Dependency files regenerated\n"




#
# NOTE: implicit rule to compile beam files from erl source.
#
w_match = $*
w_module = $(notdir $(w_match))
w_src = $($(w_module:%=%_module_erl))
w_vsn = $($(w_module:%=%_app_vsn))
w_app = $($(w_module:%=%_app))
w_asn1 = $(w_app:%=lib/%/asn1/$(w_module:%=%.asn1))

lib/%.erl:
	echo $(ERLC) \
	  $(Compilation_Flags) \
	  -I lib \
	  -o $(dir $@) \
	  $(w_asn1)

GitIdent = $(shell support/keyword-tagging.sh $(^))
lib/%.beam:
	@mkdir -p $(dir $@)
	$(ERLC) \
	  $(Compilation_Flags) \
	  -DVSN='"$(w_vsn)"' \
	  -DCOMPILE_TIME_STAMP='"$(date_now)"'\
          $(GitIdent)\
          -DBuildHost='"$$BuildHost: $(hostname) $$"'\
	  -I lib \
	  -o $(dir $@) \
	  $(w_src)
	@mkdir -p $(dir $(reload_list_file))
	@printf "$@\n" >> $(reload_list_file)



rs_match = $*
rs_name = $(firstword $(subst -, ,$(notdir $(rs_match))))
rs_vsn = $($(rs_name:%=%_rel_vsn))
rs_rel_source = $($(rs_name)_rel_src_file)
rs_rel_target = $(rs_match:%=releases/%.rel)

applications := $(strip \
                  $(foreach app_name,\
                            $(App_Names),\
                            $(app_name)-$(app_vsn) ) )
infix:=/;s/%
version_map := $(strip $(applications) $(OTP_VERSIONS))

version_regexp = 's/%VSN%/$(rs_vsn)/;s/%$(subst $(space),$(infix),$(subst -,%/,$(version_map)))/'
releases/%.rel:
	mkdir -p $(dir $@)
	sed -e $(version_regexp) < "$(rs_rel_source)" > "$(rs_rel_target)"


tgz_match = $*
tgz_make_tar = $(notdir $(tgz_match))
tgz_rel_name = $(firstword $(subst -, ,$(tgz_make_tar)))
tgz_path = $(realpath $($(tgz_rel_name)_rel_ebin_dirs))
releases/%.tar.gz:
	@mkdir -p $(dir $@)
	$(ERL) \
	  -noshell \
	  -pa $(tgz_path) \
	  -eval "{ok,[[Root]]}=init:get_argument(root),\
	         c:cd(\"$(dir $@)\"),\
	         {ok,systools_make,_}\
	           = systools:make_tar(\"$(tgz_make_tar)\",\
	                               [{erts,Root}, silent]),\
	         init:stop()."

boot_match = $*
boot_rel_name = $(firstword $(subst -, ,$(notdir $(boot_match))))
boot_ebin_dirs = $($(boot_rel_name)_rel_ebin_dirs)
boot_includes = $(boot_ebin_dirs:%=-I %)
boot_rel_file = $(boot_match:%=releases/%.rel)
releases/%.boot:
	@mkdir -p $(dir $@)
	$(ERLC) -b boot \
	  +"{variables,[{\"ROOT\",\"$(realpath .)\"}]}" \
	  $(boot_includes) \
	  -o $@ $(boot_rel_file) | sed '/Source code not found/d'



patch_regexp = 's/%$(subst $(space),$(infix),$(subst -,%/,$(version_map)))/'
%.rel: %.rel.src
	mkdir -p $(dir $@)
	sed -e $(patch_regexp) < "$^" > "$@"

erts-$(erts_otp_vsn)/bin/%.src: bin/%.src
	@mkdir -p $(dir $@)
	cp "$^" "$@"

erts-$(erts_otp_vsn)/lib/%: $(ERLDIR)erts-$(erts_otp_vsn)/lib/%
	@mkdir -p $(dir $@)
	cp "$^" "$@"

erts-$(erts_otp_vsn)/bin/%.boot: bin/%.rel
	@mkdir -p $(dir $@)
	$(ERLC) -b boot \
	  +"{variables,[{\"ROOT\",\"$(realpath .)\"}]}" \
	  -o "$@" "$^" | sed '/Source code not found/d'

erts_bin = $(erts_otp_vsn:%=erts-%/bin/)

.PHONY: erts_patch
erts_patch: $(erts_bin:%=%erl_heart.src) \
            $(erts_bin:%=%service.src) \
            $(erts_bin:%=%start_sasl.boot) \
            $(erts_bin:%=%start_clean.boot)






Install_Script := support/Install-script.src


pk_match = $*
pk_rel_dir = $(pk_match:%=releases/%/)
pk_rel_tgz = $(pk_match:%=$(pk_rel_dir)%.tar.gz)

%.package: erts_patch
	mv $(pk_rel_tgz) $(pk_rel_tgz:%=%.ORIGINAL)
	gzip -d -c $(pk_rel_tgz:%=%.ORIGINAL) > ./$*-tmp.tar
	tar --format=ustar --append --file=./$*-tmp.tar \
	  $(erts_otp_vsn:%=erts-%/bin/*.src) \
	  $(erts_otp_vsn:%=erts-%/bin/*.boot) \
	  SMF/erlang_epmd.xml \
	  SMF/manifest.xml \
	  SMF/erlang_service
	gzip -c <./$*-tmp.tar > $(pk_rel_tgz)
	rm ./$*-tmp.tar
	./support/make-package \
	  --output $@ \
	  --script $(Install_Script) \
	  $(pk_rel_tgz)














#
# Last-resort default rule for copying priv files.
#

l_match = $*
l_lib_target = $(l_match:%=lib/%)
l_rel_target = $(l_match:%=releases/%)

l_app_vsn = $(firstword $(subst /, ,$(l_match)))
l_path = $(l_match:$(l_app_vsn)%=%)
l_app_name = $(firstword $(subst -, ,$(l_app_vsn)))

l_lib_source = $(l_path:%=lib/$(l_app_name)%)
l_rel_source = $(l_path:%=releases/$(l_app_name)/src%.src)


lib/%:
	@mkdir -p "$(dir $@)"
	cp "$(l_lib_source)" "$(l_lib_target)"

releases/%:
	@mkdir -p "$(dir $@)"
	cp "$(l_rel_source)" "$(l_rel_target)"


#
# NOTE: Some applications may not have source available, like QuviqCheck
#       eqc. Their beam files etc.  must be preserved during cleaning.
#
apps_without_source := eqc
apps_with_source = $(filter-out \
                     $(apps_without_source), \
                     $(App_Names) )

clean_editor_backup = \
  $(wildcard lib/*/src/*~) \
  $(wildcard lib/*/include/*~) \
  $(wildcard releases/*/src/*~) \
  $(wildcard *~)

clean_app_dirs := $(strip \
                    $(foreach app_name,\
                              $(apps_with_source), \
                              lib/$(app_name)-$(app_vsn) ) )

clean_rel_dirs := $(foreach rel_name,\
                            $(Rel_Names),\
                            releases/$(rel_name)-$(rel_vsn) )

package_targets := $(foreach rel_name,\
                             $(Rel_Names),\
                             $(rel_name:%=%.package) )

package_files := $(foreach rel_name,\
                           $(Rel_Names),\
                           $(rel_name:%=%-$(rel_vsn).package) \
                           $(rel_name:%=Install-%-$(rel_vsn)) )

.PHONY: dep_clean
dep_clean:
	rm -rf $(dep_dir)

.PHONY: clean
clean: dep_clean
	rm -rf $(wildcard bin/*.rel) \
	       $(wildcard lib/*/c_src/*.o) \
	       $(package_files) \
	       $(erts_otp_vsn:%=erts-%) \
	       $(clean_app_dirs) \
	       $(clean_rel_dirs) \
	       $(clean_editor_backup) \
	       $(dir $(reload_list_file))


.PHONY: all
all: $(foreach app_name,$(App_Names),$(app_name:%=%.app))

.PHONY: nothing
nothing:
	@printf "ERL $(ERL)\n"
	@printf "ERLDIR $(ERLDIR)\n"
	@printf "ERL_BIN_DIR $(ERL_BIN_DIR)\n"
	@printf "OTP_SRC_DIRS $(OTP_SRC_DIRS)\n"
	@printf "OTP_VERSIONS $(OTP_VERSIONS)\n"

#
# NOTE: For testing and debugging, launch the Erlang runtime with paths set
#       to enable loading all the project modules.
#
# NOTE: The run_boot target, when the SNAME environment is set, will use the
#       corresponding boot file to launch a runtime system.
#
boot_file_vsn = $($(SNAME)_rel_vsn)
boot_file_name = $(SNAME:%=%-$(boot_file_vsn))
boot_param = ./releases/$(boot_file_name)/$(boot_file_name)
run_bootfile = -boot $(boot_param)

.PHONY: run_boot
run_boot: all
	$(ERL) -smp +P 134217727 \
	  $(SNAME:%=-sname %) $(SETCOOKIE:%=-setcookie %) \
	  $(applications:%=-pa lib/%/ebin) \
	  $(run_bootfile)

.PHONY: run
run: all
	$(ERL) -smp +P 134217727 \
	  $(SNAME:%=-sname %) $(SETCOOKIE:%=-setcookie %) \
	  $(applications:%=-pa lib/%/ebin)


make_executable := $(shell which $(MAKE))

.PHONY: unit_test
unit_test: all
	SETCOOKIE=$(@:%=%_cookie) \
	MAKE_GOAL=run \
	SNAME=$@ \
	SHELL="$(make_executable)" \
	  script -a ./tmp/$@.log

reload: all
	./support/reload-code


.PHONY: package
package: $(package_targets)

plt_app = $*
plt_vsn = $($(plt_app)_has_vsn)
plt_ebin = $(plt_app:%=lib/%-$(plt_vsn)/ebin/)
plt_modules = $($(plt_app)_modules)
plt_beams = $(plt_modules:%=$(plt_ebin)%.beam)

Dialyzer_warnings = \
  -Wunmatched_returns \
  -Werror_handling \
  -Wrace_conditions \
  -Wunderspecs


tmp/app.%.plt: tmp/otp.erts.plt tmp/otp.kernel.plt tmp/otp.stdlib.plt
	$(info plt_app=$(plt_app))
	$(info plt_beams=$(plt_beams))
	dialyzer \
	--plts $(^) \
	$(Dialyzer_warnings) \
	--get_warnings \
	--check_plt \
	$(plt_beams:%=-c %) \
	--build_plt --verbose \
	--output_plt $@




tmp/otp.%.plt:
	$(info input $*)
	dialyzer \
	$(Dialyzer_warnings) \
	--get_warnings \
	--check_plt \
	--apps $* \
	--build_plt --verbose \
	--output_plt $@

dia_name = $*
dia_rels = $($(dia_name)_is_in_release)
dia_app_list = \
 $(filter-out $(dia_name),\
   $(sort \
     $(foreach rel_name,$(dia_rels),$(rel_apps))) )

dia_apps = $(filter $(App_Names),$(dia_app_list))
dia_otp = $(filter-out $(App_Names),$(dia_app_list))
dia_app_plts = $(dia_apps:%=tmp/app.%.plt)
dia_otp_plts = $(dia_otp:%=tmp/otp.%.plt)
dia_plt_files = $(dia_app_plts) $(dia_otp_plts)
dia_beam_files = $(foreach app_name,$(dia_name),$(app_beam_files))


%.wibble:
	$(info Application $* appears in: $(dia_rels))
	$(info Which in turn contains applications: $(dia_app_list))
	$(info Of which there are OTP application: $(dia_otp))
	$(info Of which there are release applications: $(dia_apps))
	$(info Needing PLT files: $(dia_plt_files))
	$(info Needing BEAM files: $(dia_beam_files))

#
# TODO: More work required.
#       e.g. make tmp/$(release_name)-dialyzer.log
#
tmp/%-dialyzer.log: $$(dia_plt_files)
	dialyzer \
	--plts $(dia_plt_files) \
	$(Dialyzer_warnings) \
	--get_warnings \
	--check_plt \
	$(dia_beam_files:%=-c %) 2>&1 | tee -a $@


#
# TODO: Target xref is made redundant by the use of Dialyzer.
#
tmp/xref.log: all
	@printf "==== XREF analysis $(date_now) ====\n" >> $@
	$(ERL) \
	       $(addprefix -pa ,$(wildcard ./lib/*-*/ebin)) \
	       -noshell \
		-sname xref_node -setcookie xref_node_cookie \
	       -s code_analysis run_analysis \
	       | tee -a $@ 2>&1

.PHONY: xref
xref: tmp/xref.log

%.doc:
	$(ERL) \
	  -noshell \
	  -eval "edoc:application($(*),\"$(*:%=lib/%)\",[{hidden,true},{private,true},{todo,true}]),init:stop()."





Sources_md := $(wildcard doc/*.md)
Targets_md = $(Sources_md:doc/%.md=html/%.html)

dollar:=$$
code:=\`\`\`

Contents = $(*:%=%.toc)
TOC_Regexp = /Table of contents./r $(Contents)
Pretty_regexp = s/$(code)\\([a-z][a-z]*\\)/$(code)prettyprint lang-\\1/

%.toc: doc/%.md
	printf "\n" > "$@"
	md-toc < "$^" >> "$@"

README.md: $(wildcard ./doc/*.md)
	printf "\n# sevas\n\n" > "$@"
	./doc/Readme.sh $^ >> "$@"

%.linked: doc/%.md
	./doc/Links.sh "$^" > "$@"

%.with-toc: doc/%.md
	sed "$(TOC_Regexp)" < "$^" > "$@"

html/%.html: %.linked %.toc
	mkdir -p $(dir $@)
	./doc/Header.sh "$<" | sed "$(TOC_Regexp)" | sed '$(Pretty_regexp)' | multimarkdown > "$@"

%.html: %.md
	./doc/Links.sh "$^" | multimarkdown > "$@"

.PHONY: doc
doc: $(Targets_md) README.md

#
# DEBUGGING TOOL
#
bullet:=$(space)$(space)*$(space)
display = $(foreach suffix, _app \
			    _app_vsn \
			    _module_erl \
			    _module_beam \
			    _mibs \
			    _priv_files \
			    _mib_bin_files \
			    _app_dependencies \
			    _src_file \
			    _file \
			    _module \
			    _has_vsn \
			    _rel_vsn \
			    _rel_src_file \
			    _rel_app \
			    _rel_depend_app \
			    _rel_library_apps \
			    _rel_ebin_dirs \
			    _rel_tar_gzip \
			    _boot_file \
			    _rel_file \
			    _rel_target_dir \
			    _is_in_release \
			    ,\
		    $(info $(value:%=%$(suffix)) )\
		    $(foreach value,$($(value:%=%$(suffix))), \
		   $(info $(bullet)$(value) ) ) )

?%.beam ?%.app ?%.rel ?%.package:
	$(info ==== DEBUG Target $(@:?%=%) BEGIN ==== )
	$(foreach value,$(*),$(display))
	$(info ==== DEBUG Target $(@:?%=%) END ==== )

?%:
	$(info $(*) )
	$(foreach value,$($(*)), \
	  $(info $(bullet)$(value) ) )

.PHONY: App_Names
App_Names:
	$(info $(App_Names))
