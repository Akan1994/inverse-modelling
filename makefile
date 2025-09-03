F90      = /apps/sw/ubuntu22.04/gcc-11.4.0/gcc-13.2.0-tkesyophy2o6rjlzknndu3b4oyasvuqm/bin/gfortran
LIBPATH1 = /apps/sw/ubuntu22.04/gcc-13.2.0/netcdf-fortran-4.6.1-ubcs4l6pjwpgoeyvz32wpzyzykib6bwm/lib/
INCPATH1 = /apps/sw/ubuntu22.04/gcc-13.2.0/netcdf-fortran-4.6.1-ubcs4l6pjwpgoeyvz32wpzyzykib6bwm/include/

# OPTIMIZATION LEVEL
O_LEV = 0 # [0,1,2,3,g,s,fast]

LIBS = -lm -DUSE_NCF -lnetcdff

FFLAGS   = -I$(INCPATH1) -O$(O_LEV) -g -cpp -m64 -mcmodel=medium -fconvert=little-endian -frecord-marker=4 -fmessage-length=0 -flto=jobserver -O$(O_LEV) -DUSE_NCF -lnetcdff -fbacktrace -Warray-bounds -fcheck=all # -march=native

LDFLAGS  = $(FFLAGS) -L$(LIBPATH1) -Wl,-rpath,$(LIBPATH1) $(LIBS)

MAIN = prep_flexpart

MODULES  =   mod_var.o \
         mod_settings.o \
         mod_dates.o \
         mod_tools.o \
         mod_obs.o \
         mod_strings.o \
         mod_prep_ageclass.o \
         mod_prep_command.o

OBJECTS =   read_reclist.o \
         list_obsfiles.o \
         prep_pathnames.o \
         prep_outgrid.o \
         prep_reagents.o \
         prep_releases.o \
         prep_releases_reg.o \
         process_obs.o \
         read_obspack.o \
         read_wdcgg.o \
         read_noaa.o \
         read_icos.o \
         read_basic.o \
         main.o

%.o: %.mod

$(MAIN): $(MODULES) $(OBJECTS)
	+$(F90) -o $@ $(MODULES) $(OBJECTS) $(LDFLAGS)

%.o : %.f90
	+$(F90) -c $(FFLAGS) $< 

clean:
	rm -f *.o *.mod

.SUFFIXES = $(SUFFIXES) .f90

