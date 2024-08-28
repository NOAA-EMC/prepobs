# prepobs 
Software necessary to process bufr dump files to generate prepbufr files.

To install:

Clone repository:
```bash
git clone https://github.com/noaa-emc/prepobs
```

Move into desired branch and then run:

```bash
INSTALL_PREFIX=/path/you/wish/to/install/prepobs ./ush/build.sh
```

build.sh default to building for WCOSS2, but it also supports Hera, Jet, and Orion by setting `INSTALL_TARGET=hera/jet/orion/wcoss2`

or install in local clone space:

```bash
./ush/build.sh
```

There is also the option to build and install in your local clone space but install the modulefile elsewhere:
```bash
MODULEFILE_INSTALL_PREFIX=/path/you/wish/to/install/prepobs/module ./ush/build.sh
```
Installation is complete.
