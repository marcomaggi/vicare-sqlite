## Process this file with automake to produce Makefile.in

vicare_sqlite3_CURRENT	= @vicare_sqlite3_VERSION_INTERFACE_CURRENT@
vicare_sqlite3_REVISION	= @vicare_sqlite3_VERSION_INTERFACE_REVISION@
vicare_sqlite3_AGE	= @vicare_sqlite3_VERSION_INTERFACE_AGE@
vicare_sqlite3_VERSION	= $(vicare_sqlite3_CURRENT):$(vicare_sqlite3_REVISION):$(vicare_sqlite3_AGE)

lib_LTLIBRARIES			= libvicare-sqlite3.la
libvicare_sqlite3_la_LDFLAGS	= -version-info $(vicare_sqlite3_VERSION)
libvicare_sqlite3_la_SOURCES	= \
	vicare-sqlite-version.c		\
	vicare-sqlite-library.c		\
	vicare-sqlite-exec.c		\
	vicare-sqlite-connect.c		\
	vicare-sqlite-statements.c	\
	vicare-sqlite-extensions.c	\
	vicare-sqlite-blobs.c		\
	vicare-sqlite-functions.c	\
	vicare-sqlite-backup.c		\
	vicare-sqlite-collation.c	\
	vicare-sqlite-misc.c		\
	vicare-sqlite-internals.h

### end of file
