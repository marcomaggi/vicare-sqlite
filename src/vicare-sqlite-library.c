/*
  Part of: Vicare/SQLite
  Contents: SQLite library operations
  Date: Tue Jul 31, 2012

  Abstract



  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  This program is  free software: you can redistribute  it and/or modify
  it under the  terms of the GNU General Public  License as published by
  the Free Software Foundation, either version  3 of the License, or (at
  your option) any later version.

  This program  is distributed in the  hope that it will  be useful, but
  WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
  MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
  General Public License for more details.

  You should  have received  a copy  of the  GNU General  Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


/** --------------------------------------------------------------------
 ** Headers.
 ** ----------------------------------------------------------------- */

#include <vicare-sqlite-internals.h>


/** --------------------------------------------------------------------
 ** Library initialisation and finalisation.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_initialize (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_INITIALIZE
  int	rv = sqlite3_initialize();
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_shutdown (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SHUTDOWN
  int	rv = sqlite3_shutdown();
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_os_init (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OS_INIT
  int	rv = sqlite3_os_init();
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_os_end (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_OS_END
  int	rv = sqlite3_os_end();
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Library configuration.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_config (ikptr s_option_identifier, ikptr s_args, ikpcb * pcb)
/* Interface  to the  C  function "sqlite3_config()";  this function  is
   variadic.   If successful  return  SQLITE_OK, else  return a  SQLITE_
   error code.

   This function  accepts a fixnum  as first argument,  representing the
   option  identifier as  one of  the SQLITE_CONFIG_  constants; if  the
   option  identifier  is  not  recognised here:  the  return  value  is
   SQLITE_ERROR.

   S_ARGS  must be  false,  if no  arguments where  given,  or a  vector
   holding the arguments. */
{
#ifdef HAVE_SQLITE3_CONFIG
  int	option_identifier = IK_UNFIX(s_option_identifier);
  int	rv;
  /* fprintf(stderr, "id %d, args %ld\n", option_identifier, (long)s_args); */
  switch (option_identifier) {
#ifdef SQLITE_CONFIG_SINGLETHREAD
  case SQLITE_CONFIG_SINGLETHREAD:
    if (false_object == s_args)
      rv = sqlite3_config(option_identifier);
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_MULTITHREAD
  case SQLITE_CONFIG_MULTITHREAD:
    if (false_object == s_args)
      rv = sqlite3_config(option_identifier);
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_SERIALIZED
  case SQLITE_CONFIG_SERIALIZED:
    if (false_object == s_args)
      rv = sqlite3_config(option_identifier);
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_MALLOC
  case SQLITE_CONFIG_MALLOC:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier, IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_GETMALLOC
  case SQLITE_CONFIG_GETMALLOC:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier, IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_SCRATCH
  case SQLITE_CONFIG_SCRATCH:
    if ((3 == IK_VECTOR_LENGTH(s_args)) &&
	ik_is_pointer(IK_ITEM(s_args, 0)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 1)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 2)))
      rv = sqlite3_config(option_identifier,
			  IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)),
			  IK_UNFIX(IK_ITEM(s_args, 1)),
			  IK_UNFIX(IK_ITEM(s_args, 2)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_PAGECACHE
  case SQLITE_CONFIG_PAGECACHE:
    if ((3 == IK_VECTOR_LENGTH(s_args)) &&
	ik_is_pointer(IK_ITEM(s_args, 0)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 1)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 2)))
      rv = sqlite3_config(option_identifier,
			  IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)),
			  IK_UNFIX(IK_ITEM(s_args, 1)),
			  IK_UNFIX(IK_ITEM(s_args, 2)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_HEAP
  case SQLITE_CONFIG_HEAP:
    if ((3 == IK_VECTOR_LENGTH(s_args)) &&
	ik_is_pointer(IK_ITEM(s_args, 0)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 1)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 2)))
      rv = sqlite3_config(option_identifier,
			  IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)),
			  IK_UNFIX(IK_ITEM(s_args, 1)),
			  IK_UNFIX(IK_ITEM(s_args, 2)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_MEMSTATUS
  case SQLITE_CONFIG_MEMSTATUS:
    if (1 == IK_VECTOR_LENGTH(s_args))
      rv = sqlite3_config(option_identifier, (false_object == IK_ITEM(s_args, 0))? 0 : 1);
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_MUTEX
  case SQLITE_CONFIG_MUTEX:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier, IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_GETMUTEX
  case SQLITE_CONFIG_GETMUTEX:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier, IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_LOOKASIDE
  case SQLITE_CONFIG_LOOKASIDE:
    if ((2 == IK_VECTOR_LENGTH(s_args)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 0)) &&
	IK_IS_FIXNUM(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier,
			  IK_UNFIX(IK_ITEM(s_args, 0)),
			  IK_UNFIX(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_PCACHE
  case SQLITE_CONFIG_PCACHE:
    rv = SQLITE_OK;
    break;
#endif
#ifdef SQLITE_CONFIG_GETPCACHE
  case SQLITE_CONFIG_GETPCACHE:
    rv = SQLITE_OK;
    break;
#endif
#ifdef SQLITE_CONFIG_LOG
  case SQLITE_CONFIG_LOG:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier,
			  IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)),
			  IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0))
			  );
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_URI
  case SQLITE_CONFIG_URI:
    if (1 == IK_VECTOR_LENGTH(s_args))
      rv = sqlite3_config(option_identifier, (false_object == IK_ITEM(s_args, 0))? 0 : 1);
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_PCACHE2
  case SQLITE_CONFIG_PCACHE2:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier, IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
#ifdef SQLITE_CONFIG_GETPCACHE2
  case SQLITE_CONFIG_GETPCACHE2:
    if ((1 == IK_VECTOR_LENGTH(s_args)) && ik_is_pointer(IK_ITEM(s_args, 0)))
      rv = sqlite3_config(option_identifier, IK_POINTER_DATA_VOIDP(IK_ITEM(s_args, 0)));
    else
      rv = SQLITE_ERROR;
    break;
#endif
  default:
    return ika_integer_from_sqlite_errcode(pcb,SQLITE_ERROR);
  }
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Library auxiliary functions.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_memory_used (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MEMORY_USED
  return ika_integer_from_sint64(pcb, sqlite3_memory_used());
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_memory_highwater (ikptr s_reset, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_MEMORY_HIGHWATER
  int	reset = (false_object == s_reset)? 0 : 1;
  return ika_integer_from_sint64(pcb, sqlite3_memory_highwater(reset));
#else
  feature_failure(__func__);
#endif
}

/* ------------------------------------------------------------------ */

ikptr
ik_sqlite3_enable_shared_cache (ikptr s_bool)
{
#ifdef HAVE_SQLITE3_ENABLE_SHARED_CACHE
  int	bool = (s_bool == false_object);
  int	rv;
  rv = sqlite3_enable_shared_cache(bool);
  return ika_integer_from_sqlite_errcode(pcb,rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_release_memory (ikptr s_number_of_bytes, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_RELEASE_MEMORY
  int	number_of_bytes = ik_integer_to_int(s_number_of_bytes);
  int	rv;
  rv = sqlite3_release_memory(number_of_bytes);
  return ika_integer_from_int(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_soft_heap_limit64 (ikptr s_limit, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SOFT_HEAP_LIMIT64
  sqlite_int64	limit = ik_integer_to_sint64(s_limit);
  sqlite_int64	rv;
  rv = sqlite3_soft_heap_limit64(limit);
  return ika_integer_from_sint64(pcb, rv);
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_soft_heap_limit (ikptr s_limit, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_SOFT_HEAP_LIMIT
  int	limit = ik_integer_to_int(s_limit);
  sqlite3_soft_heap_limit(limit);
  return void_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Compile options.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_compileoption_used (ikptr s_option_name, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMPILEOPTION_USED
  const char *	option_name = IK_BYTEVECTOR_DATA_CHARP(s_option_name);
  int		rv;
  rv = sqlite3_compileoption_used(option_name);
  return rv? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_compileoption_get (ikptr s_index, ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COMPILEOPTION_GET
  int		idx = ik_integer_to_int(s_index);
  const char *	rv;
  rv = sqlite3_compileoption_get(idx);
  return rv? ika_bytevector_from_cstring(pcb, rv) : false_object;
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_threadsafe (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_THREADSAFE
  return (sqlite3_threadsafe())? true_object : false_object;
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Done.
 ** ----------------------------------------------------------------- */


/* end of file */
