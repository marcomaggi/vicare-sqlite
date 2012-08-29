/*
  Part of: Vicare/SQLite
  Contents: collation functions
  Date: Wed Aug 29, 2012

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
 ** Creating a collation.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_create_collation (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION
  sqlite3_create_collation();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation_v2 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION_V2
  sqlite3_create_collation_v2();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_create_collation16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_CREATE_COLLATION16
  sqlite3_create_collation16();
#else
  feature_failure(__func__);
#endif
}


/** --------------------------------------------------------------------
 ** Accepting collation requests.
 ** ----------------------------------------------------------------- */

ikptr
ik_sqlite3_collation_needed (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLLATION_NEEDED
  sqlite3_collation_needed();
#else
  feature_failure(__func__);
#endif
}
ikptr
ik_sqlite3_collation_needed16 (ikpcb * pcb)
{
#ifdef HAVE_SQLITE3_COLLATION_NEEDED16
  sqlite3_collation_needed16();
#else
  feature_failure(__func__);
#endif
}

/* end of file */
