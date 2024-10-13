#include "HsFFI.h"

#if defined(__GLASGOW_HASKELL__)
#include "Main_stub.h"
#endif

#include <errno.h>
#include <fcntl.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <sys/mount.h>
#include <unistd.h>

/*
 * The unshare call with CLONE_NEWUSER needs to happen before starting
 * additional threads, which means before initializing the Haskell RTS.
 * To achieve that, replace Haskell main with a custom one here that does
 * the unshare work and then executes the Haskell code.
 */

static bool writeProcSelfFile( const char * file, const char * data, size_t size )
{
	char path[ 256 ];
	if( snprintf( path, sizeof( path ), "/proc/self/%s", file )
			>= sizeof( path ) ){
		fprintf( stderr, "buffer too small\n" );
		return false;
	}

	int fd = open( path, O_WRONLY );
	if( fd < 0 ){
		fprintf( stderr, "failed to open %s: %s", path, strerror( errno ));
		return false;
	}

	ssize_t written = write( fd, data, size );
	if( written < 0 )
		fprintf( stderr, "failed to write to %s: %s\n", path, strerror( errno ));

	close( fd );
	return written == size;
}

int main( int argc, char * argv[] )
{
	uid_t uid = geteuid();
	gid_t gid = getegid();
	unshare( CLONE_NEWUSER | CLONE_NEWNET | CLONE_NEWNS );

	char buf[ 256 ];
	int len;

	len = snprintf( buf, sizeof( buf ), "%d %d %d\n", 0, uid, 1 );
	if( len >= sizeof( buf ) ){
		fprintf( stderr, "buffer too small\n" );
		return 1;
	}
	if ( ! writeProcSelfFile( "uid_map", buf, len ) )
		return 1;

	if ( ! writeProcSelfFile( "setgroups", "deny\n", 5 ) )
		return 1;

	len = snprintf( buf, sizeof( buf ), "%d %d %d\n", 0, gid, 1 );
	if( len >= sizeof( buf ) ){
		fprintf( stderr, "buffer too small\n" );
		return 1;
	}
	if ( ! writeProcSelfFile( "gid_map", buf, len ) )
		return 1;

	mount( "tmpfs", "/run", "tmpfs", 0, "size=4m" );

	hs_init( &argc, &argv );
	testerMain();
	hs_exit();

	return 0;
}
