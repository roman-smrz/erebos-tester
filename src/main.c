#include "HsFFI.h"

#if defined(__GLASGOW_HASKELL__)
#include "Main_stub.h"
#endif

#include <errno.h>
#include <fcntl.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/syscall.h>
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
	int ret;

	uid_t uid = geteuid();
	gid_t gid = getegid();
	ret = unshare( CLONE_NEWUSER | CLONE_NEWNET | CLONE_NEWNS );
	if( ret < 0 ){
		fprintf( stderr, "unsharing user, network and mount namespaces failed: %s\n", strerror( errno ));
		return 1;
	}

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

	/*
	 * Prepare for future filesystem isolation within additional mount namespace:
	 * - clone whole mount tree as read-only under new /tmp/new_root
	 * - keep writable /proc and /tmp
	 */

	ret = mount( "tmpfs", "/run", "tmpfs", 0, "size=4m" );
	if( ret < 0 ){
		fprintf( stderr, "failed to mount tmpfs on /run: %s\n", strerror( errno ));
		return 1;
	}

	ret = mkdir( "/run/new_root", 0700 );
	if( ret < 0 ){
		fprintf( stderr, "failed to create new_root directory: %s\n", strerror( errno ));
		return 1;
	}

	ret = mount( "/", "/run/new_root", NULL, MS_BIND | MS_REC,  NULL );
	if( ret < 0 ){
		fprintf( stderr, "failed to bind-mount / on new_root: %s\n", strerror( errno ));
		return 1;
	}

	struct mount_attr * attr_ro = &( struct mount_attr ) {
		.attr_set = MOUNT_ATTR_RDONLY,
	};
	ret = mount_setattr( -1, "/run/new_root", AT_RECURSIVE, attr_ro, sizeof( * attr_ro ) );
	if( ret < 0 ){
		fprintf( stderr, "failed set new_root as read-only: %s\n", strerror( errno ));
		return 1;
	}

	struct mount_attr * attr_rw = &( struct mount_attr ) {
		.attr_clr = MOUNT_ATTR_RDONLY,
	};
	ret = mount_setattr( -1, "/run/new_root/proc", AT_RECURSIVE, attr_rw, sizeof( * attr_rw ) );
	if( ret < 0 ){
		fprintf( stderr, "failed set new_root/proc as read-write: %s\n", strerror( errno ));
		return 1;
	}
	ret = mount_setattr( -1, "/run/new_root/tmp", AT_RECURSIVE, attr_rw, sizeof( * attr_rw ) );
	if( ret < 0 ){
		fprintf( stderr, "failed set new_root/tmp as read-write: %s\n", strerror( errno ));
	}

	ret = mount( "tmpfs", "/run/new_root/run", "tmpfs", 0, "size=4m" );
	if( ret < 0 ){
		fprintf( stderr, "failed to mount tmpfs on new_root/run: %s\n", strerror( errno ));
		return 1;
	}

	ret = mkdir( "/run/new_root/run/old_root", 0700 );
	if( ret < 0 ){
		fprintf( stderr, "failed to create old_root directory: %s\n", strerror( errno ));
		return 1;
	}

	hs_init( &argc, &argv );
	testerMain();
	hs_exit();

	return 0;
}

/*
 * - Replace filesystem hierarchy with read-only version,
 * - bind-mound rwdir from writable tree, and
 * - keep writeable /tmp from host.
 */
int erebos_tester_isolate_fs( const char * rwdir )
{
	int ret;

	ret = unshare( CLONE_NEWNS );
	if( ret < 0 ){
		fprintf( stderr, "unsharing mount namespace failed: %s\n", strerror( errno ));
		return -1;
	}

	char * cwd = getcwd( NULL, 0 );
	ret = syscall( SYS_pivot_root, "/run/new_root", "/run/new_root/run/old_root" );
	if( ret < 0 ){
		fprintf( stderr, "failed to pivot_root: %s\n", strerror( errno ));
		free( cwd );
		return -1;
	}

	char oldrwdir[ strlen(rwdir) + 15 ];
	snprintf( oldrwdir, sizeof oldrwdir, "/run/old_root/%s", rwdir );
	ret = mount( oldrwdir, rwdir, NULL, MS_BIND, NULL );
	if( ret < 0 ){
		fprintf( stderr, "failed to bind-mount %s on %s: %s\n", oldrwdir, rwdir, strerror( errno ));
		free( cwd );
		return -1;
	}

	ret = umount2( "/run/old_root", MNT_DETACH );
	if( ret < 0 ){
		fprintf( stderr, "failed to detach /run/old_root: %s\n", strerror( errno ));
		free( cwd );
		return -1;
	}

	ret = chdir( cwd );
	if( ret < 0 ){
		fprintf( stderr, "failed to chdir to %s: %s\n", cwd, strerror( errno ));
		free( cwd );
		return -1;
	}
	free( cwd );

	return 0;
}
