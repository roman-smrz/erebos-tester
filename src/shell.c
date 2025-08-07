#define _GNU_SOURCE
#include <fcntl.h>
#include <unistd.h>

int shell_pipe_cloexec( int pipefd[ 2 ] )
{
	return pipe2( pipefd, O_CLOEXEC );
}
