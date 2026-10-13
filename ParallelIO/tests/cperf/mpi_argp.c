#include <stdio.h>
#include <unistd.h>
#include <argp.h>

/**
 * Call <a href="http://www.gnu.org/s/libc/manual/html_node/Argp.html"
 * >Argp</a>'s \c argp_parse in an MPI-friendly way.  Processes
 * with nonzero rank will have their \c stdout and \c stderr redirected
 * to <tt>/dev/null</tt> during \c argp_parse.
 *
 * @param rank MPI rank of this process.  Output from \c argp_parse
 *             will only be observable from rank zero.
 * @param argp      Per \c argp_parse semantics.
 * @param argc      Per \c argp_parse semantics.
 * @param argv      Per \c argp_parse semantics.
 * @param flags     Per \c argp_parse semantics.
 * @param arg_index Per \c argp_parse semantics.
 * @param input     Per \c argp_parse semantics.
 *
 * @return Per \c argp_parse semantics.
 */
error_t mpi_argp_parse(const int rank,
                       const struct argp *argp,
                       int argc,
                       char **argv,
                       unsigned flags,
                       int *arg_index,
                       void *input)
{
    // Flush stdout, stderr
    if (fflush(stdout))
        perror("mpi_argp_parse error flushing stdout prior to redirect");
    if (fflush(stderr))
        perror("mpi_argp_parse error flushing stderr prior to redirect");

    // Save stdout, stderr so we may restore them later
    int stdout_copy, stderr_copy;
    if ((stdout_copy = dup(fileno(stdout))) < 0)
        perror("mpi_argp_parse error duplicating stdout");
    if ((stderr_copy = dup(fileno(stderr))) < 0)
        perror("mpi_argp_parse error duplicating stderr");

    // On non-root processes redirect stdout, stderr to /dev/null
    if (rank) {
        if (!freopen("/dev/null", "a", stdout))
            perror("mpi_argp_parse error redirecting stdout");
        if (!freopen("/dev/null", "a", stderr))
            perror("mpi_argp_parse error redirecting stderr");
    }

    // Invoke argp per http://www.gnu.org/s/libc/manual/html_node/Argp.html
    error_t retval = argp_parse(argp, argc, argv, flags, arg_index, input);

    // Flush stdout, stderr again
    if (fflush(stdout))
        perror("mpi_argp_parse error flushing stdout after redirect");
    if (fflush(stderr))
        perror("mpi_argp_parse error flushing stderr after redirect");

    // Restore stdout, stderr
    if (dup2(stdout_copy, fileno(stdout)) < 0)
        perror("mpi_argp_parse error reopening stdout");
    if (dup2(stderr_copy, fileno(stderr)) < 0)
        perror("mpi_argp_parse error reopening stderr");

    // Close saved versions of stdout, stderr
    if (close(stdout_copy))
        perror("mpi_argp_parse error closing stdout_copy");
    if (close(stderr_copy))
        perror("mpi_argp_parse error closing stderr_copy");

    // Clear any errors that may have occurred on stdout, stderr
    clearerr(stdout);
    clearerr(stderr);

    // Return what argp_parse returned
    return retval;
}
