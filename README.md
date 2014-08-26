process-listlike
================

Process library for types other than String - fork of process-extras

This library uses the ListLike class to provide the functions from process
for ByteStrings and Text.  In addition to the readProcess and readProcessWithExitCode
functions patterned after those in process, it provides readCreateProcess* functions
which take a CreateProcess record instead of a command name and argument list.  This
allows you to modify the CreateProcess before starting the process - for example, to
set the working directory.

Another (forthcoming) addition is readInterleaved, which reads and interleaves the
chunks of output from several handles (e.g. stdout and stderr.)  This allows you to
preserve the order in which a process produces output.
