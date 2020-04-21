# Profiling Applications on M2

## Center for Scientific Computation (CSC)

* Maintains our primary shared resource for research computing, ManeFrame II (M2),
  in collaboration with OIT
* Provides research computing tools, support, and training to all faculty, staff,
  and students using research computing resources
  [www.smu.edu/csc](https://www.smu.edu/csc) has documentation and news
* [help@smu.edu](mailto:help@smu.edu) or
  [rkalescky@smu.edu](mailto:rkalescky@smu.edu) for help

## CSC Workshop Series

|Date         |Workshop                                                     |
|-------------|-------------------------------------------------------------|
|January 21   |M2 Introduction                                              |
|January 28   |Introduction to LAPACK and BLAS                              |
|February 4   |Text Mining with Python on M2 (Lead by Dr. Eric Godat)       |
|February 11  |Using the New HPC Portal                                     |
|February 18  |Using GitHub                                                 |
|February 25  |Writing Portable Accelerator Code with KOKKOS, RAJA, and OCCA|
|March 3      |M2 Introduction                                              |
|March 10     |Introduction to Parallelization Using MPI                    |
|March 17     |No Workshop Spring Break                                     |
|March 24     |Writing High Performance Python Code                         |
|March 31     |Creating Portable Environments with Docker and Singularity   |
|April 7      |M2 Introduction                                              |
|April 14     |Introduction to Parallelization Using OpenMP and OpenACC     |
|April 21     |Profiling Applications on M2                                 |
|April 28     |Improving Code Vectorization                                 |

## Accessing ManeFrame II (M2) for this Workshop

* Via Terminal or Putty as usual (see [here](http://faculty.smu.edu/csc/documentation/access.html) for details)
* Via the HPC Portal (Note that this doesn't support X11 forwarding)
    1. Go to [hpc.smu.edu](https://hpc.smu.edu/).
    2. Sign in using your SMU ID and SMU password.
    3. Select "ManeFrame II Shell Access" from the "Clusters" drop-down menu.

## Profiling and Performance Analysis with GCC

There are two primary mechanisms for profiling code: determining which
routines take the most time, and determining which specific lines of
code would be best to optimize. Thankfully, the [GNU compiler
collection](http://gcc.gnu.org/) includes utilities for both of these
tasks, as will be illustrated below. Utilities with similar
functionality are included with some other compilers.

### Generating a profile

In the GNU compilers (and many others), you can enable profiling
information through adding in the `-p` compiler flag. Add this compiler
flag to the commands in the `CMakeCache.txt` for the target `mmm`

Profiling information is generated by running the executable once to
completion. 

```
$ module load spack gcc-9.2 armadillo cmake
$ cmake .
$ cmake --build .
$ srun -p development,htc,standard-mem-s -c 1 --mem=6G -t 5 ./mmm 2000 2000 2000
```

Write down the total runtime required for the program (you will use this
information later on).

When the program has finished, you should see a new file in the
directory called `gmon.out`. This contains the relevant profiling data,
and was written during the execution of the code.

Examine the profiling information by using the program `gprof`. You use
this by calling `gprof`, followed by the executable name. It will
automatically look in the `gmon.out` file in that directory for the
profiling data that relates to the executable. Run the command

```
$ gprof mmm
```

When you run `gprof`, it outputs all of the profiling information to the
screen. To enable easier examination of these results, you should
instead send this data to a file. You can redirect this information to
the file `profiling_data.txt` with the command

```
$ gprof mmm > profiling_data.txt
```

You will then have the readable file `profiling_data.txt` with the
relevant profiling information.

### Identifying bottlenecks

Read through the first table of profiling information in this file. The
first column of this table shows the percentage of time spent in each
function called by the executable. Identify which one takes the vast
majority of the time. This bottleneck should be the first routine that
you investigate for optimization.

Look through the routine identified from the previous step, the
function may be contained in a file with a different name, so you can
use `grep` to find which file contains the routine:

```
$ grep -i <routine_name> *
```

where `<routine_name>` is the function that you identified from the
previous step.

Once you have determined the file that contains the culprit function,
you can use the second utility routine `gcov` to determine which lines
in the file are executed the most. To use `gcov`, you must modify the
compile line once more, to use the compilation flags
`-fprofile-arcs -ftest-coverage`.

Add these compiler flags to the commands in the `CMakeLists.txt` for the
target `mmm`, recompile, and re-run the executable,

```
$ srun -p development,htc,standard-mem-s -c 1 --mem=6G -t 5 ./mmm 2000 2000 2000
$ mv ./CMakeFiles/mmm.dir/mmm.cpp.gcno mmm.gcno
$ mv ./CMakeFiles/mmm.dir/mmm.cpp.gcda mmm.gcda
```

You should now see additional files in the directory with extentions
`.gcda` and `.gcno`. If you do not see these files, revisit the above
instructions to ensure that you haven't missed any steps.

You should now run `gcov` on the input file that held the function you
identified from the steps above. For example, if the source code file
was `file.cpp`, you would run

```
$ gcov mmm.cpp
```

This will output some information to the screen, including the name of a
`.gcov` file that it creates with information on the program. Open this
new file using `nano`, and you will see lines like the following:

```
      2001:   10:    for (unsigned long int k = 0; k < p; ++k) {
   4002000:   11:        for (unsigned long int j = 0; j < n; ++j) {
8004000000:   12:            for (unsigned long int i = 0; i < m; ++i) {
8000000000:   13:                C.at(i, k) += A.at(i, j) * B.at(j, k);
```

The first column of numbers on the left signify the number of times each
line of code was executed within the program. The second column of
numbers correspond to the line number within the source code file. The
remainder of each line shows the source code itself. From the above
snippet, we see that lines 54 and 55 were executed 1.01 and 1 million
times, respectively, indicating that these would be prime locations for
code optimization.

Find the corresponding lines of code in the function that you identified
from the preceding step. It is here where you should focus your
optimization efforts.

### Optimizing code

Save a copy of the source code file you plan to modify using the `cp`
command, e.g.

```
$ cp file.cpp file_old.cpp
```

where `file` is the file that you have identified as containing the
bottleneck routine (use the appropriate extension for your coding
language). We will use this original file again later in the session.

Now that you know which lines are executed, and how often, you should
remove the `gcov` compiler options, but keep the `-p` in your
`CMakeLists.txt`.

Determine what, if anything, can be optimized in this routine. The topic
of code optimization is bigger than we can cover in a single workshop
session, but here are some standard techniques.

#### Code optimization techniques

1.  Is there a simpler way that the arithmetic could be accomplished?
    Sometimes the most natural way of writing down a problem does not
    result in the least amount of effort. For example, we may implement
    a line of code to evaluate the polynomial $p(x) =
    2x^4-3x^3+5x^2-8x+7$ using either

    ```
    p = 2.0*x*x*x*x - 3.0*x*x*x + 5.0*x*x - 8*x + 7.0;
    ```

    or

    ```
    p = (((2.0*x - 3.0)*x + 5.0)*x - 8.0)*x + 7.0;
    ```

    The first line requires 10 multiplication and 4 addition/subtraction
    operations, while the second requires only 4 multiplications and 4
    additions/subtractions.

2.  Is the code accessing memory in an optimal manner? Computers store
    and access memory from RAM one \"page\" at a time, meaning that if
    you retrieve a single number, the numbers nearby that value are also
    stored in fast-access cache memory. So, if each iteration of a loop
    uses values that are stored in disparate portions of RAM, each value
    could require retrieval of a separate page. Alternatively, if each
    loop iteration uses values from memory that are stored nearby one
    another, many numbers in a row can be retrieved using a single RAM
    access. Since RAM access speeds are significantly slower than cache
    access speeds, something as small as a difference in loop ordering
    can make a huge difference in speed.

3.  Is the code doing redundant computations? While modern computers can
    perform many calculations in the time it takes to access one page of
    RAM, some calculations are costly enough to warrant computing it
    only once and storing the result for later reuse. This is especially
    pertinent for things that are performed a large number of times. For
    example, consider the following two algorithms:

    ```
    for (i=1; i<10000; i++) {
    d[i] = u[i-1]/h/h - 2.0*u[i]/h/h + u[i+1]/h/h;
    } 
    ```

    and

    ```
    double hinv2 = 1.0/h/h;
    for (i=1; i<10000; i++) {
    d[i] = (u[i-1] - 2.0*u[i] + u[i+1])*hinv2;
    }
    ```

    Since floating-point division is significantly more costly than
    multiplication (roughly $10\times$), and the division by $h^2$ is
    done redundantly both within and between loop iterations, the second
    of these algorithms is typically much faster than the first.

4.  Is the code doing unnecessary data copies? In many programming
    languages, a function can be written to use either *call-by-value*
    or *call-by-reference*.

    In call-by-value, all arguments to a function are copied from the
    calling routine into a new set of variables that are local to the
    called function. This allows the called function to modify the input
    variables without concern about corrupting data in the calling
    routine.

    In call-by-reference, the called function only receives memory
    references to the actual data held by the calling routine. This
    allows the called function to directly modify the data held by the
    calling routine.

    While call-by-reference is obviously more \"dangerous,\" it avoids
    unnecessary (and costly) memory allocation/copying/deallocation in
    the executing code. As such, highly efficient code typically uses
    call-by-reference, with the programmer responsible for ensuring that
    data requiring protection in the calling program is manually copied
    before function calls, or that the functions themselves are
    constructed to avoid modifying the underlying data.

    In C and C++, call-by-value is the default, whereas Fortran uses
    call-by-reference. However in C, pointers may be passed through
    function calls to emulate call-by-reference. In C++, either pointers
    can be sent through function calls, or arguments may be specified as
    being passed by reference (using the `&` symbol).

Find what you can fix, so long as you do not change the mathematical
result. Delete and re-compile the executable,

Re-examine the results using `gprof`, and repeat the optimization
process until you are certain that the code has been sufficiently
optimized. You should be able to achieve a significant performance
improvement (at least 40% faster than the original).

Write down the total runtime required for your hand-optimized program.
Copy your updated code to the file `file_new.cpp` (again, use the
appropriate extension for your coding language).

## Profiling Python Scripts

Like GCC and Python, R has the ability to profile at the line level.

```
module purge
module load python
srun -p development,htc,standard-mem-s -c 1 --mem=6G -t 5 python3 mmm.py
```

This demonstrates the performance difference between an optimized matrix-matrix
implementation and a simple implementation.

```
srun -p development,htc,standard-mem-s -c 1 --mem=6G -t 5 python3 -m cProfile -o profile_data.txt mmm.py
python3 view_profile.py | grep gemm
```

The `view_profile.py` script extracts line-level profile information from the
`profile_data.txt` file.

## Profiling R Scripts

Like GCC and Python, R has the ability to profile at the function and line levels.

```
module purge
$ module load r/3.6.2
$ srun -p development,htc,standard-mem-s -c 1 --mem=6G -t 5 Rscript mmm.R 1
```

This demonstrates the performance difference between an optimized matrix-matrix
implementation and a simple implementation.

```
$ srun -p development,htc,standard-mem-s -c 1 --mem=6G -t 5 Rscript profile.R
```

The `profile.R` script runs the profiler twice, once for function-level
information and again for line-level information.

